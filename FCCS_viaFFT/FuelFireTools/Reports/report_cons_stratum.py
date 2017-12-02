#-------------------------------------------------------------------------------
# Name:        report_cons_stratum.py
# Purpose:     Consume consumption by stratum report.
#              Use reportlab to generate pdf reports from the calculated data (in .csv files)
#              - http://www.reportlab.com/software/opensource/
#
# Author:      kjells
# Created:     10/2/2013
#-------------------------------------------------------------------------------
#!/usr/bin/env python

import sys
import datetime
import pandas as pd
import numpy as np
from reportlab.platypus import SimpleDocTemplate, Table, TableStyle, Paragraph, Frame, Spacer
from reportlab.lib import colors
from reportlab.lib.units import cm
from reportlab.lib.pagesizes import letter, landscape
from reportlab.lib.styles import ParagraphStyle, getSampleStyleSheet
from reportlab.lib.enums import TA_LEFT, TA_RIGHT, TA_CENTER, TA_JUSTIFY
import num_canvas
import ev_data
import constants as const

def create_document(name):
    # basic layout, landscape/protrait and margins
    pdfReportPages = name
    doc = SimpleDocTemplate(pdfReportPages, pagesize=landscape(letter),
        topMargin=const.MARGIN_TOP, bottomMargin=const.MARGIN_BOTTOM,
        leftMargin=const.MARGIN_LEFT, rightMargin=const.MARGIN_RIGHT)
    return doc

def add_header_content(elements):
    styles=getSampleStyleSheet()
    styles.add(ParagraphStyle(name='Center', alignment=TA_CENTER))

    # get environmental scenario data
    # Column headers for ev data
    #    BUName,EVName,UseMetric,Wind,Slope,FM1,FM10,FM100,FM1000,FMDuff,FMHerb,FMShrub,ConCanopy,ConShrub,ConPiles
    ev = ev_data.ev_data()
    bu_name = 'None' if not ev.valid else ev.get_parameter_value('BUName')
    use_metric = ev.get_parameter_value('UseMetric')
    unitsize = ev.get_parameter_value('UnitSize')
    permit = ev.get_parameter_value('Permit')
    fire_type = ev.get_parameter_value('Type')

    # Title
    ptext = '<font size={}>Consume 4.2 Consumption by Stratum Report</font>'.format(const.FONTSIZE_TITLE)
    elements.append(Paragraph(ptext, styles["Center"]))

    # Table for date, bu name, and ev name
    cols = [100,200]

    header_data = []
    today = datetime.date.today()
    header_data.append(['Report date:', '{}'.format(today.strftime('%B %d, %Y'))])
    header_data.append(['Unit Name:', '{}'.format(bu_name)])
    header_data.append(['Units of measure:', '{}'.format('Mg/ha' if use_metric else "tons/acre")])
    header_data.append(['Unit size:', '{}'.format(unitsize)])
    header_data.append(['Fire type:', '{}'.format("" if 0.0 == fire_type else fire_type)])
    header_data.append(['Permit#:', '{}'.format("" if 0.0 == permit else permit)])

    header_table = Table(header_data, colWidths=cols)
    header_table.hAlign = 'LEFT'
    tbl_style = TableStyle([('TEXTCOLOR',(0,0),(-1,-1),colors.black),
                           ('VALIGN',(0,0),(-1,-1),'TOP'),
                           ('ALIGN',(0,0),(-1,1), 'LEFT'),
                           ('LEFTPADDING',(0,0),(-1,-1), 0),
                           ('LEADING',(0,0),(-1,-1), 4),
                           ('FONTSIZE',(0,0),(-1,-1),10)])
    header_table.setStyle(tbl_style)

    elements.append(Spacer(1, 6))
    elements.append(header_table)
    elements.append(Spacer(1, 12))
    
def get_consume_loadings():
    '''
    This is to get the preburn loadings. Consume reads this information but doesn't write it out.
    Adding this to Consume breaks all the regression tests. Consider for the future.
    '''
    LOADINGS_FILE = './00_Results/fccs/consume_loadings.csv'
    return pd.read_csv(LOADINGS_FILE, header=1)

def get_content(datafile):
    ''' These are the columns required for this report. Retrieve them from the .csv
    file and order them appropriatly
    Filename	ID	Total fuelbed	Canopy	Shrub	Herb 	Wood	LLM 	Ground fuels

    '''
    df = pd.read_csv(datafile)
    df = df.fillna(0.0)
    
    df_loadings = get_consume_loadings()
    
    sub_df = pd.DataFrame(
        {
            'Fuelbed': df.Fuelbeds,
            'Filename': df.Filename,
            'Preburn Loading': (df_loadings.get('Total_available_fuel_loading')).round(2),
            'Total': (df.get('Total Consumption')).round(2),
            'Canopy': (df.get('Canopy Consumption')).round(2),
            'Shrub': (df.get('Shrub Consumption')).round(2),
            'Herb': (df.get('Herb Consumption')).round(2),
            'Wood': (df.get('Wood Consumption')).round(2),
            'LLM': (df.get('LLM Consumption')).round(2),
            'Ground': (df.get('Ground Consumption')).round(2)
        }
    )
    cols = ['Fuelbed',
            'Filename',
            'Preburn Loading',
            'Total',
            'Canopy',
            'Shrub',
            'Herb',
            'Wood',
            'LLM',
            'Ground'
            ]
    sub_df = sub_df.reindex_axis(cols, axis=1)
    return sub_df

def assemble_document(datafile, outfile):
    # create the document
    doc = create_document(output_filename)

    # "Flowable" objects container
    elements = []

    # header content
    add_header_content(elements)

    # retrieve data and set up column spacing
    contents = get_content(datafile)
    cols = [
        45,
        110,
        90,
        65,
        65,
        65,
        65,
        65,
        65,
        65
        ]

    # Assemble data for each column using simple loop to append it into data list
    data = []
    data.append([i.split('~')[0] if '~' in i else i for i in contents.columns.tolist()])

    # add the actual data
    for i in contents.index:
        data.append(list(contents.irow(i).values))

    # add the table style -- this applies formatting to the table
    table_that_splits_over_pages= Table(data, colWidths=cols, repeatRows=1)
    table_that_splits_over_pages.hAlign = 'LEFT'
    tbl_style = TableStyle([('TEXTCOLOR',(0,0),(-1,-1),colors.black),
                           ('VALIGN',(0,0),(-1,-1),'TOP'),
                           ('ALIGN',(0,0),(0,0), 'CENTER'),
                           ('FONTSIZE',(0,1),(-1,-1),8),
                           ('INNERGRID', (0,1), (-1,-1), 0.5, colors.black),
                           ('BOX',(0,0),(-1,1),1,colors.black),
                           ('BOX',(0,1),(-1,-1),1,colors.black)])
    tbl_style.add('BACKGROUND',(0,0),(-1,1),colors.lightblue)
    tbl_style.add('BACKGROUND',(0,1),(-1,-1),colors.white)
    
    table_that_splits_over_pages.setStyle(tbl_style)
    elements.append(table_that_splits_over_pages)

    # - put it all together
    doc.build(elements, canvasmaker=num_canvas.NumberedCanvas)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Start
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if 3 == len(sys.argv):
    inputfile = sys.argv[1]
    output_filename = sys.argv[2]
    assemble_document(inputfile, output_filename)
else:
    print("\nError: the input file and the output filename are required.\n")








