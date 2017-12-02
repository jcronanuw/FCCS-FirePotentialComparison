#-------------------------------------------------------------------------------
# Name:        report_loadings.py
# Purpose:     FCCS Potentials report.
#              Use reportlab to generate pdf reports from the calculated data (in .csv files)
#              - http://www.reportlab.com/software/opensource/
#
# Author:      kjells
# Created:     12/9/2013
#-------------------------------------------------------------------------------
#!/usr/bin/env python

import sys
import os
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

GROWTH_FILE = 'growth_out.csv'
CO_THRESHOLD = 0.01

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
    styles.add(ParagraphStyle(name='Left', alignment=TA_LEFT))

    # get environmental scenario data
    # Column headers for ev data
    #    BUName,EVName,UseMetric,Wind,Slope,FM1,FM10,FM100,FM1000,FMDuff,FMHerb,FMShrub,ConCanopy,ConShrub,ConPiles
    ev = ev_data.ev_data()
    bu_name = 'None' if not ev.valid else ev.get_parameter_value('BUName')
    use_metric = ev.get_parameter_value('UseMetric')

    # Title
    ptext = '<font size={}>FEPS 2.0 Hourly Emissions Report</font>'.format(const.FONTSIZE_TITLE)
    elements.append(Paragraph(ptext, styles["Center"]))

    # Table for date, bu name, and ev name
    cols = [100,200]

    header_data = []
    today = datetime.date.today()
    header_data.append(['Report date:', '{}'.format(today.strftime('%B %d, %Y'))])
    header_data.append(['Unit Name:', '{}'.format(bu_name)])
    units = 'mg/hour' if use_metric else 'tons/hour'
    header_data.append(['Units of measure:', '{}'.format(units)])

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
    elements.append(Spacer(1, 6))
    ptext = '<font size=8>Hourly emissions of carbon dioxide (CO2), carbon monoxide (CO), methane (CH4), &lt; 2.5 micron particulate matter, and &lt; 10 micron particulate matter</font>'
    elements.append(Paragraph(ptext, styles['Left']))
    elements.append(Spacer(1, 12))
    return use_metric

def get_content(datafile, use_metric):
    ''' These are the columns required for this report. Retrieve them from the .csv
    file and order them appropriatly
    Hour,Fire Size,CO2,CO,CH4,PM2.5,PM10
    '''
    df_growth = pd.read_csv(os.path.join(os.path.split(datafile)[0], GROWTH_FILE))
    fire_hours = df_growth.index[-1]

    df = pd.read_csv(datafile)
    
    # truncate output when CO goes below threshold. In rare cases we are below the threshold right at the outset. 
    #  Ensure that we show the number of hours of the fire at minimum
    try:
        idx_last = df.loc[df['CO'] >= CO_THRESHOLD].last_valid_index() + 1
    except:
        idx_last = 0
        
    if fire_hours > idx_last:
        idx_last = fire_hours
        
    df = df.ix[0:idx_last]
    df = df.fillna(0.0)

    df_growth = df_growth.reindex(df.index, method='pad')	# extend to the length of the output file
    
    fire_size = 'Cumulative {}'.format('Hectares' if use_metric else 'Acres')

    sub_df = pd.DataFrame(
        {
            'Hour': [int(i) for i in df.index.values],
            fire_size: df_growth.get('size').round(2),
            'CO2': df.CO2.round(2),
            'CO': df.CO.round(2),
            'CH4': df.CH4.round(2),
            'PM2.5': df.PM25.round(2),
            'PM10': df.PM10.round(2),
        }
    )
    
    cols = ['Hour',
        fire_size,
        'CO2',
        'CO',
        'CH4',
        'PM2.5',
        'PM10' ]
    
    sub_df = sub_df.reindex_axis(cols, axis=1)
    sub_df[['Hour']] = sub_df[['Hour']].astype(str)
    return sub_df

def assemble_document(datafile, outfile):
    # create the document
    doc = create_document(output_filename)

    # "Flowable" objects container
    elements = []

    # header content
    use_metric = add_header_content(elements)

    # retrieve data and set up column spacing
    contents = get_content(datafile, use_metric)
    cols = [
        60,
        120,
        60,
        60,
        60,
        60,
        60]

    # Assemble data for each column using simple loop to append it into data list
    data = []
    data.append([i.split('~')[0] if '~' in i else i for i in contents.columns.tolist()])

    # add the actual data
    for i in contents.index:
	# NOTE: the values invocation will convert int to float. strings are unaffected
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
if len(sys.argv) >= 3:
    inputfile = sys.argv[1]
    output_filename = sys.argv[2]
    assemble_document(inputfile, output_filename)
else:
    print("\nError: the input file and the output filename are required.\n")








