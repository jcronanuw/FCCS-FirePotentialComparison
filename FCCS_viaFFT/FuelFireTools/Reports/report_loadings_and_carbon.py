#-------------------------------------------------------------------------------
# Name:        report_loadings.py
# Purpose:     FCCS Loadings report.
#              Use reportlab to generate pdf reports from the calculated data (in .csv files)
#              - http://www.reportlab.com/software/opensource/
#
# Author:      kjells
# Created:     10/2/2013
#-------------------------------------------------------------------------------
#!/usr/bin/env python

import sys
import datetime
import pandas as pan
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

CARBON_MULTIPLIER_LOW = 0.4
CARBON_MULTIPLIER_HIGH = 0.5
LOADINGS_MULTIPLIER = 1.0

def create_document(name):
    # basic layout, landscape/protrait and margins
    pdfReportPages = name
    doc = SimpleDocTemplate(pdfReportPages, pagesize=landscape(letter),
        topMargin=const.MARGIN_TOP, bottomMargin=const.MARGIN_BOTTOM,
        leftMargin=const.MARGIN_LEFT, rightMargin=const.MARGIN_RIGHT)
    return doc

def add_header_content(elements, do_carbon=False):
    styles=getSampleStyleSheet()
    styles.add(ParagraphStyle(name='Center', alignment=TA_CENTER))

    # get environmental scenario data
    # Column headers for ev data
    #    BUName,EVName,UseMetric,Wind,Slope,FM1,FM10,FM100,FM1000,FMDuff,FMHerb,FMShrub,ConCanopy,ConShrub,ConPiles
    ev = ev_data.ev_data()
    bu_name = 'None' if not ev.valid else ev.get_parameter_value('BUName')
    use_metric = ev.get_parameter_value('UseMetric')

    # Title
    ptext = ''
    if do_carbon:
        ptext = '<font size={}> FCCS 3.0 Carbon Report</font>'.format(const.FONTSIZE_TITLE)
    else:
        ptext = '<font size={}>FCCS 3.0 Total Aboveground Biomass Report</font> (boles included)'.format(const.FONTSIZE_TITLE)
    elements.append(Paragraph(ptext, styles["Center"]))

    # Table for date, bu name, and ev name
    cols = [100,200]

    header_data = []
    today = datetime.date.today()
    header_data.append(['Report date:', '{}'.format(today.strftime('%B %d, %Y'))])
    header_data.append(['Unit Name:', '{}'.format(bu_name)])
    header_data.append(['Units of measure:', '{}'.format('Mg/ha' if use_metric else "tons/acre")])

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
    
def get_content(datafile, do_carbon=False):
    ''' These are the columns required for this report. Retrieve them from the .csv
    file and order them appropriatly
    '''
    mult_low = LOADINGS_MULTIPLIER if not do_carbon else CARBON_MULTIPLIER_LOW
    mult_high = LOADINGS_MULTIPLIER if not do_carbon else CARBON_MULTIPLIER_HIGH
        
    df = pan.read_csv(datafile)
    df = df.fillna(0.0)
    sub_df = pan.DataFrame(
        {
            'Filename': df.Filename.map(lambda x: (str(x).split('.')[0])),
            'ID': df.Fuelbed_number,
            'Total fuelbed': ((df.Total_aboveground_biomass)*mult_high).round(2),
            'Canopy': (((df.Tree_aboveground_load  +
                        df.Snag_class1_foliage_load +
                        df.Snag_class1_wood_load +
                        df.Snag_class1_other_load +
                        df.Ladderfuels_load)*mult_high) +
                        ((df.Snag_class2_load + df.Snag_class3_load)*mult_low)).round(2),
            'Shrub': ((df.Shrub_primary_load + df.Shrub_secondary_load)*mult_high).round(2),
            'Herb': ((df.Herb_primary_load + df.Herb_secondary_load)*mult_high).round(2),
            'Wood':   (((df.Woody_sound_1hr_load +
                        df.Woody_sound_10hr_load +
                        df.Woody_sound_100hr_load +
                        df.Woody_sound_1000hr_load +
                        df.Woody_sound_10khr_load +
                        df.Woody_sound_GT10k_load +
                        df.Woody_pile_load +
                        df.Woody_stumps_sound_load +
                        df.Woody_stumps_rotten_load +
                        df.Woody_stumps_lightered_load)*mult_high) +
                        ((df.Woody_rotten_1000hr_load +
                        df.Woody_rotten_10k_load + df.Woody_rotten_GT10k_load)*mult_low)).round(2),
            'LLM': ((df.LLM_litter_load + df.LLM_lichen_load + df.LLM_moss_load)*mult_high).round(2),
            'Ground fuels': (((df.Ground_upperduff_load + df.Ground_lowerduff_load)*mult_low) +
                ((df.Ground_basalaccum_load + df.Ground_squirrelmid_load)*mult_high)).round(2)
        }    )
    cols = ['Filename',
            'ID',
            'Total fuelbed',
            'Canopy',
            'Shrub',
            'Herb',
            'Wood',
            'LLM',
            'Ground fuels'
            ]
    sub_df = sub_df.reindex_axis(cols, axis=1)
    return sub_df

def assemble_document(datafile, outfile, do_carbon=False):
    # create the document
    doc = create_document(output_filename)

    # "Flowable" objects container
    elements = []

    # header content
    add_header_content(elements, do_carbon)

    # retrieve data and set up column spacing
    contents = get_content(datafile, do_carbon)
    cols = [
        120,
        35,
        80,
        80,
        80,
        80,
        80,
        80,
        80
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
if len(sys.argv) >= 3:
    inputfile = sys.argv[1]
    output_filename = sys.argv[2]
    do_carbon = True if (4 == len(sys.argv) and 'carbon' == sys.argv[3]) else False
    assemble_document(inputfile, output_filename, do_carbon=do_carbon)
else:
    print("\nError: the input file and the output filename are required.\n")








