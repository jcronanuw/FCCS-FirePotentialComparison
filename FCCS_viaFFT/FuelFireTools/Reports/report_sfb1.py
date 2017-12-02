#-------------------------------------------------------------------------------
# Name:        report_sfb1.py
# Purpose:     Surface Fire Behavior report.
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


def map_to_fm40(x):
    FM40_MAP = {
        101: 'GR1',
        102: 'GR2',
        103: 'GR3',
        104: 'GR4',
        105: 'GR5',
        106: 'GR6',
        107: 'GR7',
        108: 'GR8',
        109: 'GR9',
        121: 'GS1',
        122: 'GS2',
        123: 'GS3',
        124: 'GS4',
        141: 'SH1',
        142: 'SH2',
        143: 'SH3',
        144: 'SH4',
        145: 'SH5',
        146: 'SH6',
        147: 'SH7',
        148: 'SH8',
        149: 'SH9',
        161: 'TU1',
        162: 'TU2',
        163: 'TU3',
        164: 'TU4',
        165: 'TU5',
        181: 'TL1',
        182: 'TL2',
        183: 'TL3',
        184: 'TL4',
        185: 'TL5',
        186: 'TL6',
        187: 'TL7',
        188: 'TL8',
        189: 'TL9',
        201: 'SB1',
        202: 'SB2',
        203: 'SB3',
        204: 'SB4' }
    ix = int(x)
    return FM40_MAP[ix] if ix in FM40_MAP else ix

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
    ev_name = 'None' if not ev.valid else ev.get_parameter_value('EVName')
    use_metric = ev.get_parameter_value('UseMetric')

    # Title
    ptext = '<font size={}>FCCS 3.0 Surface Fire Behavior Report</font>'.format(const.FONTSIZE_TITLE)
    elements.append(Paragraph(ptext, styles["Center"]))

    # Table for date, bu name, and ev name
    cols = [70,200]

    header_data = []
    today = datetime.date.today()
    header_data.append(['Report date:', '{}'.format(today.strftime('%B %d, %Y'))])
    header_data.append(['Unit Name:', '{}'.format(bu_name)])
    header_data.append(['EV Name:', '{}'.format(ev_name)])

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
    explanatory_text = '<font size=8>The surface fire behavior report includes predicted surface fire rate of spread (ROS), flame length (FL), and total reaction intensity (RI) and reaction intensity by surface fuel stratum including shrubs, herb, wood, and litter-lichen-moss (LLM).'
    elements.append(Paragraph(explanatory_text, styles["Left"]))

    elements.append(Spacer(1, 6))
    ptext = '<font size=8>Parameters:\tWind ({}{})  Slope ({}%)  1hrFM ({}%)  10hrFM ({}%)  100hrFM ({}%)  ShrubFM ({}%)  HerbFM ({}%)</font>'.format(
        ev.get_parameter_value('Wind'),
        'kph' if use_metric else "mph",
        ev.get_parameter_value('Slope'),
        ev.get_parameter_value('FM1'),
        ev.get_parameter_value('FM10'),
        ev.get_parameter_value('FM100'),
        ev.get_parameter_value('FMShrub'),
        ev.get_parameter_value('FMHerb')
    )
    elements.append(Paragraph(ptext, styles["Normal"]))
    elements.append(Spacer(1, 6))

def get_content(datafile):
    ''' These are the columns required for this report. Retrieve them from the .csv
    file and order them appropriatly
    '''
    df = pan.read_csv(datafile)
    df = df.fillna(0)
    sub_df = pan.DataFrame(
        {
            'Filename': df.Filename.map(lambda x: (str(x).split('.')[0])),
            'ID': df.Fuelbed_number,
            'ROS': df.Custom_ROS.round(1),
            'FL': df.Custom_FL.round(1),
            'RI total': df.Custom_RI.map(lambda x: (int(x))),
            'RI shrub': df.Custom_RI_Shrub.map(lambda x: (int(x))),
            'RI herb': df.Custom_RI_Herb.map(lambda x: (int(x))),
            'RI wood': df.Custom_RI_Wood.map(lambda x: (int(x))),
            'RI llm': df.Custom_RI_LLM.map(lambda x: (int(x))),
            'Crosswalk': df.Crosswalk13.map(lambda x: (int(x))),
            '% ROS': df.get('ROS%13').map(lambda x: (int(x))),
            '% FL': df.get('FL%13').map(lambda x: (int(x))),
            'Crosswalk~40': df.Crosswalk40.map(map_to_fm40),
            '% ROS~40': df.get('ROS%40').map(lambda x: (int(x))),
            '% FL~40': df.get('FL%40').map(lambda x: (int(x)))
        }
    )
    cols = ['Filename',
            'ID',
            'ROS',
            'FL',
            'RI total',
            'RI shrub',
            'RI herb',
            'RI wood',
            'RI llm',
            'Crosswalk',
            '% ROS',
            '% FL',
            'Crosswalk~40',
            '% ROS~40',
            '% FL~40'
            ]
    sub_df = sub_df.reindex_axis(cols, axis=1)
    return sub_df

def assemble_document(datafile, outfile, do_metric):
    # create the document
    doc = create_document(output_filename)

    # "Flowable" objects container
    elements = []

    # header content
    add_header_content(elements)

    # retrieve data and set up column spacing
    contents = get_content(datafile)
    cols = [90,35,35,35,50,50,50,50,50,50,50,50,50,50,50]

    # Assemble data for each column using simple loop to append it into data list
    data = []
    data.append([i.split('~')[0] if '~' in i else i for i in contents.columns.tolist()])

    # add spanning elements
    spanners = ['','','{}'.format('m/min' if do_metric else 'ft/min'),'{}'.format('m' if do_metric else 'ft'),'|---------------                         {}                    ---------------|'.format('kw/m2' if do_metric else 'BTU/ft2/min'),'','','','','|-------------   Fuel Model 13   -------------|','','','|-------------   Fuel Model 40   -------------|','','']
    data.append(spanners)

    # add the actual data
    for i in contents.index:
        data.append(list(contents.irow(i).values))

    # add the table style -- this applies formatting to the table
    table_that_splits_over_pages= Table(data, colWidths=cols, repeatRows=2)
    table_that_splits_over_pages.hAlign = 'LEFT'
    tbl_style = TableStyle([('TEXTCOLOR',(0,0),(-1,-1),colors.black),
                           ('VALIGN',(0,0),(-1,-1),'TOP'),
                           ('ALIGN',(0,0),(-1,1), 'CENTER'),
                           ('ALIGN',(0,2),(-1,-1), 'RIGHT'),
                           ('LEADING',(0,0),(-1,0), 0),
                           ('BOTTOMPADDING',(0,0),(-1,0), 0),
                           ('TOPPADDING',(0,1),(-1,1), 0),
                           ('FONTSIZE',(0,1),(-1,-1),8),
                           ('INNERGRID', (0,2), (-1,-1), 0.5, colors.black),
                           ('SPAN',(4,1),(8,1)),
                           ('SPAN',(9,1),(11,1)),
                           ('SPAN',(12,1),(-1,1)),
                           ('BOX',(0,0),(-1,1),1,colors.black),
                           ('BOX',(0,2),(-1,-1),1,colors.black)])

    tbl_style.add('BACKGROUND',(0,0),(-1,1),colors.lightblue)
    tbl_style.add('BACKGROUND',(0,2),(-1,-1),colors.white)
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
    do_metric = True if 'do_metric' in sys.argv else False
    assemble_document(inputfile, output_filename, do_metric)
else:
    print("\nError: the input file and the output filename are required.\n")








