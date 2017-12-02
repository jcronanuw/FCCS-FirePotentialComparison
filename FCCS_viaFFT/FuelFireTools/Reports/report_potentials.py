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
    ptext = '<font size={}>FCCS 3.0 Potentials Report</font>'.format(const.FONTSIZE_TITLE)
    elements.append(Paragraph(ptext, styles["Center"]))

    # Table for date, bu name, and ev name
    cols = [100,200]

    header_data = []
    today = datetime.date.today()
    header_data.append(['Report date:', '{}'.format(today.strftime('%B %d, %Y'))])
    header_data.append(['Unit Name:', '{}'.format(bu_name)])
    header_data.append(['Units of measure:', '{}'.format('Index (0-9)')])

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
    ptext = '<font size=8>The potentials report includes FCCS fire potentials including Surface Fire Behavior Summary Potential (SFB), Reaction Potential (Reaction), Spread Potential (Spread), Flame Length Potential (Flame), Crown Fire Behavior Summary Potential (CFP), Crown Fire Initiation Potential (Initiation), Crown-to-Crown Transmissivity Potential (Transmissivity), Crown Fire Spreading Potential (Spread), and Available Fuel Summary Potential</font>'
    elements.append(Paragraph(ptext, styles['Left']))
    elements.append(Spacer(1, 12))

def get_content(datafile):
    ''' These are the columns required for this report. Retrieve them from the .csv
    file and order them appropriatly
    Filename	ID	Total fuelbed	Canopy	Shrub	Herb 	Wood	LLM 	Ground fuels
    '''
    df = pan.read_csv(datafile)
    df = df.fillna(0.0)
    
    fcc_code = df.FCC_Code.map(lambda x: (int(x)))
    sfp = fcc_code / 100
    cfp = (fcc_code / 10) % 10
    afp = fcc_code % 10
    
    sub_df = pan.DataFrame(
        {
            'Filename': df.Filename.map(lambda x: (str(x).split('.')[0])),
            'ID': df.Fuelbed_number,
            'FCC_Code': fcc_code,
            'SFP': sfp,
            'Reaction': df.Reaction_potential.round(1),
            'Spread': df.Spread_potential.round(1),
            'Flame': df.Flamelength_potential.round(1),
            'CFP': cfp,
            'Initiation': df.Crown_initiation_potential.round(1),
            'Transmissivity': df.get('Crown-crown_trans_potential').round(1),
            'Spread~': df.Crownfire_spread_potential.round(1),
            'AFP': afp,
            'Flame~': df.Flame_available_potential.round(1),
            'Smolder': df.Smolder_available_potential.round(1),
            'Residual': df.Residual_smolder_potential.round(1)
        }
    )
    cols = ['Filename',
            'ID',
            'FCC_Code',
            'SFP',
            'Reaction',
            'Spread',
            'Flame',
            'CFP',
            'Initiation',
            'Transmissivity',
            'Spread~',
            'AFP',
            'Flame~',
            'Smolder',
            'Residual'
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
        120,
        50,
        55,
        35,
        45,
        40,
        40,
        30,
        45,
        70,
        40,
        35,
        35,
        45,
        50
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
    assemble_document(inputfile, output_filename)
else:
    print("\nError: the input file and the output filename are required.\n")








