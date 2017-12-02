#-------------------------------------------------------------------------------
# Name:        report_cons_unit_emissions.py
# Purpose:     Consume Emissions report for the unit
#              Use reportlab to generate pdf reports from the calculated data (in .csv files)
#              - http://www.reportlab.com/software/opensource/
#
# Author:      kjells
# Created:     2/28/2014
#-------------------------------------------------------------------------------
#!/usr/bin/env python

import sys
import datetime
import pandas as pan
import numpy as np
from reportlab.platypus import SimpleDocTemplate, Table, TableStyle, Paragraph, Frame, Spacer
from reportlab.lib import colors
from reportlab.lib.units import cm
from reportlab.lib.pagesizes import letter, portrait
from reportlab.lib.styles import ParagraphStyle, getSampleStyleSheet
from reportlab.lib.enums import TA_LEFT, TA_RIGHT, TA_CENTER, TA_JUSTIFY
import num_canvas
import ev_data
import constants as const
from collections import defaultdict
import os

REPORTS = {
    'emissions': {
        'columns': ['CH4 Emissions', 'CO Emissions', 'CO2 Emissions', 'NMHC Emissions', 'PM Emissions', 'PM10 Emissions', 'PM25 Emissions'],
        'conversion_factor' : (0.0005, 0.0001), # converts from pounds to tons or kg to megagrams (inputs should be correctly english or metric)
        'units' : ('Emissions (Tons)', 'Emissions (Kg)'),
        'header_col_first' : 'Pollutant',
        'title' : 'Total Emissions',
        'format' : '{:.2f}'
    },
    'consumption': {
        'columns': ['Total Consumption','Canopy Consumption','Shrub Consumption','Herb Consumption','Wood Consumption','LLM Consumption','Ground Consumption'],
        'conversion_factor' : (1, 1),
        'units' : ('Consumption (Tons)', 'Consumption (Mg)'),
        'header_col_first' : 'Stratum',
        'title' : 'Total Consumption',
        'format' : '{:.2f}'
    },
    'heatrelease': {
        'columns': ['Total Heat Release','Flaming Heat Release','Smoldering Heat Release','Residual Heat Release'],
        'conversion_factor' : (1, 1),
        'units' : ('BTU', 'BTU'),
        'header_col_first' : 'Phases',
        'title' : 'Total Heat Release',
        'format' : '{:.2e}'
    }}

def create_document(name):
    # basic layout, landscape/portrait and margins
    pdfReportPages = name
    doc = SimpleDocTemplate(pdfReportPages, pagesize=portrait(letter),
        topMargin=const.MARGIN_TOP, bottomMargin=const.MARGIN_BOTTOM,
        leftMargin=const.MARGIN_LEFT, rightMargin=const.MARGIN_RIGHT)
    return doc

def check_param(val):
    return (not val or np.isnan(val))

def add_header_content(elements, ev, title_string):
    styles=getSampleStyleSheet()
    styles.add(ParagraphStyle(name='Center', alignment=TA_CENTER))

    # get environmental scenario data
    # Column headers for ev data
    #    BUName,EVName,UseMetric,Wind,Slope,FM1,FM10,FM100,FM1000,FMDuff,FMHerb,FMShrub,ConCanopy,ConShrub,ConPiles
    bu_name = 'None' if not ev.valid else ev.get_parameter_value('BUName')
    unitsize = ev.get_parameter_value('UnitSize')
    permit = ev.get_parameter_value('Permit')
    fire_type = ev.get_parameter_value('Type')

    # Title
    ptext = '<font size={}>Consume 4.2 {} for unit {}</font>'.format(const.FONTSIZE_TITLE, title_string, bu_name)
    elements.append(Paragraph(ptext, styles["Center"]))

    # Table for date, bu name, and ev name
    cols = [100,200]

    header_data = []
    today = datetime.date.today()
    header_data.append(['Report date:', '{}'.format(today.strftime('%B %d, %Y'))])
    header_data.append(['Unit Name:', '{}'.format(bu_name)])
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
    return bu_name

def get_content(datafile, ev, report_type):
    def get_bu_map(unit_name):
        fb_to_pct_map = {}
        unit_size = 0
        unit_path = './UserConfig/{}.bu'.format(unit_name)
        if os.path.exists(unit_path):
            with open(unit_path, 'r') as infile:
                linecount = 0
                for line in infile:
                    if line.startswith('UnitSize'):
                        unit_size = int(float(line.strip().split('=')[1]))
                    if line.startswith('Fuelbeds'):
                        chunks = line.split(',')
                        fb = chunks[0].split('\\')[-1]
                        percentage = chunks[-3]
                        fb_to_pct_map[fb] = float(percentage) * 0.01
                    linecount += 1
        return (unit_size, fb_to_pct_map)

    use_metric = ev.get_parameter_value('UseMetric')
    unit_choice = REPORTS[report_type]['units']
    unit_name = unit_choice[0] if not use_metric else unit_choice[1]

    conversion_choice = REPORTS[report_type]['conversion_factor']
    conversion_factor = conversion_choice[0] if not use_metric else conversion_choice[1]

    unit_size, fb_by_percent = get_bu_map(ev.get_parameter_value('BUName'))
    df = pan.read_csv(datafile)
    df = df.fillna(0.0)
    totals = defaultdict(float)
    columns =REPORTS[report_type]['columns']
    for row in df.iterrows():
        pct = unit_size * fb_by_percent[row[1].get('Filename')]
        for p in columns:
            totals[p] += row[1].get(p) * pct

    retval = [[REPORTS[report_type]['header_col_first'], unit_name]]
    for p in columns:
        val = totals[p] * conversion_factor
        formater = REPORTS[report_type]['format']
        retval.append([p, formater.format(val)])
    return retval

def assemble_document(datafile, outfile, report_type):
    # create the document
    doc = create_document(output_filename)

    # "Flowable" objects container
    elements = []

    # header content
    ev = ev_data.ev_data()
    add_header_content(elements, ev, REPORTS[report_type]['title'])

    # retrieve data and set up column spacing
    contents = get_content(datafile, ev, report_type)
    cols = [
        100,
        100
        ]

    # Assemble data for each column using simple loop to append it into data list
    data = []

    # add the actual data
    '''
    for i in contents.index:
        data.append(list(contents.irow(i).values))
    '''
    for row in contents:
        data.append(row)

    # add the table style -- this applies formatting to the table
    table_that_splits_over_pages= Table(data, colWidths=cols, repeatRows=1)
    table_that_splits_over_pages.hAlign = 'LEFT'
    tbl_style = TableStyle([('TEXTCOLOR',(0,0),(-1,-1),colors.black),
                           ('VALIGN',(0,0),(-1,-1),'TOP'),
                           ('ALIGN',(0,0),(1,0), 'CENTER'),
                           ('ALIGN',(1,1),(-1,-1), 'RIGHT'),
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
if 4 == len(sys.argv):
    inputfile = sys.argv[1]
    output_filename = sys.argv[2]
    type = sys.argv[3]
    assemble_document(inputfile, output_filename, type)
else:
    print("\nError: the input file, the output filename, and the type/catagory (emisssion, consumption, heatrelease) are required.\n")








