#-------------------------------------------------------------------------------
# Name:        graph_growth.py
# Purpose:
#
# Author:      kjells
#
# Created:     4/14/2015
#-------------------------------------------------------------------------------
#!/usr/bin/env python
import sys
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

FEPS_INPUT_FILE = './00_results/feps/runfeps.csv'
FIRE_SIZE_COLUMN = 'fire_size_final'

MAP = {
    'firesize' : {
        'data_columns' : ['size'],
        'title' : 'Fire Size',
        'axis_x' : 'hours',
        'axis_y' : ['Acres', 'Hectares'],
        'use_firesize' :  False
    },
    'consumption' : {
        'data_columns' : ['flame','smolder','residual'],
        'title' : 'Hourly Consumption',
        'axis_x' : 'hours',
        'axis_y' : ['tons/hour', 'mg/hour'],
        'use_firesize' :  True
    },
    'CO2' : {
        'data_columns' : ['CO2'],
        'title' : 'CO2 Emissions',
        'axis_x' : 'hours',
        'axis_y' : ['tons/hr', 'mg/hr'],
        'use_firesize' :  False
    },
    'CO' : {
        'data_columns' : ['CO'],
        'title' : 'CO Emissions',
        'axis_x' : 'hours',
        'axis_y' : ['tons/hr', 'mg/hr'],
        'use_firesize' :  False
    },
    'CH4' : {
        'data_columns' : ['CH4'],
        'title' : 'CH4 Emissions',
        'axis_x' : 'hours',
        'axis_y' : ['tons/hr', 'mg/hr'],
        'use_firesize' :  False
    },
    'PM25' : {
        'data_columns' : ['PM25'],
        'title' : 'PM25 Emissions',
        'axis_x' : 'hours',
        'axis_y' : ['tons/hr', 'mg/hr'],
        'use_firesize' :  False
    },
    'PM10' : {
        'data_columns' : ['PM10'],
        'title' : 'PM10 Emissions',
        'axis_x' : 'hours',
        'axis_y' : ['tons/hr', 'mg/hr'],
        'use_firesize' :  False
    },
    'NMHC' : {
        'data_columns' : ['NMHC'],
        'title' : 'VOC Emissions',
        'axis_x' : 'hours',
        'axis_y' : ['tons/hr', 'mg/hr'],
        'use_firesize' :  False
    },
    'plume_top' : {
        'data_columns' : ['plume_top', 'plume_bot'],
        'title' : 'Plume Rise',
        'axis_x' : 'hours',
        'axis_y' : ['feet', 'meter'],
        'use_firesize' :  False
    }
}

def get_fire_size(keyword):
    if MAP[keyword]['use_firesize']:
        df = pd.read_csv(FEPS_INPUT_FILE)
        return df.get(FIRE_SIZE_COLUMN)[0]
    return 1

def do_plot(filename, keyword, truncate_pos=-1, do_metric=False):
    idx_units = 1 if do_metric else 0
    df = pd.read_csv(filename)
    fire_size = get_fire_size(keyword)
    print(fire_size)
    for col in MAP[keyword]['data_columns']:
        ys = df.get(col)[:int(truncate_pos)] * fire_size
        plt.plot(df.index[:int(truncate_pos)], ys)
    plt.xlabel(MAP[keyword]['axis_x'])
    plt.ylabel(MAP[keyword]['axis_y'][idx_units])
    plt.title(MAP[keyword]['title'])
    
    # only show a legend if there is more than one variable
    if len(MAP[keyword]['data_columns']) > 1:
        plt.legend(MAP[keyword]['data_columns'], loc='best')
        
    plt.gcf().canvas.set_window_title(MAP[keyword]['title'])
    plt.show()


def main():
    exit_code = 1
    if len(sys.argv) >= 2:
        file_with_data = sys.argv[1]
        keyword = sys.argv[2]
        fire_duration = sys.argv[3]
        do_metric = 1 if 'do_metric' in sys.argv else 0
        if os.path.exists(file_with_data):
            if keyword in MAP.keys():
                do_plot(file_with_data, keyword, fire_duration, do_metric)
                exit_code = 0
            else:
                print('\nError: invalid keyword {}'.format(keyword))
        else:
            print('\nError: input file "{}" does not exist.'.format(file_with_data))
    else:
        print('\nPlease specify: <infile> <keyword>.\n')
    exit(exit_code)

if __name__ == '__main__':
    main()

