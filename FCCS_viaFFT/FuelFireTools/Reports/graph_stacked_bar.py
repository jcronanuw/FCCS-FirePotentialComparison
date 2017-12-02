#-------------------------------------------------------------------------------
# Name:        graph.py
# Purpose:
#
# Author:      kjells
#
# Created:     10/03/2013
#-------------------------------------------------------------------------------
#!/usr/bin/env python
import sys
import math
import pandas as pan
import numpy as np
import matplotlib.pyplot as plt

#FCCS_SUMMARY_FILE = 'short_fccs_summary.csv'
FCCS_SUMMARY_FILE = 'fccs_summary.csv'
Y_LABEL = ('BTU/ft2/min', 'kw/m2')
TITLE = 'Reaction Intensity'

def get_max_y(df, col_names_map):
    max_item = max(df['total'])
    places = int(np.log10(max_item))
    start = int(max_item / pow(10, places))
    bins = (start + 2)
    return (bins, (bins * pow(10, places)))

def get_max_y_old(df, col_names_map):
    return max(df['total'])

def get_column_values(col_names, infile):
    df = pan.read_csv(infile)
    df_sub = pan.DataFrame({'FBNum': df.Fuelbed_number,'Values': df.get('Fuelbed_number')})
    for col in col_names.keys():
        df_sub[col] = df.get(col_names[col])
    return df_sub

def do_plot(col_names_map, infile, do_metric):
    df = get_column_values(col_names_map, infile)
    bar_xcoords = np.arange(df.index[-1] + 1)
    bar_width = 0.5
    colors = ['sienna','darkorange','gold','forestgreen']  # html colors supported
    color_idx = 0
    bottoms = np.zeros(df.index[-1] + 1)
    plot_chunks = []
    layers = ['LLM', 'Wood', 'Herb', 'Shrub']
    for key in layers:
        plot_chunks.append(plt.bar(bar_xcoords, df.get(key).values, bar_width, color=colors[color_idx], bottom=bottoms))
        bottoms += df.get(key).values
        color_idx += 1

    # - fussy ordering
    layers.reverse()
    plot_chunks.reverse()
    legend = plt.legend([i[0] for i in plot_chunks], layers, loc='best')
    legend.get_frame().set_alpha(0.5)
    plot_chunks.reverse()

    # the xbound changes with the number of columns, this ensures that 1 or 2 column graphs look reasonable
    #  (the column doesn't consume the entire width of the graph)
    plt.gca().set_xbound(-bar_width , len(df.index))

    plt.ylabel(Y_LABEL[1 if do_metric else 0])
    plt.xlabel('Fuelbed Number')
    plt.title(TITLE)
    plt.xticks(bar_xcoords+bar_width/2., df.FBNum.values )
    bins, yticks = get_max_y(df, col_names_map)
    plt.yticks(np.arange(0,yticks, int(yticks/bins)))
    plt.grid(True, axis='y')
    plt.show()

def main():
    if len(sys.argv) >= 2:
        col_names_map = {
            'total': 'Benchmark_RI',
            'Shrub': 'Benchmark_RI_Shrub',
            'Herb': 'Benchmark_RI_Herb',
            'Wood': 'Benchmark_RI_Wood',
            'LLM': 'Benchmark_RI_LLM' }
        infile = sys.argv[1]
        do_metric = True if 'do_metric' in sys.argv else False
        do_plot(col_names_map, infile, do_metric)
    else:
        print("\n\Error: please supply a data input file.n")

if __name__ == '__main__':
    main()
