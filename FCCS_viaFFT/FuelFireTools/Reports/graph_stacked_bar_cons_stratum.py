#-------------------------------------------------------------------------------
# Name:        graph_stacked_bar_cons_stratum.py
# Purpose:
#
# Author:      kjells
#
# Created:    1/2/2014
#-------------------------------------------------------------------------------
#!/usr/bin/env python
import sys
import math
import pandas as pan
import numpy as np
import matplotlib.pyplot as plt

TITLE = 'Consumption by Stratum'

def get_max_y(df, col_names_map):
    max_item = 0
    for i in range(0, len(df)):
        tmp_result = 0
        for col in col_names_map:
            tmp_result += df.irow(i).get(col)
        if tmp_result > max_item:
            max_item = np.ceil(tmp_result)
    places = int(np.log10(max_item))
    start = int(max_item / pow(10, places))
    bins = (start + 2)
    return (bins, (bins * pow(10, places)))

def get_column_values(col_names, infile):
    df = pan.read_csv(infile)
    df_sub = pan.DataFrame({'FBNum': df.Fuelbeds})
    for col in col_names.keys():
        df_sub[col] = df.get(col_names[col])
    return df_sub

def do_plot(col_names_map, infile, do_metric):
    df = get_column_values(col_names_map, infile)
    bar_xcoords = np.arange(df.index[-1] + 1)
    bar_width = 0.5
    colors = ['dimgray', 'sienna','darkorange', 'gold', 'forestgreen', 'royalblue']  # html colors supported
    color_idx = 0
    bottoms = np.zeros(df.index[-1] + 1)
    plot_chunks = []
    layers = ['Ground','LLM', 'Wood', 'Herb', 'Shrub', 'Canopy']
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

    plt.xlabel('Fuelbed Number')
    plt.title(TITLE)
    plt.ylabel("Mg/ha" if do_metric else "tons/acre")
    plt.xticks(bar_xcoords+bar_width/2., df.FBNum.values )
    bins, yticks = get_max_y(df, col_names_map)
    plt.yticks(np.arange(0,yticks, int(yticks/bins)))
    plt.grid(True, axis='y')

    plt.show()

def main():
    if len(sys.argv) >= 2:
        col_names_map = {
            'Canopy': 'Canopy Consumption',
            'Shrub': 'Shrub Consumption',
            'Herb': 'Herb Consumption',
            'Wood': 'Wood Consumption',
            'LLM': 'LLM Consumption',
            'Ground': 'Ground Consumption' }
        infile = sys.argv[1]
        do_metric = True if 'do_metric' in sys.argv else False
        do_plot(col_names_map, infile, do_metric)
    else:
        print("\n\Error: please supply a data input file.n")

if __name__ == '__main__':
    main()
