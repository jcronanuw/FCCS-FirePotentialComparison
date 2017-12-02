#-------------------------------------------------------------------------------
# Name:        graph_stacked_bar_potential.py
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

TITLE = 'FCCS Potentials'

def get_max_y(df, col_names_map):
    # each of the 3 categories is 0-9
    return (9) + 1

def get_max_y_old(df, col_names_map):
    return max(df['total'])

def get_column_values(col_names, infile):
    df = pan.read_csv(infile)
    df_sub = pan.DataFrame({'FBNum': df.Fuelbed_number,'Values': df.get('Fuelbed_number')})
    for col in col_names.keys():
        df_sub[col] = df.get(col_names[col])
    return df_sub

def do_plot(col_names_map, infile):
    df = get_column_values(col_names_map, infile)
    bar_width = 0.20
    bar_xcoords = np.arange(df.index[-1] + 1)
    colors = ['darkorange', 'royalblue', 'sienna']  # html colors supported
    color_idx = 0
    layers = ['SFP', 'CFP', 'AFP']
    legend_layers = ['Surface', 'Crown', 'Available']
    _, ax = plt.subplots()
    plot_chunks = []
    for idx, key in enumerate(layers):
        int_vals = [np.around(i) for i in df.get(key).values]
        plot_chunks.append(ax.bar(bar_xcoords + idx*bar_width, int_vals, bar_width, color=colors[color_idx]))
        color_idx += 1

    legend = plt.legend([i[0] for i in plot_chunks], legend_layers, loc='best')
    legend.get_frame().set_alpha(0.5)

    # the xbound changes with the number of columns, this ensures that 1 or 2 column graphs look reasonable
    #  (the column doesn't consume the entire width of the graph)
    plt.gca().set_xbound(-bar_width , len(df.index))

    plt.xlabel('Fuelbed Number')
    plt.ylabel('Index (0-9)')
    plt.title(TITLE)
    plt.xticks(bar_xcoords+((3*bar_width)/2.), df.FBNum.values )
    yticks = get_max_y(df, col_names_map)
    plt.yticks(np.arange(0,yticks, 3))
    plt.grid(True, axis='y')

    plt.show()

def main():
    if len(sys.argv) >= 2:
        col_names_map = {
            'AFP': 'Available_fuel_potential',
            'CFP': 'Crown_fire_potential',
            'SFP': 'Surface_fire_potential' }
        infile = sys.argv[1]
        do_plot(col_names_map, infile)
    else:
        print("\n\Error: please supply a data input file.n")

if __name__ == '__main__':
    main()
