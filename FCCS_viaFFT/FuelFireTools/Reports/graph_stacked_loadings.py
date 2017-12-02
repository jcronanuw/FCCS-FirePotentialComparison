#-------------------------------------------------------------------------------
# Name:        graph_stacked_loadings.py
# Purpose:
#
# Author:      kjells/pceagle
#
# Created:     08/2014
#-------------------------------------------------------------------------------
#!/usr/bin/env python
import sys
import math
import pandas as pan
import numpy as np
import matplotlib.pyplot as plt

CARBON_MULTIPLIER_LOW = 0.4
CARBON_MULTIPLIER_HIGH = 0.5
LOADINGS_MULTIPLIER = 1.0

#FCCS_SUMMARY_FILE = 'short_fccs_summary.csv'
FCCS_SUMMARY_FILE = 'fccs_summary.csv'
Y_LABEL = ('tons/ac', 'Mg/ha')
TITLE = ('Aboveground Biomass by Stratum', 'Carbon Loading by Stratum')

def get_max_y(df, col_names_map):
    max_item = max(df['total'])
    places = int(np.log10(max_item))
    #places=1
    start = int(max_item / pow(10, places))
    bins = (start + 2)
    return (bins, (bins * pow(10, places)))


def get_content(datafile, do_carbon=False):
    ''' These are the columns required for this report. Retrieve them from the .csv
    file and order them appropriatly
    '''
    mult_low = CARBON_MULTIPLIER_LOW if do_carbon else LOADINGS_MULTIPLIER
    mult_high = CARBON_MULTIPLIER_HIGH if do_carbon else LOADINGS_MULTIPLIER

    df = pan.read_csv(datafile)
    df = df.fillna(0.0)
    sub_df = pan.DataFrame(
        {
            'FBNum': df.Fuelbed_number,
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
            'Ground': (((df.Ground_upperduff_load + df.Ground_lowerduff_load)*mult_low) +
                ((df.Ground_basalaccum_load + df.Ground_squirrelmid_load)*mult_high)).round(2)
        }
    )
    sub_df['total'] = sub_df['Canopy'] + sub_df['Shrub'] + sub_df['Herb'] + sub_df['Wood']+sub_df['LLM']+sub_df['Ground']
    return sub_df

def do_plot(infile, do_carbon, do_metric):
    df = get_content(infile, do_carbon)
    # Figure,subplot
    fig = plt.figure()
    ax = fig.add_subplot(111)
    bar_xcoords = np.arange(df.index[-1] + 1)
    bar_width = 0.5
    colors = ['dimgray', 'sienna','darkorange', 'gold', 'forestgreen', 'royalblue']  # html colors supported
    color_idx = 0
    bottoms = np.zeros(df.index[-1] + 1)

    plot_chunks = []
    heights = []
    layers = ['Ground','LLM', 'Wood', 'Herb', 'Shrub', 'Canopy']
    for key in layers:
        plot_chunks.append(ax.bar(bar_xcoords, df.get(key).values, bar_width, color=colors[color_idx], bottom=bottoms))
        bottoms += df.get(key).values
        color_idx += 1

    # - fussy ordering
    layers.reverse()
    plot_chunks.reverse()
    legend = ax.legend([i[0] for i in plot_chunks], layers, loc='best')
    legend.get_frame().set_alpha(0.5)
    legend.title="Stratum"
    plot_chunks.reverse()

    # the xbound changes with the number of columns, this ensures that 1 or 2 column graphs look reasonable
    #  (the column doesn't consume the entire width of the graph)
    fig.gca().set_xbound(-bar_width , len(df.index))

    plt.ylabel(Y_LABEL[1 if do_metric else 0])
    plt.xlabel('Fuelbed Number')
    plt.title(TITLE[1 if do_carbon else 0])
    plt.xticks(bar_xcoords+bar_width/2., df.FBNum.values )
    bins, yticks = get_max_y(df, layers)
    interval = int(yticks/bins)
    plt.yticks(np.arange(0,yticks,interval))
    plt.grid(True, axis='y')

    #for index, row in df.iterrows():
    #    height = row['total']
    #    ax.text(bar_xcoords[index]+bar_width/2., height, '%d'%int(height),
    #            ha='center', va='bottom')

    plt.show()

def main():
    if len(sys.argv) >= 2:
        infile = sys.argv[1]
        do_metric = True if 'do_metric' in sys.argv else False
        do_carbon = True if 'do_carbon' in sys.argv else False
        do_plot(infile, do_carbon, do_metric)
    else:
        print("\n\Error: please supply a data input file.n")

if __name__ == '__main__':
    main()
