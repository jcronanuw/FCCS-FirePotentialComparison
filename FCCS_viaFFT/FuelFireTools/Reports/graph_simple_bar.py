#-------------------------------------------------------------------------------
# Name:        graph_simple_bar.py
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

# map component parts based on name
# key= column name values = [ylabel, title, color, fuelbed column]
MAP = {
    'Custom_ROS' : [('ft/min','m/min'), 'Rate of Spread', 'royalblue', 'Fuelbed_number'],
    'Custom_FL' : [('ft','m'), 'Flame Length', 'darkorange', 'Fuelbed_number'],
    'Surface_fire_potential' : [('Index (0-9)','Index (0-9)'), 'Surface Fire Potential', 'darkorange', 'Fuelbed_number'],
    'Crown_fire_potential' : [('Index (0-9)','Index (0-9)'), 'Crown Fire Potential', 'royalblue', 'Fuelbed_number'],
    'Available_fuel_potential' : [('Index (0-9)','Index (0-9)'), 'Available Fuel Potential', 'Saddlebrown', 'Fuelbed_number'],
    'Total_aboveground_biomass' : [('tons/acre','Kg/ha'), 'Aboveground Biomass', 'royalblue', 'Fuelbed_number'],
    'Total_carbon_load' : [('tons/acre','Kg/ha'), 'Carbon Loading', 'royalblue', 'Fuelbed_number'],
    # - Consume values
    'Total Consumption' : [('tons/acre','Mg/ha'), 'Total Consumption', 'crimson', 'Fuelbeds'],
    'CH4 Emissions'        : [('lbs/acre','Kg/ha' ), 'CH4 Emissions',  'royalblue', 'Fuelbeds'],
    'CO Emissions' : [('lbs/acre','Kg/ha'), 'CO Emissions', 'royalblue', 'Fuelbeds'],
    'CO2 Emissions' : [('lbs/acre','Kg/ha'), 'CO2 Emissions', 'royalblue', 'Fuelbeds'],
    'NMHC Emissions' : [('lbs/acre','Kg/ha'), 'NMHC Emissions', 'royalblue', 'Fuelbeds'],
    'PM Emissions' : [('lbs/acre','Kg/ha'), 'PM Emissions', 'royalblue', 'Fuelbeds'],
    'PM10 Emissions' : [('lbs/acre','Kg/ha'), 'PM10 Emissions', 'royalblue', 'Fuelbeds'],
    'PM25 Emissions' : [('lbs/acre','Kg/ha'), 'PM25 Emissions', 'royalblue', 'Fuelbeds'],
    '' : [('',''), '', 'gold', 'Fuelbeds'],
}

def get_max_y(max_col_value):
    int_max = int(max_col_value)
    add_this = .1 * int_max
    add_this = add_this if add_this > 0 else 1
    top =  int_max + add_this
    print(top)
    return top

def get_column_values(filename, col_name):
    df = pan.read_csv(filename)
    fuelbed_column = MAP[col_name][-1]
    return pan.DataFrame({'FBNum': df.get(fuelbed_column),'Values': df.get(col_name)})

def do_plot(filename, col_name, title, y_label, bar_color):
    df = get_column_values(filename, col_name)
    bar_xcoords = np.arange(df.index[-1] + 1)
    bar_width = 0.5
    p1 = plt.bar(bar_xcoords, df.Values.values, bar_width, color=bar_color)

    # the xbound changes with the number of columns, this ensures that 1 or 2 column graphs look reasonable
    #  (the column doesn't consume the entire width of the graph)
    plt.gca().set_xbound(-bar_width , len(df.index))

    plt.ylabel(y_label)
    plt.xlabel('Fuelbed Number')
    plt.title(title)
    plt.xticks(bar_xcoords+bar_width/2., df.FBNum.values )
    yticks = math.ceil(get_max_y(max(df.Values)))
    step = (yticks/10. if yticks > 10 else 1)
    plt.yticks(np.arange(0,yticks, step))
    plt.grid(True, axis='y')

    plt.show()


def main():
    if len(sys.argv) >= 2:
        file_with_data = sys.argv[1]
        column_name = sys.argv[2]
        do_metric = 1 if 'do_metric' in sys.argv else 0
        do_plot(file_with_data, column_name, MAP[column_name][1], MAP[column_name][0][do_metric], MAP[column_name][2])
    else:
        print('\nPlease specify: <infile> <data column>.\n')

if __name__ == '__main__':
    main()

