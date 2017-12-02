#-------------------------------------------------------------------------------
# Name:        ev_data.py
# Purpose:     Read the environmental data file under which the last set of
#               calculations were run
#
# Author:      kjells
#
# Created:     05/12/2013
#-------------------------------------------------------------------------------
#!/usr/bin/env python

import os
import pandas as pan

EV_FILE = './00_results/fccs/evreportdata.csv'

class ev_data(object):
    def __init__(self):
        self._df = None
        if os.path.exists(EV_FILE):
            self._df = pan.read_csv(EV_FILE)
            self._df = self._df.fillna(0.0)
            if 'true' == str(self._df.UseMetric[0]).lower():
                self._df.UnitSize = self._df.UnitSize * 0.404686


    def valid(self):
        return False if self._df is None else True

    def get_parameter_value(self, name):
        retval = ""
        if self.valid():
            if name in self._df.columns.tolist():
                retval = self._df.get(name)[0]
        return retval

def main():
    ev = ev_data()
    check = ev.valid()
    print(ev.get_parameter_value('Wind'))
    print(ev.get_parameter_value('Name'))
    print(ev.get_parameter_value('Slope'))
    print(ev.get_parameter_value('FM1000'))


if __name__ == '__main__':
    main()











