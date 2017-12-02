#-------------------------------------------------------------------------------
# Name:        concat_feps_outputs.py
# Purpose:     This script concatenates the 3 different FEPS output files into one.
#
# Author:      kjells
#
# Created:     6/10/2015
#-------------------------------------------------------------------------------

import pandas as pd
import os

'''The 3 FEPS output files'''
CONS_OUT = 'feps_out_fractional_consumption.csv'
EMISSIONS_OUT =  'feps_out_hourly_emissions.csv'
PLUME_OUT = 'feps_out_plume.csv'

def all_files_available():
    return os.path.exists(CONS_OUT) and os.path.exists(EMISSIONS_OUT) and os.path.exists(PLUME_OUT)

def concat_outputs():
    if all_files_available():
        df_cons = pd.read_csv(CONS_OUT)
        df_emis = pd.read_csv(EMISSIONS_OUT)
        df_emis = df_emis.drop('hour', axis=1)  # 'hour' appears in all files so drop it from 2
        df_plume = pd.read_csv(PLUME_OUT)
        df_plume = df_plume.drop('hour', axis=1)  # 'hour' appears in all files so drop it from 2
        
        df = pd.concat([df_cons, df_emis, df_plume], axis=1)
        
        cols_in_order = [
            'Hour',
            'Area_fract',
            'Flame',
            'Smolder',
            'Residual',
            'CO2',
            'CO',
            'CH4',
            'PM25',
            'PM10',
            'NOx',
            'SO2',
            'NH3',
            'NMHC',
            'Heat',
            'Smold_fraction',
            'Plume_bot',
            'Plume_top',
            'Cons_eff',
            'Buoy_eff',
            'Ent_eff',
            'Briggs_stability',
            'Briggs_buoyancy',
            'Briggs_plume_ht']
        
        
        col_contents_in_order = [
            df.get('hour'),
            df.get('area_fract'),
            df.get('flame'),
            df.get('smolder'),
            df.get('residual'),
            df.get('CO2'),
            df.get('CO'),
            df.get('CH4'),
            df.get('PM25'),
            df.get('PM10'),
            df.get('NOx'),
            df.get('SO2'),
            df.get('NH3'),
            df.get('VOC'),
            df.get('heat'),
            df.get('smold_frac'),
            df.get('plume_bot'),
            df.get('plume_top'),
            df.get('cons_eff'),
            df.get('buoy_eff'),
            df.get('ent_eff'),
            df.get('briggs_stab'),
            df.get('briggs_buoy'),
            df.get('briggs_max_H')
            ]
            
        # Rename and reorder columns in the concatenated DataFrame
        df_out = pd.DataFrame({v: col_contents_in_order[i] for i,v in enumerate(cols_in_order)})
        df_out = df_out[cols_in_order]
        
        df_out.to_csv('feps_all.csv', index = False)
            
#-------------------------------------------------------------------------------
#   Run...
#-------------------------------------------------------------------------------
if __name__ == '__main__':
    concat_outputs()
    
    
