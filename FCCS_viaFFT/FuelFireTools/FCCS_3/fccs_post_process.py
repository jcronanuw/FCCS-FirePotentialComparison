import sys
import os
import pandas as pan

def to_no_convert(ignore_me): return ignore_me
    
def to_mgha(val):
    return (val*2.2416).round(2)
    
def to_meter(val):
    return (val*.3048).round(2)
    
def to_meter_per_min(val):
    return (val*0.3047999902464).round(2)
    
def to_kJ_per_m2(val):
    return (val*0.1892754465477).round(2)
    
def to_centimeter(val):
    return (val*2.54).round(2)
    
def is_comment(line):
    return True if line.startswith('#') else False
    
def build_map(filename):
    if os.path.exists(filename):
        the_map = {}
        cols = []
        with open(filename, 'r') as infile:
            for line in infile:
                if is_comment(line): continue
                chunks = line.split(',')
                the_map[chunks[0].strip()] = (chunks[1].strip(), globals()[chunks[2].strip()])
                cols.append(chunks[1].strip())
        return (the_map, cols)
    else:
        print('\n\n\Error: {} does not exist!!!n'.format(filename))
        exit(1)

def do_convert(f, to_metric, outfile, map_and_cols):
    df = pan.read_csv(f)
    output = {}
    mapper = map_and_cols[0]
    for key in mapper.keys():
        try:
            output[mapper[key][0]] = mapper[key][1](df.get(key)) if to_metric else df.get(key)
        except:
            print('\nError: Problem using key : "{}"'.format(key))
    new_df = pan.DataFrame(output)
    cols = map_and_cols[1]
    new_df = new_df.reindex_axis(cols, axis=1)    
    new_df.to_csv(outfile, index=False)
    
if len(sys.argv) > 1:
    if len(sys.argv) > 3:
        # a fifth argument of anything means do metric conversion
        to_metric = True if 5 == len(sys.argv) else False
        map_and_cols = build_map(sys.argv[2])
        do_convert(sys.argv[1], to_metric, sys.argv[3], map_and_cols)
    
else:
    print('\n\nUse to convert FCCS output to metric equivalents. Provide file to convert and optional mapping file.')
    print('\t <scriptname> <fccs_summary.csv> <mapping file> <outputfile> <to_metric>')
    
    
    
    