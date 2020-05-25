
import json
import csv
import re
from unidecode import unidecode

def write_csv(f, data):
    keys = data[0].keys()
    dict_writer = csv.DictWriter(f, keys)
    dict_writer.writeheader()
    dict_writer.writerows(data)

oup = []
with open('google-places-output/2.json') as f:
    entries = json.load(f)

    for g in entries:
        oup.append({
            'placename_orig': g['placename_orig']
            #'endr_orig': g['endr_orig']
        })
    
    with open('debug_places.csv', 'w') as w:
        write_csv(w, oup)
