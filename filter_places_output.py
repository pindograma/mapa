import json
import csv
import re
from unidecode import unidecode
from sys import argv

def write_csv(f, data):
    keys = data[0].keys()
    dict_writer = csv.DictWriter(f, keys)
    dict_writer.writeheader()
    dict_writer.writerows(data)

oup = []
with open(argv[1]) as f:
    entries = json.load(f)

    i = 0
    for g in entries:
        data = g['data']['candidates']
        endr_orig = g['endr_orig']

        for d in data:
            good = True
            addr = unidecode(d['formatted_address'])
            
            city = re.search(r'(^|,\s*)([\w\s\'-]+?) - [A-Z][A-Z],', addr)
            if city is not None:
                city = city.group(2)
                city = city.replace('Municipality', '').replace('-', ' ').strip()

            has_centro = re.search('centro', addr, re.IGNORECASE)
            is_povoado = (
                endr_orig.startswith('POVOADO') or
                endr_orig.startswith('DISTRITO') or
                endr_orig.startswith('COMUNIDADE') or
                endr_orig.startswith('SITIO') or
                endr_orig.startswith('FAZENDA') or
                endr_orig.startswith('ASSENTAMENTO') or
                endr_orig.startswith('LOCALIDADE') or
                endr_orig.startswith('POV ') or
                endr_orig.startswith('ALDEIA') or
                endr_orig.startswith('VILA') or
                endr_orig.startswith('COLONIA'))

            if has_centro and is_povoado:
                good = False

            if (addr.upper().startswith('PREFEITURA') and (
                g['placename'].replace(' ', '').startswith('ESC') or
                g['placename'].replace(' ', '').startswith('EM') or
                g['placename'].replace(' ', '').startswith('EE') or
                g['placename'].replace(' ', '').startswith('CAMARA'))):
                good = False

            if city is not None and city.upper() != g['cidade'].replace('-', ' '):
                good = False

            oup.append({
                'ID': g['ID'],
                'placename': g['placename'],
                'found': d['name'],
                'endr_orig': endr_orig,
                'found_endr': d['formatted_address'],
                'lat': d['geometry']['location']['lat'],
                'lon': d['geometry']['location']['lng'],
                'good': int(good)
            })

    with open('places_output.csv', 'w') as w:
        write_csv(w, oup)
