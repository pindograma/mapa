# filter_geocoder_output.py
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

import json
import csv
from unidecode import unidecode

not_found = []
found = []
ambiguous = []
bad = []

def write_csv(f, data):
    keys = data[0].keys()
    dict_writer = csv.DictWriter(f, keys)
    dict_writer.writeheader()
    dict_writer.writerows(data)

def get_component(d, component):
    for c in d['address_components']:
        if component in c['types']:
            return c['short_name']

    return None

def is_evidently_bad(g, d):
    if d['geometry']['location_type'] == 'APPROXIMATE':
        return True

    if d['geometry']['location_type'] == 'GEOMETRIC_CENTER':
        return True

    uf = get_component(d, 'administrative_area_level_1')
    cidade = get_component(d, 'administrative_area_level_2')

    if uf is None or cidade is None:
        return True

    if unidecode(uf).upper() != g['uf']:
        return True

    if unidecode(cidade).upper() != unidecode(g['cidade']).upper():
        return True

    return False

def handle(g, d):
    return {
        'ID': g['ID'],
        'endr_orig': g.get('endr_orig'),
        'endr': g['endr'],
        'uf': g['uf'],
        'cidade': g['cidade'],
        'bairro_orig': g['bairro'],
        'bairro': get_component(d, 'sublocality_level_1'),
        'lat': d['geometry']['location']['lat'],
        'lon': d['geometry']['location']['lng'],
        'location_type': d['geometry']['location_type']
    }

with open('google-output/output9.json') as f:
    entries = json.load(f)

    for g in entries:
        data = g['data']

        if len(data) == 0:
            not_found.append({
                'endr': g['endr'],
                'endr_orig': g.get('endr_orig')
            })

        elif len(data) == 1:
            if not is_evidently_bad(g, data[0]):
                found.append(handle(g, data[0]))
            else:
                bad.append(handle(g, data[0]))

        else:
            candidates = []
            for d in data:
                if not is_evidently_bad(g, d):
                    candidates.append(d)

            if len(candidates) == 1:
                found.append(handle(g, candidates[0]))

            else:
                ambiguous.extend([handle(g, c) for c in candidates])

    with open('not_found.csv', 'w') as nf:
        write_csv(nf, not_found)

    with open('ambiguous.csv', 'w') as a:
        write_csv(a, ambiguous)

    with open('geocoded.csv', 'w') as geo:
        write_csv(geo, found)
    
    with open('geocoded_bad.csv', 'w') as b:
        write_csv(b, bad)
