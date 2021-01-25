# geocode.py
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

import googlemaps
import csv
import pprint
import re
import json
from unidecode import unidecode
from sys import argv
from pathlib import Path

with open('gapi-key.txt', 'r') as ff:
    ak = ff.read().strip()
    gmaps = googlemaps.Client(key = ak)

    oup = []
    with open(argv[1]) as f:
        addrs = [{k: v for k, v in row.items()}
            for row in csv.DictReader(f, skipinitialspace=True)]

        for addr in addrs:
            try:
                endr_orig = unidecode(addr['endereco']).upper()
                
                endr = endr_orig
                endr = ' '.join(endr.strip().split())

                endr = endr.replace("ZONA URBANA", "")

                endr = endr.split('(')[0].strip()
                endr = endr.split('AO LADO')[0].strip()
                endr = endr.split(' - ')[0].strip()
                
                m = re.search(r'.*?(?=,\s+[^"\dSN])', endr)
                if m is not None:
                    endr = m.group(0).strip()

                endr = endr.replace('FONE ', '').replace('TELEFONE ', '')
                endr = re.sub(r'\(?\d{2}\)?\d{4}\d?-?\d{4}', '', endr)
                endr = re.sub(r'\d{4}\d?-?\d{4}', '', endr)

                bairro = unidecode(addr['bairro']).upper()
                endr = re.sub(bairro + '$', '', endr)

                cidade = unidecode(addr['cidade']).upper()

                endr = ' '.join([endr, bairro, cidade, addr['uf']])
                print(endr)

                components = {
                    'locality': bairro,
                    'administrative_area': cidade
                }

                result = gmaps.geocode(endr, components)

                oup.append({
                    'ID': addr['ID'],
                    'endr_orig': endr_orig,
                    'endr': endr,
                    'bairro': addr['bairro'],
                    'uf': addr['uf'],
                    'cidade': addr['cidade'],
                    'data': result
                })

            except Exception as e:
                print('ERROR! STOPPED!')
                print(e)
                break

    json.dump(oup, open('backup.json', 'w'))
    with open(Path(argv[1]).stem + '.json', 'w') as f:
        json.dump(oup, f)
