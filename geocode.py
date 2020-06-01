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

ak = 'AIzaSyBQJS1WsLDzpET0-a57HZ7yOpUu-MfKEGE'
gmaps = googlemaps.Client(key = ak)

oup = []
with open('EXPORT_GOOGLE_ADDR_NF.csv') as f:
    addrs = [{k: v for k, v in row.items()}
        for row in csv.DictReader(f, skipinitialspace=True)]

    for addr in addrs:
        try:
            endr_orig = unidecode(addr['ENDERECO']).upper()
            
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

            bairro = unidecode(addr['BAIRRO_LOCAL_VOT']).upper()
            endr = re.sub(bairro + '$', '', endr)

            cidade = unidecode(addr['LOCALIDADE_LOCAL_VOTACAO']).upper()

            endr = ' '.join([endr, bairro, cidade, addr['SGL_UF']])
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
                'bairro': addr['BAIRRO_LOCAL_VOT'],
                'uf': addr['SGL_UF'],
                'cidade': addr['LOCALIDADE_LOCAL_VOTACAO'],
                'data': result
            })

        except Exception as e:
            print('ERROR! STOPPED!')
            print(e)
            break

with open('g_geocoder_output.json', 'w') as f:
    json.dump(oup, f)