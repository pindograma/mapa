# find_place.py
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This program is licensed under the GNU General Public License, version 3.
# See the LICENSE file for details.

import googlemaps
import csv
import json
from unidecode import unidecode
from time import sleep

ak = 'AIzaSyBQJS1WsLDzpET0-a57HZ7yOpUu-MfKEGE'
gmaps = googlemaps.Client(key = ak)

oup = []

with open('test_placename_sp.csv') as f:
    addrs = [{k: v for k, v in row.items()}
        for row in csv.DictReader(f, skipinitialspace=True)]

    for addr in addrs:
        try:
            placename_orig = unidecode(addr['LOCAL_VOTACAO']).upper()

            placename = placename_orig
            placename = placename.replace('.', '')
            placename = ' '.join(placename.strip().split())

            cidade = unidecode(addr['LOCALIDADE_LOCAL_VOTACAO']).upper()

            pname = ' '.join([placename, cidade, addr['SGL_UF']])
            print(pname)
            
            sleep(0.3)
            result = gmaps.find_place(pname, 'textquery', fields = [
                'formatted_address',
                'geometry',
                'name',
                'place_id',
                'plus_code'
            ])

            oup.append({
                'placename_orig': placename_orig,
                'placename': pname,
                'endr_orig': addr['norm_endr'],
                'cidade': cidade,
                'uf': addr['SGL_UF'],
                'data': result
            })

        except Exception as e:
            print('ERROR!')
            print(e)
            break

with open('g_places_output.json', 'w') as f:
    json.dump(oup, f)