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
from sys import argv

with open('gapi-key.txt', 'r') as ff:
    ak = ff.read.strip()
    
    ak = 'AIzaSyBQJS1WsLDzpET0-a57HZ7yOpUu-MfKEGE'
    gmaps = googlemaps.Client(key = ak)

    oup = []

    with open(argv[1]) as f:
        addrs = [{k: v for k, v in row.items()}
            for row in csv.DictReader(f, skipinitialspace=True)]

        for addr in addrs:
            try:
                placename_orig = unidecode(addr['local']).upper()

                placename = placename_orig
                placename = placename.replace('.', '')
                placename = ' '.join(placename.strip().split())

                cidade = unidecode(addr['cidade']).upper()

                pname = ' '.join([placename, cidade, addr['uf']])
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
                    'ID': addr['ID'],
                    'placename_orig': placename_orig,
                    'placename': pname,
                    'endr_orig': addr['endereco'],
                    'cidade': cidade,
                    'uf': addr['uf'],
                    'data': result
                })

            except Exception as e:
                print('ERROR!')
                print(e)
                break

    with open('g_places_output.json', 'w') as f:
        json.dump(oup, f)
