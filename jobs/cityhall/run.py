import requests
import sys
import csv
import datetime
import logging
from bdeep.context import getJobArgs

log = logging.getLogger('BDEEP')

args = getJobArgs()

authEndpoint = args['authEndpoint']
getEndpoint = args['getEndpoint']
filePath = args['outputPath']

lines = sys.argv[1].split(',')
today = str(datetime.date.today())

with open(filePath % today, "a") as f:
    csvwriter = csv.writer(f)

    s = requests.Session()
    r = s.post(authEndpoint)

    for line in lines:
        payload = {"codigoLinha": line}
        r = s.get(getEndpoint, params=payload)

        json = r.json()

        for pos in json['vs']:
            csvwriter.writerow([line, json['hr'], pos['a'], pos['p'], pos['px'], pos['py']])
