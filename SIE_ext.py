##This script downloads the newest updated values of sea ice extent from NOAA. It saves the csv file to the current directory. 
import requests
import os

#North=Northern Hemisphere Sea Ice Extent, 
I_want="North"

file_url = "https://noaadata.apps.nsidc.org/NOAA/G02135/north/daily/data/N_seaice_extent_daily_v3.0.csv"

try:
    r = requests.get(file_url)
    r.raise_for_status()
except requests.exceptions.HTTPError as err:    
    raise SystemExit(err)

filename = "N_seaice_extent_daily_v3.0.csv"
current_directory = os.getcwd()
file_path = os.path.join(current_directory, filename)

with open(file_path, "wb") as f:
    f.write(r.content)
