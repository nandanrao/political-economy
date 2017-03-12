import pandas as pd
import numpy as np
import requests
from decimal import Decimal
from re import sub
from bs4 import BeautifulSoup

def get_header(url):
    soup = BeautifulSoup(requests.get(url).text, 'html.parser')
    return [t.td.text.strip() for t in soup.find_all('table')[0].find_all('tr')[1:]]

def read_table(path, url = None, **kwargs):
    if url:
        headers = get_header(url)
    names = headers if url else None
    return pd.read_csv(path, sep = ",", quotechar="|", header = None, names = names, **kwargs)

def mathify(s):
    return Decimal(sub(r'[^\d.]', '', s))
