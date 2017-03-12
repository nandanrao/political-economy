import pandas as pd
import numpy as np
import editdistance
import re
import requests
from tools import mathify, get_header, read_table

def clean_name(name, thresh = 30):
    return str(name).strip().lower()[0:thresh]

def give_name(df, name):
    return df.assign(name = df[name].map(clean_name))

def combiner(a, b, c):
    df = pd.merge(pd.merge(a, b, left_on='name', right_on='original'),
                    c, left_on='committee', right_on='name')
    return (df.assign(name = df.name_y)
            .drop(['committee', 'original', 'distance', 'name_x', 'name_y'], axis=1))

def make_merge_frame(pacs, committees):
    lost = [s for s in pacs.name if s not in committees.name.unique()]
    a = assign_lost_ones(lost, committees)
    return (a[(a.distance > 0) & (a.distance < 6)]
            .sort_values('distance')
            .groupby('original')
            .head(1))

def clean_super_pac_data(df, committees, thresh = 1000):
    d = df[df['IndependentExpenditures'].map(mathify) > thresh]
    pacs = give_name(d, 'Group')
    return pacs

def assign_lost_ones(lost, committees):
    committee_names = committees.name.unique()
    distances = [(editdistance.eval(l, s), l, s) for l in lost for s in committee_names]
    return pd.DataFrame(distances, columns=['distance', 'original', 'committee'])

def id_super_pacs(pacs, committees):
    committees = give_name(committees, 'PACShort')
    pacs = clean_super_pac_data(pacs, committees)
    m = make_merge_frame(pacs, committees)
    a = combiner(pacs, m, committees)
    return pd.concat([pd.merge(committees, pacs, on='name'), a])

def label_expenditures(expenditures, pacs, committees):
    super_pacs = id_super_pacs(pacs, committees).CmteID.tolist()
    labels = pd.Series([1 if c in super_pacs else 0 for c in expenditures.spe_id])
    return expenditures.assign(super_pac = labels)

############### scrapin'
def get_super_pacs(year):
    path = 'https://www.opensecrets.org/outsidespending/summ.php?cycle='+str(year)+'&chrt=V&disp=O&type=S'
    html = requests.get(path).text
    super_pacs = pd.read_html(html, flavor='bs4')[1]
    return super_pacs


############ DFs

# make folder org for committees
committees = read_table('../data/cmtes16.txt', 'https://www.opensecrets.org/resources/datadictionary/Data%20Dictionary%20for%20Cmtes.htm')

# get super pacs
super_pacs = get_super_pacs(2016)

# get indepdnent expenditures
independent_expeditures = pd.read_csv('../data/FEC/IndependentExpenditures.csv')

# label and print
spends = pd.merge(pc, independent_expeditures, left_on='CmteID', right_on='spe_id')
