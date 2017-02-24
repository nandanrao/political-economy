import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
import seaborn as sns

def show_one(self):
    fig = self.get_figure()
    fig.savefig("/tmp/chart.png", dpi=90)
    fig.clear()

matplotlib.artist.Artist.show_one = show_one

#######################################################
import requests
from bs4 import BeautifulSoup

def get_header(url):
    soup = BeautifulSoup(requests.get(url).text, 'html.parser')
    return [t.td.text.strip() for t in soup.find_all('table')[0].find_all('tr')[1:]]

def read_table(path, url = None, **kwargs):
    if url:
        headers = get_header(url)
    names = headers if url else None
    return pd.read_csv(path, sep = ",", quotechar="|", header = None, names = names, **kwargs)



pac_to_pac = read_table('../data/pac_other16.txt', 'https://www.opensecrets.org/resources/datadictionary/Data%20Dictionary%20PAC%20to%20PAC%20Data.htm')

pac_to_candidate = read_table('../data/pacs16.txt', 'https://www.opensecrets.org/resources/datadictionary/Data%20Dictionary%20for%20PAC%20to%20Cands%20Data.htm')

individual = read_table('../data/indivs16.txt', 'https://www.opensecrets.org/resources/datadictionary/Data%20Dictionary%20for%20Individual%20Contribution%20Data.htm', nrows = 3000000)

committees = read_table('../data/cmtes16.txt', 'https://www.opensecrets.org/resources/datadictionary/Data%20Dictionary%20for%20Cmtes.htm')



sns.distplot(individual[individual.Amount > 100000].Amount, kde = False, rug = True).show_one()


committee_individuals = pd.merge(individual, committees, on = 'CmteID')


committee_individuals[committee_individuals.Amount > 33400].PACShort.value_counts()


super_pacs_names = committee_individuals[committee_individuals.Amount > 33400].PACShort.unique()


super_pac_ids = committee_individuals[committee_individuals.Amount > 33400].CmteID


ospc = committee_individuals[committee_individuals.CmteID.isin(super_pac_ids)]

ospc[ospc.Amount < 1000][ospc.Amount > 0]

ospc[ospc.Amount > 1000]


maybe_reg_pacs = [p for p in committee_individuals[committee_individuals.Amount <= 5000].PACShort.unique() if p not in super_pacs]


committee_individuals[committee_individuals.Amount == 5000].groupby('ContribID').sum().sort_values('Amount')


pac_contributions = pd.merge(committees, pac_to_candidate, left_on='CmteID', right_on='PACID')

pac_contributions = pd.merge(committees, pac_to_candidate, left_on='CmteID', right_on='PACID')

pac_contributions.sort_values(['Amount'], ascending= False)[['Amount', 'PACShort']]

pac_contributions[['Amount', 'PACShort']].groupby('PACShort').sum().sort_values('Amount', ascending = False)


############

html = requests.get('https://www.opensecrets.org/outsidespending/summ.php?chrt=V&type=S').text

super_pacs = pd.read_html(html, flavor='bs4')[1]

super_pac_names = super_pacs.Group.unique()

committee_individuals[committee_individuals.PACShort.isin(super_pac_names)].PACShort.unique().size


##########/?????
from re import sub
from decimal import Decimal

def mathify(s):
    return Decimal(sub(r'[^\d.]', '', s))

super_pacs_that_matter = super_pacs[super_pacs['Total Raised'].map(mathify) > 10]

super_pacs[super_pacs['IndependentExpenditures'].map(mathify) > 10]


################
from sklearn.feature_extraction import text
from sklearn.cluster import KMeans, DBSCAN


independent_expeditures = pd.read_csv('../data/FEC/IndependentExpenditures.csv')

def tokenize(l):
    lower = lambda x: map(lambda y: y.lower(), x)
    tokenizer = text.CountVectorizer().build_tokenizer()
    costs = map(tokenizer, l)
    return map(lower, costs)

costs = tokenize(independent_expeditures.pur.unique())
c = map(lambda x: model.transform_paragraph(x, ignore_missing = True), costs)
# c = filter(lambda x: 'nan' != x[0], c)

distances = pairwise.pairwise_distances(c[320:330], metric = 'cosine')
kmeans = KMeans(n_clusters=10, random_state=0).fit(distances)
df = pd.DataFrame({'purpose': independent_expeditures.pur.unique(), 'label' : kmeans.labels_})
