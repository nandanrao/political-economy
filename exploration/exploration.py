import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
import seaborn as sns
from tools import mathify, get_header, read_table

def show_one(self):
    fig = self.get_figure()
    fig.savefig("/tmp/chart.png", dpi=90)
    fig.clear()

matplotlib.artist.Artist.show_one = show_one

#######################################################


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


# check ultorg!!
# remove pac or political action comitt...
# or 2016...
# basically anything on the very end...



# independent_expeditures[independent_expeditures.spe_nam.str.contains('^league', flags = re.IGNORECASE)].groupby('spe_nam').size()


# check for right to rise... multiple!
# committees[[c if c is True else False for c in committees.PACShort.str.contains('^the', flags=re.IGNORECASE)]]

# committees_who_matter = [c for c in committees.CmteID.sort() if c in committee_individuals.CmteID.unique()]
