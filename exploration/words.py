from sklearn.feature_extraction import text
from sklearn.metrics import pairwise
from sklearn.cluster import KMeans, DBSCAN
from glove import Glove

model = Glove.load_stanford('../data/word_embeddings/glove.6B.100d.txt')

independent_expeditures = pd.read_csv('../data/FEC/IndependentExpenditures.csv')

def tokenize(l):
    lower = lambda x: map(lambda y: y.lower(), x)
    tokenizer = text.CountVectorizer().build_analyzer()
    costs = map(tokenizer, l)
    return map(lower, costs)

costs = tokenize(independent_expeditures.pur.unique())
c = map(lambda x: model.transform_paragraph(x, ignore_missing = True), costs)
# c = filter(lambda x: 'nan' != x[0], c)

distances = pairwise.pairwise_distances(c[1:300], metric = 'euclidean')
kmeans = KMeans(n_clusters=10, random_state=0).fit(distances)
df = pd.DataFrame({'purpose': independent_expeditures.pur.unique()[1:300], 'label' : kmeans.labels_})


def get_top(zipped, n):
    s = [sorted(z, key = lambda x: x[0], reverse = True) for z in zipped]
    return [[y[1] for y in x[0:n]] for x in s]

def get_keywords(arr):
    fit = text.CountVectorizer(stop_words = 'english').fit(arr)
    csr = fit.transform(arr)
    popularity = csr.sum(0)
    by_pop = [c.multiply(popularity).tolil().data[0] for c in csr]
    tokens = fit.inverse_transform(csr)
    zipped = [zip(x,y) for x,y in zip(by_pop, tokens)]
    return get_top(zipped,3)



x = map(lambda x: model.transform_paragraph(x, ignore_missing = True), get_keywords(independent_expeditures.pur.unique()))

distances = pairwise.pairwise_distances(x[0:300], metric = 'euclidean')
kmeans = KMeans(n_clusters=10, random_state=0).fit(distances)
df = pd.DataFrame({'purpose': independent_expeditures.pur.unique()[0:300], 'label' : kmeans.labels_})
