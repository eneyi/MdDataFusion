from numpy import log
from nltk import bigrams
import pandas as pd
from nltk.corpus import stopwords
from itertools import combinations
from collections import defaultdict
from pandas import DataFrame
from nltk.stem import PorterStemmer


ps=PorterStemmer()
stops = ['some','come','best','often','some','age','day','want', 'call','are','within','way', 'doctor','this','take','one','read','short','see','usual','eat','prevent','run','help','earlier','use', 'people','may','even','everyone','get','sign','take','symtom','you','includes','include','also','reduce','cause','take','get','worse','tea','drink','know','the','this','that']
stops=stopwords.words("english")+stops

def clean(corpus):
    corpus = " ".join([ps.stem(i) for i in corpus.split() if i.isalpha() and i not in stops and len(i) > 3])
    corpus=corpus.lower().strip()
    return corpus

def word_freq(corpus):
    words = list(set(corpus.lower().split()))
    words = [word for word in words if word not in stops]
    freq = [(word, corpus.count(word)) for word in words]
    return [i for i in freq if i[1] > 1 and len(i[0])>2]

def co_occurrence_matrix(corpus):
    from collections import defaultdict
    corpus = " ".join([i for i in corpus.split() if i.isalpha() and i not in stopwords.words("english") and len(i) > 1])
    com = defaultdict(lambda : defaultdict(int))
    vocab = list(corpus.split(" "))
    #vocab = list(vocab)

    for i in range(len(vocab)-1):
        for j in range(i+1, len(vocab)):
            w1, w2 = sorted([vocab[i], vocab[j]])
            if w1 != w2:
                com[w1][w2] += 1
            else:
                com[w1][w2]=1
    # Key:Value = Word:Index
    vocab_to_index = { word:i for i, word in enumerate(vocab) }

    # Create bigrams from all words in corpus
    bi_grams = list(bigrams(vocab))
    return com


def createDictionary(corpus):
    wordlist = "".join((char if char.isalpha() else " ") for char in corpus).split()
    wordlist = [word.lower() for word in wordlist if len(word) > 1]
    wordlist = [word for word in wordlist if word not in stopwords.words("english")]
    wordfreq = [(word, wordlist.count(word)) for word in list(set(wordlist))]
    wordfreq = sorted(wordfreq, key=lambda freq: freq[1], reverse=True)
    wordfreq = [i for i in wordfreq if i[1] > 5]
    return wordfreq

def PMI(corpus):
    ############pre cleaning###################
    #sentence tokenize
    texts = [" ".join([ps.stem(j) for j in i.split() if j not in stopwords.words("english") and len(i) > 1]) for i in corpus.split("\n")]
    #word tokenize
    texts = [clean(i).split() for i in texts]
    texts = [i for i in texts if len(i)>0]


    cx,cxy = {},{}
    for text in texts:
        #create a word frequency
        for x in text:
            if x in cx.keys():
                cx[x] += 1
            else:
                cx[x] = 1
        #create a word combination count from each sentence
        for x, y in map(sorted, combinations(text, 2)):
            if (x,y) in cxy.keys():
                cxy[(x, y)] += 1
            else:
                cxy[(x, y)] = 1

    min_count, max_count = (1 / 1000) * len(texts), (1/200) * len(corpus)

    for x in list(cx.keys()):
        if cx[x] < min_count or cx[x] > max_count:
            del cx[x]

    x2i, i2x = {}, {}
    for i, x in enumerate(cx.keys()):
        x2i[x], i2x[i] = i,x


    sx,sxy = sum(cx.values()),sum(cxy.values())

    pmi_samples, data =  defaultdict(lambda : defaultdict(int)), []

    for (x, y), n in cxy.items():
        data.append(log((n / sxy) / (cx[x] / sx) / (cx[y] / sx)))
        pmi_samples[x][y] = data[-1]

    #return pd.DataFrame(pmi_samples).fillna(0).sum().sort_values(ascending=False)
    return DataFrame(pmi_samples).sum().sort_values(ascending=False)
