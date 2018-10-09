
class TextQuantify:

    def __init__(self, text):
        self.text = text

    #reads in a file and returns a stemmed wordlists from sentences in file
    def readFile(self,filePath):
        from nltk.stem import PorterStemmer
        with open(filePath, "r+") as ff:
            words=ff.readlines()
            words=[i.strip() for i in words]
            words = [PorterStemmer().stem(i) for i in words]
            ff.close()
        return words

    #removes ounctuations from words
    def preClean(self):
        punctuations = '!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~'
        tx = self.text.lower()
        for punc in punctuations.split("."):
            tx = tx.replace(punc, "")
        return tx

    #removes stopwords from sentences
    def removeStopWords(self):
        from nltk.corpus import stopwords
        tx = self.preClean()
        wordlist = tx.split()
        filtered_words = [word for word in wordlist if word not in stopwords.words('english')]
        return " ".join(filtered_words)

    #stems each word in a text document
    def stemmed(self):
        from nltk.stem import PorterStemmer
        tx = self.preClean()
        ss = PorterStemmer()
        wordlist = tx.split()
        st = " ".join([ss.stem(i) for i in wordlist])
        return st

    #returns the total number of individual characters in a text document
    def docLength(self):
        return len(self.text)

    #returns the total number of words in a text document
    def wordCount(self):
        return len(self.text.split())

    #returns a word frequency document i.e the number of times each unique word occurs in a text document
    def word_freq(self):
        wordlist = list(set(self.text.split()))
        frequencies = [self.text.count(word) for word in wordlist]
        return frequencies

    def sentiment(self):
        from nltk.sentiment.vader import SentimentIntensityAnalyzer
        sid = SentimentIntensityAnalyzer()
        scores=sid.polarity_scores(self.text)
        return scores

    #returns the positivity of a text document
    def positivity(self):
        pos = self.sentiment()
        return pos["pos"]

    #returns the negativity of a text document
    def negativity(self):
        neg = self.sentiment()
        return neg["neg"]

    ##returns the neutrality of a text document
    def neutrality(self):
        neu = self.sentiment()
        return neu["neu"]


    def compound(self):
        comp = self.sentiment()
        return comp["compound"]

    ##returns the polarity of a text document
    def polarity(self):
        from textblob import TextBlob as tb
        blob = tb(self.text)
        sentiment = blob.sentiment
        pol = sentiment.polarity
        return pol

    #returns the subjectivity of a text document
    def subjectivity(self):
        from textblob import TextBlob as tb
        blob = tb(self.text)
        sentiment = blob.sentiment
        sub = sentiment.subjectivity
        return sub

    def relatedness(self, wordlist):
        self.text = self.preClean()
        document = self.stemmed()
        wordlist = list(set(wordlist))
        rank = 0
        for word in wordlist:
            rank += document.count(word)
        return rank

    #returns the entropy of a text document
    def entropy(self):
        import scipy as sc
        words = self.text.split()
        counts = [words.count(i) for i in list(set(words))]
        probabilities = [i/len(words) for i in counts]
        #p_data= data.value_counts()/len(data) # calculates the probabilities
        entropy=sc.stats.entropy(probabilities)  # input probabilities to get the entropy
        return entropy

    def entropy_gaussian(self):
        from math import log
        wordlist = self.text.split()
        length = len(wordlist)

        if length <= 1:
            return 0

        probabilities = self.word_freq()
        probabilities = [i/length for i in probabilities if i != 0]
        classes = len(probabilities)
        logs = [log(i) for i in probabilities]
        logs = [logs[i]*probabilities[i] for i in range(0,classes)]
        #scale entropy by log(K)
        return -sum(logs)/log(classes)


    def quantify(self):
        data={}
        data["docLength"] = self.docLength()
        data["wordcount"] = self.wordCount()
        data["positivity"] = self.positivity()
        data["negativity"] = self.negativity()
        data["neutrality"] = self.neutrality()
        data["compound"] = self.compound()
        data["polarity"] = self.polarity()
        data["subjectivity"] = self.subjectivity()
        data["entropy"] = self.entropy()
        #data["entropy_gaussian"] = self.entropy_gaussian()
        cyber_words = self.readFile("LexiconBuilding/lexicons/cyberwordlist.txt")
        flu_words = self.readFile("LexiconBuilding/lexicons/fluwordlist.txt")
        fire_words = self.readFile("LexiconBuilding/lexicons/firewordlist.txt")
        weaponization = self.readFile("LexiconBuilding/lexicons/weaponization.txt")
        data["cyber_rel"] = self.relatedness(cyber_words)
        data["flu_rel"] = self.relatedness(flu_words)
        data["fire_rel"] = self.relatedness(fire_words)
        data["weaponization"] = self.relatedness(weaponization)

        return data
