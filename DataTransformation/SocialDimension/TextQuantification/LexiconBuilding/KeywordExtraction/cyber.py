from utilities import createDictionary
from os import listdir
from utilities import clean, word_freq, PMI

print("Setting Data Directories")
#setting raw data directories
datadir = "../DataExtraction/db/cwncorpus/"
files = listdir(datadir)

data = []

print("Reading in Data Files")
#reading in raw data files
for i in files:
    with open(datadir+i,"r+", encoding="latin-1") as ff:
        data.append(ff.read())
    ff.close()

print("Creating Wordlists")
data = " ".join(data)

print(word_freq(clean(data)))
