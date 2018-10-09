from utilities import createDictionary
from pandas import DataFrame

print("Reading in exploits db data ...")
##read in exploitsdb data
with open("../DataExtraction/db/exploit_db.csv", "r+") as ff:
    exploits = ff.readlines()
    exploits = [row.split("\t") for row in exploits]
    exploits = [row for row in exploits if len(row) > 1]
    exploits=" ".join([j for k in exploits for j in k])
    ff.close()

print("Reading in google hacking database db data ...")
#read in ghdb
with open("../DataExtraction/db/ghdb.csv", "r+") as ff:
    ghdb = ff.readlines()
    ghdb=[row.split("\t") for row in ghdb]
    ghdb = [row for row in ghdb if len(row) > 1]
    ghdb = DataFrame(ghdb)
    ghdb = [i for i in ghdb[3]]
    ff.close()

print("Reading in exploits Vulnerabilities data ...")
#read in cve descriptions
with open("../DataExtraction/db/cveDesriptions.txt", "r+") as ff:
    description = ff.read()
    ff.close()

print("Creating Wordlist")
#create word dictionary
cvedict=createDictionary(description)
exdict = createDictionary(exploits)
ghdbdict = createDictionary(ghdb)


cve = [i[0] for i in cvedict]
exp = [i[0] for i in exdict]
ghd = [i[0] for i in ghdbdict]

print("Writing out wordlist ...")
#write out data
with open("../lexicons/weaponization.txt","w+") as ff:
    for i in list(set(cve+exp+ghd)):
        ff.write("%s\n" % i)
