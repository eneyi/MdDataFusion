from pandas import read_json, DataFrame
from os import listdir
from json import dump, load
from datetime import datetime as dt

print("Setting Data Directory...")
datadir = "../../Database/raw/EconomicDimension/bitData/"
files = listdir(datadir)
data = []
print("Reading in data files...")

for i in files:
    with open(datadir+i, "r+") as ff:
        dd = load(ff)
        for j in dd:
            data.append(j)

print("Formatting data ....")
data = DataFrame(data)

print("Dropping duplicate values ....")
data = data.drop_duplicates()

data.columns=["datetime", "open", "high","low","close","volume_btc", "volume_usd", "weighted_price"]

print("Formatting Dates")
data["datetime"] = [dt.fromtimestamp(i) for i in data.datetime]


print("Writing data")
data.to_csv("../../Database/cleaned/EconomicDimension/bitcoins.csv")
print(data)
