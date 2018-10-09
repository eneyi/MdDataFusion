from TextQuantification import TextQuantify
from pandas import DataFrame, read_csv

print("Setting Data Directories ...")
datadir = "../../../Database/raw/SocialDimension/microblogs/Microblogs.csv"

data = read_csv(datadir, encoding="latin-1")
data.columns = [i.lower() for i in data.columns]
data["text"] = [str(i) for i in data.text]
text = data.groupby("created_at")["text"].sum()
print("Quantifying Texts")

quantified = [TextQuantify(i).quantify() for i in text]
quantified = DataFrame(quantified)

quantified.to_csv("../../../Database/cleaned/SocialDimension/microblogs.csv")
