from pandas import DataFrame, concat
from os import listdir, chdir
from datetime import datetime as dt
from re import findall

#specify data directories
idspath="../../Database/raw/PhysicalDimension/ids/"
idsfiles = [i for i in listdir(idspath) if ".txt" in i]
idsdata=[]

#read in txt files
for i in idsfiles:
    with open(idspath+"/"+i, "r+") as ff:
        lines = ff.read()
        lines = lines.split("\n\n")
        idsdata.append(lines)
        print("Read in "+ i)
    ff.close()

print("Raw Data Read Successful !! :-)")
print("Extracting lines ....")

#extract lines from txt files
idsdata2 = []
for i in idsdata:
    sub = []
    for j in i:
        ss = j.split("\n")
        idsdata2.append(ss)

print("Line extration successful !! :-)")
print("Extracting Data Fields .... ")

## Extract data fields from lines
#extract alert text
alert=[item.replace("[**]", "").strip() for sublist in idsdata2 for item in sublist if "[**]" in item]
alert = [i.split()[1::] for i in alert]

#extract alert description
description = [" ".join(i[1:len(i)]) for i in alert]

#extract alert classification
classification = [i[0].replace("(","").replace(")", "").replace(":","").strip() for i in alert]
classification = DataFrame(classification)

#extract alert protocol
proto=[item for sublist in idsdata2 for item in sublist if "PROTO" in item]
proto = [i.split() for i in proto]

#extract alert priority level
priority=[item for sublist in idsdata2 for item in sublist if "Priority" in item]
priority = [findall(r"\d+",i)[0] for i in priority]

#extract alert ip data
ipsda=[item for sublist in idsdata2 for item in sublist if "->" in item]
ipsda = [i.split() for i in ipsda]


#format data
print("Data fields Extraction Successful !! :-)")
print("Formatting Data")
alert=DataFrame(alert)
ipsda =DataFrame(ipsda)
priority=DataFrame(priority)
proto = DataFrame(proto)
classification = DataFrame(classification)
description = DataFrame(description)


print("Data formatting Successful !! :-)")
print("Combining Data...")

#combine data
data = concat([ipsda,proto,priority,classification,description], axis=1)

print("Formatting Columns")
#extract columns
data.columns = [i for i in range(0,len(data.columns))]
data = data.iloc[:,[0,1,3,11,12,13]]
cols = ["datetime", "srcip","dstip", "priority","alert", "description"]
data.columns=cols
data=data.dropna()

print("Formatting Date .....")
data['datetime'] = [dt.strptime(i.split(".")[0]+"-2011", "%m/%d-%H:%M:%S-%Y") for i in data.datetime]

print("Date Formatting Successful !!! :-)")

data.to_csv("../../Database/cleaned/PhysicalDimension/ids/ids.csv")
