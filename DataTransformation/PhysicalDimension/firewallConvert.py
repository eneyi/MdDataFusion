from os import listdir
from pandas import read_csv, concat
from datetime import datetime as dt

#set raw data directoris
print("Setting Data Directories ...")
fwdir = "../../Database/raw/PhysicalDimension/firewall/"
fwfiles = [i for i in listdir(fwdir) if ".csv" in i]

#read in raw data files
print("Reading in Raw Data Files ....")
fwdata={}
for i in fwfiles:
    fwdata[i] = read_csv(fwdir+i)


fwdata=concat(fwdata, ignore_index=True)
fwdata.columns=["datetime", "syslog", "operation", "message", "protocol", "sourceip", "dstip", "srchost", "dsthost", "srcport", "dstport", "service", "direction","conbuilt", "contorn"]

#format data
print("Formatting Data ...")
inbound = [1 if i == "inbound" else 0 for i in fwdata.direction]
outbound = [1 if i == "outbound" else 0 for i in fwdata.direction]
fwdata["inbound"], fwdata["outbound"] = inbound, outbound
fwdata["datetime"] = [dt.strptime(i, "%d/%b/%Y %H:%M:%S") for i in fwdata.datetime]

print("Successfully Transformed data :-)")


print("Writing Data :-)")
fwdata.to_csv("../../Database/cleaned/PhysicalDimension/firewall/firewall.csv")
print("Written data :-)")
