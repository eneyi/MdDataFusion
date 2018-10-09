##this script gets per minute bitcoin prices for

from bs4 import BeautifulSoup as bs
from requests import get
from fake_useragent import UserAgent
from random import shuffle, randrange
from datetime import date, timedelta
from json import loads, dump
from os import path, mkdir

#selects a random user agent for http requests
def spoofme():
    ua = UserAgent()
    chrome = ua.data['browsers']['chrome'][5:40]
    shuffle(chrome)
    selection = chrome[randrange(1,len(chrome))]
    return selection

#this function prints out all days within a diven start and end date range
#inthe format YYYY-mm-dd
def getDates(start, end):
    #start and end are tuples of date object in format (YYYY, mm, dd)
    date1 = date(start[0], start[1], start[2])
    date2 = date(end[0], end[1], end[2])
    delta = date2 - date1
    dates = []
    for i in range(delta.days+1):
        dd=date1+timedelta(days=i)
        dates.append(dd)
    return dates


##creates string form of get request url
def makeUrl(date1, date2):
    base = "https://bitcoincharts.com/charts/chart.json?m=btcalphaUSD&SubmitButton=Draw&r=2&i=1-min&c=1&s="
    sub1 = "&e="
    sub2 = "&Prev=&Next=&t=S&b=&a1=&m1=10&a2=&m2=25&x=0&i1=&i2=&i3=&i4=&v=1&cv=0&ps=0&l=0&p=0&"
    url = base+str(date1)+sub1+str(date2)+sub2
    return url

#extracts json data from a url
def getPage(url):
    spoofed = "IE"
    ##eliminate incompactible browsers
    while "IE" in spoofed:
        spoofed = spoofme()
    try:
        req = get(url, headers={'user-agent':spoofed}).text
        data = loads(req)
    except:
        data = ""
    return data


def getData(start, end):
    #get all dates within date range
    dates  = getDates(start, end)
    for i in range(1,len(dates)):
        ##create dates
        date1 = dates[i-1]
        date2 = dates[i]

        #make url
        url = makeUrl(date1, date2)

        #create file to write out json data
        filename = str(date1)+"to"+str(date2)

        ##get json data
        data = getPage(url)

        #writeout json data
        baseDir = "../../Database/raw/EconomicDimension/bitData/"
        jsonpath = baseDir+filename +".json"

        if not path.exists(baseDir):
            mkdir(baseDir)

        with open(jsonpath, 'w+') as ff:
            dump(data, ff)
            ff.close()
        print("Written out "+jsonpath)
    return 0

getData([2017,1,1], [2018,3,1])
