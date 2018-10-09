from bs4 import BeautifulSoup as bs
from fake_useragent import UserAgent
from requests import get
from random import shuffle, randrange


# this function creates a fake user client on each use request

def spoofme():
    ua = UserAgent()
    chrome = ua.data['browsers']['chrome'][5:40]
    shuffle(chrome)
    pick = chrome[randrange(1, len(chrome))]
    return pick

# this function accesses a url and return a soup object


def getPage(url):

    try:
        req = get(url, headers={'user-agent': spoofme()}).content
        soup = bs(req)
    except:
        soup = ""
    return soup


def getPostsLinks(soup):
    links = soup.find_all("a", "post-card-content-link")
    links = ["https://www.cyberwarnews.info"+i['href'] for i in links]
    return links

def getPostText(postLink):
    soup = getPage(postLink)
    try:
        title = soup.find("h1").getText()
    except:
        title = "no title"

    textPanel = soup.find("section", {"class":"post-full-content"})
    texts = textPanel.find_all("p")
    text = "".join([i.getText() for i in texts])
    return [title, text]