from utilities import getPostsLinks, getPage

base="https://www.cyberwarnews.info"


soup1 = getPage(base)
for i in range(1,85):
    url = base+"/page/"+str(i)+"/"
    try:
        page = getPage(url)
        linked = getPostsLinks(page)
        for j in linked:
            print(j)
    except:
        pass
