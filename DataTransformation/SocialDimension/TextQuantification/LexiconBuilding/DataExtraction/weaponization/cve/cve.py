from utilities import getPage
from time import sleep


years = [i for i in range(2000,2019)]
site = "http://www.cvedetails.com"
base_url = "https://www.cvedetails.com/browse-by-date.php"



#scrape data from cve table
def getPages(year_url):
    year_page = getPage(year_url)
    pagination = year_page.find("div", {"class":"paging", "id":"pagingb"})
    pagination = [site+i['href'] for i in pagination.findAll("a")]
    return pagination


def getDescriptions(page):
    page_soup = getPage(page)
    table_soup = page_soup.find("table", {"id":"vulnslisttable", "class":"searchresults"})
    rows = [row .findAll("td") for row in table_soup.findAll("tr")]
    #descriptions = [row for row in rows if row.has_attr("id")]
    rows = [row for row in rows if len(row)==1]
    return rows

#get table data
def getVulnerabilitiesTable(page):
    page_soup = getPage(page)
    table_soup = page_soup.find("table", {"id":"vulnslisttable", "class":"searchresults"})
    rows = [row .findAll("td") for row in table_soup.findAll("tr")]
    #descriptions = [row for row in rows if row.has_attr("id")]
    rows = [row for row in rows if len(row)>1]
    return rows


#get year url
def makeUrl(year):
    year_url = "https://www.cvedetails.com/vulnerability-list/year-%s/vulnerabilities.html" % year
    return year_url


if __name__ =="__main__":
    for year in years:
        current_year = makeUrl(str(year))
        pagination = getPages(current_year)
        for page in pagination:
            descriptions = getDescriptions(page)
            for i in descriptions:
                print(i[0].getText().strip())
        sleep(100)

