from utilities import getPage

pages = [i for i in range(1,91)]
site="https://www.exploit-db.com/google-hacking-database/"
base = "https://www.exploit-db.com/google-hacking-database/?action=search&ghdb_search_page="


def getGHDBTable(url):
    soup = getPage(url)
    table_soup = soup.find("table", {"class":"category-list"})
    cells = [row.findAll("td") for row in table_soup.findAll("tr")]
    for cell in cells:
        if len(cell)==3:
            date = cell[0].getText().strip()


            descriptionLink = cell[1].find("a")
            if descriptionLink:
                descriptionLink = descriptionLink['href']
            else:
                descriptionLink = ""


            category = cell[2].find("a")
            if category:
                category = category.getText().strip()
            else:
                category = ""


            summary = cell[2].find("p")
            if summary:
                summary = summary.getText().strip()
            else:
                summary= ""

            print("{0}\t{1}\t{2}\t{3}".format(date,descriptionLink,category,summary))


    return 0

if __name__ == "__main__":
    for page in pages:
        url = base+str(page)
        getGHDBTable(url)