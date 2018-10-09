from utilities import getPostText
from re import search




#read in postlinks
with open("postlinks.txt", "r+") as ff:
    lines = ff.readlines()
    lines=[i.strip() for i in lines]
    ff.close()
for i in range(0, len(lines)):
    try:

        text = getPostText(lines[i])
        date = search(r'\d{4}/\d{2}/\d{2}', lines[i]).group()
        outfile = "../db/cwncorpus/" + str(i) + ".txt"

        writable = text[0] + "\n" + str(date) + "\n" + text[1]
        with open(outfile, "w+") as ww:
            ww.write(writable)
            ww.close()

        print("Written " + outfile)

    except Exception as e:
        print(e)
        print("Error at "+outfile)
        pass
