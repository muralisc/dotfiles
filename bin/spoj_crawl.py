#!/bin/python
import re
from bs4 import BeautifulSoup
import ipdb
import urllib.request
import time
import sys

url = "http://www.spoj.com/users/"+ sys.argv[1] +"/"
req = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
content = urllib.request.urlopen(req).read()
soup = BeautifulSoup(content)
tables= soup.find_all(class_="table-condensed")
problems = re.findall( "\w+", tables[0].text )
for problem in problems:
    url = "http://www.spoj.com/ranks/"+ problem +"/"
    req = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
    content = urllib.request.urlopen(req).read()
    soup = BeautifulSoup(content)
    done= soup.findAll("table",class_="problems" )[0].tbody.findAll("td")[0].text
    print ( problem, done)
    # print(problem, done, "err" , file=sys.stderr)
    # ipdb.set_trace()
