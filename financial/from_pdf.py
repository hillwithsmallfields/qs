#!/usr/bin/python3

import glob
import os
import re

pattern = re.compile("SAVINGS ACCOUNT STATEMENT 23673144([^.]*).pdf")

def main():
    for pdf in glob.glob("SAVINGS ACCOUNT STATEMENT 23673144*.pdf"):
        numberstring = pattern.match(pdf).group(1).lstrip('(').rstrip(')')
        if numberstring == "":
            numberstring = "0"
        os.system("pdf2txt -o statement-%04d.txt '%s'" % (int(numberstring), pdf))

if __name__ == '__main__':
    main()
