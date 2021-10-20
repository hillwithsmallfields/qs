#!/usr/bin/python3

import argparse

def trim_csv(infilename, outfilename=None):
    with open(infilename, 'rb') as instream:
        data = instream.read()
        start = 0
        while data[start] & 0x80:
            start += 1
        if start > 0:
            with open(outfilename or infilename, 'wb') as outstream:
                outstream.write(data[start:])

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", "-o")
    parser.add_argument("filename")
    args = parser.parse_args()
    trim_csv(args.filename, args.filename)

if __name__ == '__main__':
    main()
