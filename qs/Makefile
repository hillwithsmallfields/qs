#!/usr/bin/make

all: prepare_financisto README.pdf

prepare_financisto: prepare_financisto.c
	gcc -o prepare_financisto prepare_financisto.c

README.pdf: README.md
	pandoc -o README.pdf README.md
