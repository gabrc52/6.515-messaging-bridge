#!/bin/bash

# There are two options:
pandoc -t revealjs -s -o index.html slides.md -V revealjs-url=https://unpkg.com/reveal.js -V theme=sky --slide-level=2
pandoc -t beamer slides.md -V theme:Warsaw -o slides.pdf
