#!/bin/bash


mkdir -p build
rm -r build/*

set -e
mkdir build/tables
mkdir -p build/images/{postscript,jpg}

#cp -r images/figure-{3..5}/**/*.jpg build/images/jpg
# cp -r images/figure-{3..5}/**/*.ps build/images/postscript

for subf in ./reports/**/*.tex; do
    pdflatex -output-directory=./build/tables $subf
    cp $subf ./build/tables/$(basename $subf)
done

rm build/tables/*aux
rm build/tables/*log

#zip -r -9 package_$(date +%Y_%m_%d_%H_%M).zip build/*
