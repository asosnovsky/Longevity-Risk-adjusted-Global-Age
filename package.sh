#!/bin/bash

set -e

mkdir -p build
rm -r build/*
mkdir build/tables

cp -r images build/images

mkdir -p build/tables/MABA_Tables_Revised
pdflatex -output-directory="reports/Revised Tables/" "reports/Revised Tables/MABA_Tables_Revised_Ariel.tex"
cp "reports/Revised Tables/MABA_Tables_Revised_Ariel.tex" build/tables/MABA_Tables_Revised
cp "reports/Revised Tables/MABA_Tables_Revised_Ariel.pdf" build/tables/MABA_Tables_Revised

mkdir -p build/tables/Tables-4
pdflatex -output-directory="reports/Historical Modelling/" "reports/Historical Modelling/Historic.tex"
cp "reports/Historical Modelling/Historic.tex" build/tables/Tables-4
cp "reports/Historical Modelling/Historic.pdf" build/tables/Tables-4

zip -r -9 -m package_$(date +%Y_%m_%d_%H_%M).zip build/*
