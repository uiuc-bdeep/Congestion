#!/bin/bash
set -e

tar -xzf data-stores/stores.tar.gz
tar -xzf data-stream/stream.tar.gz
ls -l

export BDEEP_PROD=1
export BDEEP_R_ROOT=$PWD

SCRIPTS=$PWD/repo/vot

env

cp $SCRIPTS/environment.R .

Rscript "$SCRIPTS/1.0 add car.time 0 to extended crawler.R"
Rscript "$SCRIPTS/1.1 long data files.R"
Rscript "$SCRIPTS/1.2 vot estimation.R"
Rscript "$SCRIPTS/1.3 vot plot.R"
Rscript "$SCRIPTS/1.4 assign vot to trips.R"
Rscript "$SCRIPTS/1.5 week aggregation.R"

cp -r views analysis/
cp "intermediate/vot/travel time and vot - weekly.csv" analysis/weekly-vot.csv

tar -czf analysis.tar.gz analysis
cp analysis.tar.gz analysis/

ls -l
ls -l analysis
