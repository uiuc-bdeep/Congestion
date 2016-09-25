#!/bin/bash
set -e

tar -xzf resource-data-stores/stores.tar.gz
tar -xzf resource-data-stream/stream.tar.gz
ls -l

Rscript "resource-repo/Intraday VOT - limited survey-crawler diff - depart time 4 hours range.R"

tar -czf analysis.tar.gz analysis
cp analysis.tar.gz analysis/

ls -l
ls -l analysis
