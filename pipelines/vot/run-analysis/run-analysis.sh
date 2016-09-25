#!/bin/bash
set -e

tar -xzf data-stores/stores.tar.gz
tar -xzf data-stream/stream.tar.gz
ls -l

Rscript "repo/pipelines/vot/Intraday VOT - limited survey-crawler diff - depart time 4 hours range.R"

tar -czf analysis.tar.gz analysis
cp analysis.tar.gz analysis/

ls -l
ls -l analysis
