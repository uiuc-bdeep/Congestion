#!/bin/bash
set -e
set -x

ls -al

tar -xzf resource-analysis/analysis.tar.gz

ls -al

TEMPLATE="resource-repo/VOT extended crawler.tex"

pdflatex -jobname=report -output-directory report "$TEMPLATE"
