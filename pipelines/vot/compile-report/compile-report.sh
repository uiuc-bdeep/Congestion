#!/bin/bash
set -e
set -x

ls -al

tar -xzf analysis/analysis.tar.gz

ls -al

TEMPLATE="repo/pipelines/vot/VOT extended crawler.tex"

pdflatex -jobname=report -output-directory report "$TEMPLATE"
