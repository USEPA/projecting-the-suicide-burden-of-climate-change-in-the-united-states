#!/bin/bash

export PROJECT_LOC="/Users/abelova/ICF/EPA Climate and Mental Health - General/03_Analysis Data and Manuscript/modeling/ccmh-src"
export DATA_LOC="/Users/abelova/ICF/EPA Climate and Mental Health - General/03_Analysis Data and Manuscript/modeling/ccmh-data"
export WORKBOOK_LOC="/Users/abelova/ICF/EPA Climate and Mental Health - General/03_Analysis Data and Manuscript/modeling/ccmh-workbooks"

echo PROJECT_LOC=\"$PROJECT_LOC\" > .Renviron
echo DATA_LOC=\"$DATA_LOC\" >> .Renviron
echo WORKBOOK_LOC=\"$WORKBOOK_LOC\" >> .Renviron
