# datadent-bd-sae

## Summary

Estimating nutrition intervention coverage indicators for administrative 2 districts in Bangladesh using Demographic and Health Survey data.

## Directory structure

This project framework was conceptualized using resources from the [Tilburg Science Hub](https://tilburgsciencehub.com/).

The `make.R` file in the main directory folder sources scripts in the correct order to reproduce the analysis. 

Other scripts are stored in the `src` folder, with sub-folders for each stage of the project pipeline. 

Generated files are those that are created by running the source code on the raw data. They are stored in the `gen` sub-folder. The `gen` sub-folder contains sub-folders that match the pipeline stages.

Sub-folders in `gen` contain additional sub-folders:

-   `input`: any required input files to run this step of the pipeline
-   `temp`: temporary files
-   `output`: stores the final result of the pipeline stage
-   `audit`: quality checks, diagnostic information on the performance of each step in the pipeline


