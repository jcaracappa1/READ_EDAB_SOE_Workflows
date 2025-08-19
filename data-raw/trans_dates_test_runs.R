# Example of how to test on the Rstudio container 

# suite of paths to input and output files
outputPath <- "/home/bbeltz/EDAB_Dev/beltz/"
inputPath <- "/home/bbeltz/EDAB_Dev/beltz/TS_SHP_adv rep MAB GOM GBK NES SCSPoly.csv"

# source workflow functions from data-raw since they are not accessible from the package installation
source(here::here("data-raw/workflow_trans_dates.R"))

# calculate the transition dates indicator
indicator_trans_dates <- workflow_trans_dates(inputPath = inputPath,
                                              outputPath = outputPath)
