# Example of how to test on the Rstudio container 
# DO NOT SOURCE THIS SCRIPT. IT will not run
#
# Please make a copy and edit for your own purposes

# suite of paths to input and output files
outputPathDataSets <- "/home/abeet/EDAB_Dev/beet/"
outputPath <- "/home/abeet/EDAB_Dev/beet/"
inputPathSurvey <- "/home/abeet/EDAB_Dev/beet/surveyNoLengths.rds"
inputPathSpecies <- "/home/abeet/EDAB_Datasets/SOE_species_list_24.rds"
inputPathAlbatross <- "/home/abeet/EDAB_Dev/beet/albatrossData.rds"
inputPathBigelow <- "/home/abeet/EDAB_Dev/beet/bigelowData.rds"
inputPathBennet <- "/home/abeet/EDAB_Dev/beet/commercial_bennet.rds"
inputPathComdat <- "/home/abeet/EDAB_Dev/beet/commercial_comdat.rds"
menhadenPath <- "/home/abeet/EDAB_Dev/grezlik/menhadenEOF.rds"

# source workflow functions from data-raw since they are not accessible from the package installation
source(here::here("data-raw/workflow_aggregate_biomass.R"))
source(here::here("data-raw/workflow_survey_shannon.R"))
source(here::here("data-raw/workflow_bennet.R"))
source(here::here("data-raw/workflow_pull_survey_data.R"))
source(here::here("data-raw/workflow_pull_commercial_data.R"))
source(here::here("data-raw/workflow_comdat.R"))


## Connects to the data base.
# This is only needed to pull data from survey and commercial dbs
# you'll need to add the server and your user id
channel <- dbutils::connect_to_database("server","user")
# pull and write survey data
rawData <- SOEworkflows::get_survey_data(channel,outputPathDataSets)
# pull and write commercial data
commercial_data <- SOEworkflows::get_commercial_data(channel,outputPathDataSets)


# workflows for pulling data
survey <- workflow_pull_survey_data(channel,outputPath = outputPathDataSets)
commercial <- workflow_pull_commercial_data(channel,outputPath = outputPathDataSets)

# calculate the bennet index
indicator_bennet <- workflow_bennet(inputPathBennet = inputPathBennet,
                                   inputPathSpecies = inputPathSpecies,
                                   outputPath = outputPath)

# calculate the aggregate biomass indicator and write it to desired location
indicator_aggegegate_biomass <- workflow_aggregate_biomass(outputPath = outputPath,
                                   inputPathSurvey = inputPathSurvey,
                                   inputPathSpecies = inputPathSpecies)

# calculate the survey_shannon indicator and write it to desired location
indicator_survey_shannon <- workflow_survey_shannon(outputPath = outputPath,
                                inputPathBigelow = inputPathBigelow,
                                inputPathAlbatross = inputPathAlbatross)

# comdat workflow
indicator_comdat <- workflow_comdat(comdat_path = inputPathComdat,
                input_path_species = inputPathSpecies,
                menhaden_path = menhadenPath,
                outputPathDataSets = outputPath)
