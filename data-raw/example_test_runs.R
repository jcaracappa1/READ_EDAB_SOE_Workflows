# Example of how to test on the Rstudio container 
outputPathDataSets <- "/home/abeet/EDAB_Dev/beet"
outputPath <- "/home/abeet/EDAB_Dev/beet"
inputPathSurvey <- "/home/abeet/EDAB_Dev/beet/surveyNoLengths.rds"
inputPathSpecies <- "/home/abeet/EDAB_Datasets/SOE_species_list_24.rds"
inputPathAlbatross <- "/home/abeet/EDAB_Dev/beet/albatrossData.rds"
inputPathBigelow <- "/home/abeet/EDAB_Dev/beet/bigelowData.rds"

source(here::here("data-raw/workflow_aggregate_biomass.R"))
source(here::here("data-raw/workflow_survey_shannon.R"))

channel <- dbutils::connect_to_database("NEFSC_USERS","abeet")

rawData <- SOEworkflows::get_survey_data(channel,outputPathDataSets)

indD <- workflow_aggregate_biomass(outputPath = outputPath,
                                   inputPathSurvey = inputPathSurvey,
                                   inputPathSpecies = inputPathSpecies)


indD <- workflow_survey_shannon(outputPath = outputPath,
                                inputPathBigelow = inputPathBigelow,
                                inputPathAlbatross = inputPathAlbatross)
