# Example of how to test on the Rstudio container 
outputPathDataSets <- "/home/EDAB_Dev/beet"
outputPath <- "/home/EDAB_Dev/beet"
inputPathSurvey <- "/home/EDAB_Dev/beet/surveyNoLengths.rds"
inputPathSpecies <- "/home/EDAB_Datasets/SOE_species_list_24.rds"
inputPathAlbatross <- "/home/EDAB_Dev/beet/albatrossData.rds"
inputPathBigelow <- "/home/EDAB_Dev/beet/bigelowData.rds"

source(here::here("data-raw/workflow_species_dist.R"))

channel <- dbutils::connect_to_database("NEFSC_USERS","mgrezlik")

rawData <- SOEworkflows::get_survey_data(channel,outputPathDataSets)

indD <- workflow_species_dist.R(outputPath = outputPath,
                                   inputPathSurvey = inputPathSurvey,
                                   inputPathSpecies = inputPathSpecies)

