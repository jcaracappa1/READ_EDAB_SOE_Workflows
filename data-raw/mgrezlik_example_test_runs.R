# Example of how to test on the Rstudio container 
outputPathDataSets <- "/home/mgrezlik/EDAB_Dev/beet"
outputPath <- "/home/mgrezlik/EDAB_Dev/beet"
inputPathSurvey <- "/home/mgrezlik/EDAB_Dev/beet/surveyNoLengths.rds"
inputPathSpecies <- "/home/mgrezlik/EDAB_Datasets/SOE_species_list_24.rds"
inputPathAlbatross <- "/home/mgrezlik/EDAB_Dev/beet/albatrossData.rds"
inputPathBigelow <- "/home/mgrezlik/EDAB_Dev/beet/bigelowData.rds"
staticPath <-  "/home/mgrezlik/EDAB_Resources/"

source(here::here("data-raw/workflow_species_dist.R"))

# channel <- dbutils::connect_to_database("NEFSC_USERS","mgrezlik")

# rawData <- SOEworkflows::get_survey_data(channel,outputPathDataSets)

indD <- workflow_species_dist(outputPath = outputPath,
                                   inputPathSurvey = inputPathSurvey,
                                   inputPathSpecies = inputPathSpecies,
                                   staticPath = staticPath)

# # compare workflow outputs to ecodata
# max <- indD |> dplyr::mutate(source = 'max')
# ecodata <- ecodata::species_dist |> dplyr::mutate(source = 'ecodata')
# compare <- dplyr::bind_rows(max,ecodata)
# 
# library(ggplot2)
# compare |> 
#   dplyr::filter(Var == 'along-shelf distance') |> 
# ggplot(aes(x=Time, y = Value, color = source))+
#   geom_line()
# 
# compare |> 
#   dplyr::filter(Var == 'depth') |> 
#   ggplot(aes(x=Time, y = Value, color = source))+
#   geom_line()
# 
# compare |> 
#   dplyr::filter(Var == 'distance to coast') |> 
#   ggplot(aes(x=Time, y = Value, color = source))+
#   geom_line()
