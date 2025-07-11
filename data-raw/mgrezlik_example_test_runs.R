# Example of how to test on the Rstudio container 
outputPathDataSets <- "/home/mgrezlik/EDAB_Dev/beet"
outputPath <- "/home/mgrezlik/EDAB_Dev/beet"
input_path_commercial_comdat <- "/home/mgrezlik/EDAB_Dev/beet/commercial_comdat.rds"
inputPathSurvey <- "/home/mgrezlik/EDAB_Dev/beet/surveyNoLengths.rds"
inputPathSpecies <- "/home/mgrezlik/EDAB_Datasets/SOE_species_list_24.rds"
# ditching camel case moving forward
input_path_species <- "/home/mgrezlik/EDAB_Datasets/SOE_species_list_24.rds"
inputPathAlbatross <- "/home/mgrezlik/EDAB_Dev/beet/albatrossData.rds"
inputPathBigelow <- "/home/mgrezlik/EDAB_Dev/beet/bigelowData.rds"
staticPath <-  "/home/mgrezlik/EDAB_Resources/"
menhaden_path <- "/home/mgrezlik/EDAB_Datasets/AM Landings by Gaichas Regions 1967-2024.xlsx"
comdat_path <- '/home/mgrezlik/EDAB_Dev/beet/commercial_comdat.rds'

source(here::here("data-raw/workflow_species_dist.R"))

# channel <- dbutils::connect_to_database("NEFSC_USERS","mgrezlik")

# rawData <- SOEworkflows::get_survey_data(channel,outputPathDataSets)

# indD <- workflow_species_distribution(outputPath = outputPath,
#                                    inputPathSurvey = inputPathSurvey,
#                                    inputPathSpecies = inputPathSpecies)


# testing comdat ---------------------

commercial_summary <- create_comdat(
  comdat_path <- '/home/mgrezlik/EDAB_Dev/beet/commercial_comdat.rds',
  report_year = 2025,
  end_year = 2024,
  input_path_species <- "/home/mgrezlik/EDAB_Datasets/SOE_species_list_24.rds",
  menhaden_path <- "/home/mgrezlik/EDAB_Datasets/AM Landings by Gaichas Regions 1967-2024.xlsx"
)

comdat <- get_comdat(
  processed_comdat = commercial_summary, 
  save_for_package = FALSE # Set to FALSE to see the result directly
)

## comparing my comdat outputs to old comdat ---------------
comdat_max <- comdat |> 
                dplyr::mutate(source = 'max')

comdat_ecodata <- ecodata::comdat |> 
                    dplyr::mutate(source = 'ecodata')

comdat_compare <- dplyr::bind_rows(comdat_max, comdat_ecodata)

difference_summary <- comdat_compare |>
  tidyr::pivot_wider(
    names_from = source,
    values_from = Value
  ) |>
  # Calculate both absolute and percentage difference
  dplyr::mutate(
    absolute_diff = max - ecodata,
    percent_diff = (max - ecodata) / ecodata * 100
  )


# Pipeline method has more observations than are found in ecodata (34037 compared to 26141)
# anti join to find observations in comdat_max that are not in comdat_ecodata

missing_from_ecodata <- dplyr::anti_join(
  comdat_max,
  comdat_ecodata,
  by = c("Time", "Var", "EPU")
)
