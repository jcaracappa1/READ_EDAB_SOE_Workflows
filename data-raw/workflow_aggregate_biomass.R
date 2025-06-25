#' Creates aggregate_biomass indicator for SOE
#' 
#' @param inputPathSurvey Character string. Full path to the survdat data pull rds file
#' @param inputPathSpecies Character string. Full path to the species list data pull rds file
#' @param outputPath Character string. Path to folder where data pull should be saved
#' 
#' @return Nothing. rds file exported
#' 
#' @section Dependencies:
#' 
#' This assumes that the survey data has been pulled and resides in the path `inputPathSurvey` and that
#' the species data resides in `inputPathSpecies`
#' 
#' @examples
#' \dontrun{
#'   outputPath <- here::here()
#'   inputPathSurvey <- here::here("surveyNoLengths.rds")
#'   inputPathSpecies <- "/home/<user>/EDAB_Datasets/SOE_species_list_24.rds"
#'   workflow_aggregate_biomass(outputPath,inputPathSurvey,inputPathSpecies)
#' }
#' 

workflow_aggregate_biomass <- function(outputPath,inputPathSurvey,inputPathSpecies) {

  # Assumes that survey data has been pulled and is located in inputPathSurvey
  #get_survey_data(channel,outputPath = outputPath)
  
  # Add check to skip running workflow if data not present
  if(file.exists(inputPathSpecies) && file.exists(inputPathSurvey) && (!is.null(outputPath))) {
  
    indicatorData <- SOEworkflows::create_aggregate_biomass(inputPathSurvey = inputPathSurvey,
                             inputPathSpecies = inputPathSpecies)
    
    # Write data to file
    saveRDS(indicatorData,paste0(outputPath,"/aggregate_biomass.rds"))
    
  } else {
    # 
    message("One or more of the input files are not present in the location specified")
  }
}