#' Get survey data set to store somewhere
#'
#' @description
#' Pulls survey data in format required for indicator generation
#'
#' @param channel an Object inherited from DBIConnection-class. This object is used to connect to communicate with the database engine.
#' @param outputPath Character string. Path to folder where data pull should be saved (Defaul = NULL). 
#' If not NULL the pull will be saved to the `outputPath` with names (`lbatrossData.rds`,`bigelowData.rds`,`surveyNoLengths.rds`)
#'
#'@return A list of survey data pulls
#'
#'@export

get_survey_data <- function(channel,outputPath=NULL) {
  
  end.year <- format(Sys.Date(),"%Y")
  # Get the survey data for aggregate biomass
  survey1 <- survdat::get_survdat_data(channel, getLengths = F)
  
  # Get the survey data for exp_n, survey_shannon
  # Grab Albatross time series (< 2009)
  al.data <- survdat::get_survdat_data(channel, filterByYear = 1963:2008,
                                       getLengths = F)
  
  #Grab data without Bigelow conversions (>= 2009)
  big.data <- survdat::get_survdat_data(channel, filterByYear = 2009:end.year,
                                        conversion.factor = F, getLengths = F)
  
  # create a list of three datasets
  survey_data <- list(
    survey1 = survey1,
    al.data = al.data,
    big.data = big.data
  )
  
  # May need to save these to a specific location
  # Return the data
  if (!is.null(outputPath)) {
    saveRDS(al.data,paste0(outputPath,"/albatrossData.rds"))
    saveRDS(big.data,paste0(outputPath,"/bigelowData.rds"))
    saveRDS(survey1,paste0(outputPath,"/surveyNoLengths.rds"))
  }
  
  return(survey_data)
}
