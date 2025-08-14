#' Get survey data using survdat
#'
#' @description
#' Pulls survey data in format required for indicator generation
#'
#' @param channel an Object inherited from DBIConnection-class. .
#' This object is used to connect to communicate with the database engine.
#'
#'@return A list of survey data pulls
#'
#'@examples
#'\dontrun{
#'channel <- dbutils::connect_to_database("server",user)
#'rawData <- get_survey_data(channel)
#'}
#'
#'@export

get_survey_data <- function(channel) {
  end.year <- format(Sys.Date(), "%Y")
  # Get the survey data for aggregate biomass
  survey1 <- survdat::get_survdat_data(channel, getLengths = F)

  # Get the survey data for exp_n, survey_shannon
  # Grab Albatross time series (< 2009)
  al.data <- survdat::get_survdat_data(
    channel,
    filterByYear = 1963:2008,
    getLengths = FALSE
  )

  #Grab data without Bigelow conversions (>= 2009)
  big.data <- survdat::get_survdat_data(
    channel,
    filterByYear = 2009:end.year,
    conversion.factor = FALSE,
    getLengths = FALSE
  )

  # Get the survey data for condition indicator.
  # Individual lengths and weights are required
  condition <- survdat::get_survdat_data(
    channel,
    all.season = TRUE,
    getBio = TRUE
  )

  # create a list of three datasets
  survey_data <- list(
    survey1 = survey1,
    al.data = al.data,
    big.data = big.data,
    condition = condition
  )

  return(survey_data)
}
