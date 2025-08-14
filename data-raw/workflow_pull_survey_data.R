#' Pull survey data required for indicator creation
#'
#' @param channel an Object inherited from DBIConnection-class. .
#' This object is used to connect to communicate with the database engine.
#' @param outputPath Character string. Path to folder where data pull should be saved.
#' If not NULL the pull will be saved to the
#' folder `outputPath` with names (`albatrossData.rds`,
#' `bigelowData.rds`,`surveyNoLengths.rds`, `condition.rds`)
#'
#' @return list of data objects. rds files exported
#'
#'
#' @examples
#' \dontrun{
#'   channel <- dbutils::connect_to_database("server","user")
#'   outputPath <- here::here()
#'   workflow_pull_survey_data(channel,outputPath)
#' }
#'

workflow_pull_survey_data <- function(channel, outputPath = NULL) {
  # pull survey data
  survey_data <- SOEworkflows::get_survey_data(channel)

  # Save these to a specific location
  if (!is.null(outputPath)) {
    saveRDS(survey_data$al.data, paste0(outputPath, "/albatrossData.rds"))
    saveRDS(survey_data$big.data, paste0(outputPath, "/bigelowData.rds"))
    saveRDS(survey_data$survey1, paste0(outputPath, "/surveyNoLengths.rds"))
    saveRDS(survey_data$condition, paste0(outputPath, "/condition.rds"))
  }
  return(survey_data)
}
