#' Pull commercial data required for indicator creation
#'
#' @param channel an Object inherited from DBIConnection-class. .
#' This object is used to connect to communicate with the database engine.
#' @param outputPath Character string. Path to folder where data pull should be saved.
#' If not NULL the pull will be saved to the
#' folder `outputPath` with names (`comdat.rds`,`bennet.rds`)
#'
#' @return list of data objects. rds files exported
#'
#'
#' @examples
#' \dontrun{
#'   channel <- dbutils::connect_to_database("server","user")
#'   outputPath <- here::here()
#'   workflow_data_pull(channel,outputPath)
#' }
#'

workflow_pull_commercial_data <- function(channel, outputPath = NULL) {
  # pull commercial data
  commercial_data <- SOEworkflows::get_commercial_data(channel)

  # Save these to a specific location
  if (!is.null(outputPath)) {
    saveRDS(commercial_data$comdat, paste0(outputPath, "commercial_comdat.rds"))
    saveRDS(commercial_data$bennet, paste0(outputPath, "commercial_bennet.rds"))
  }

  return(commercial_data)
}
