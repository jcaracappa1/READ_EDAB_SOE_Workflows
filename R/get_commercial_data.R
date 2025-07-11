#' Get commercial data set to store somewhere
#'
#' @description
#' Pulls commercial data in multiple formats required for indicator generation
#' Menhaden data are removed from the commercial data pull since they are incomplete
#'
#' @param channel an Object inherited from DBIConnection-class.
#' @param outputPathDatasets Character string. Path to folder where data pull 
#' should be saved (Default = NULL). If not NULL the pull will be saved to the
#' folder `outputPathDatasets` with names (`comdat.rds`,`bennet.rds`)
#'
#'@return A list of commercial data pulls
#'
#'@examples
#'\dontrun{
#'channel <- dbutils::connect_to_database("server","user")
#'outputPathDataSets <- here::here()
#'rawData <- get_commercial_data(channel, outputPathDataSets)
#'}
#'
#'@export

get_commercial_data <- function(channel, outputPathDatasets=NULL) {
  
  end.year <- as.numeric(format(Sys.Date(), "%Y"))
  
  ## These EPU definitions should/could be in an external file or a data package
  #Set up EPU definitions
  gom <- data.table::data.table(AREA = c(500, 510, 512:515), EPU = 'GOM')
  gb  <- data.table::data.table(AREA = c(521:526, 551, 552, 561, 562), EPU = 'GB')
  mab <- data.table::data.table(AREA = c(537, 539, 600, 612:616, 621, 622, 625, 626, 631, 632),
                    EPU = 'MAB')
  ss  <- data.table::data.table(AREA = c(463:467, 511), EPU = 'SS')
  
  epuAreas <- data.table::rbindlist(list(gom, gb, mab, ss))
  epuAreas[, NESPP3 := 1]
  epuAreas[, MeanProp := 1]
  
  
  # Get the commercial data for comdat and bennet
  comland1 <- comlandr::get_comland_data(channel, 
                                         filterByYear = 1964:end.year,
                                         refYear = end.year-1, 
                                         refMonth = 1,
                                         aggArea = T,
                                         userAreas = epuAreas,
                                         applyProp = F, 
                                         aggGear = F,
                                         unkVar = c('MONTH', 'NEGEAR', 'AREA'),
                                         knStrata = c('HY', 'QY', 'MONTH', 'NEGEAR', 'TONCL2', 'AREA'))
  # Remove menhaden
  comland1$comland <- comland1$comland |>
    dplyr::filter(!(NESPP3 == 221))
  
  # create a list of data sets
  commercial_data <- list(
    comdat = comland1,
    bennet = comland1
  )
  
  # Save these to a specific location
  if (!is.null(outputPathDatasets)) {
    saveRDS(comland1, paste0(outputPathDatasets, "commercial_comdat.rds"))
    saveRDS(comland1, paste0(outputPathDatasets, "commercial_bennet.rds"))
  }
  
  return(commercial_data)
}
