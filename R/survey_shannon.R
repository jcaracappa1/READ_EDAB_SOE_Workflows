#' Calculates survey_shannon diversity index for automated workflow
#'
#' This uses the survdat data pull from the survey package. 
#' It is formatted exactly like the ecodata data object
#' This calculates the shannon diversity at the station level (by year, cruise etc)
#' Then takes the mean of the shannon indices for each year
#'
#' @param inputPathBigelow Character string. Full path to the Bigelow data pull rds file
#' @param inputPathAlbatross Character string. Full path to the Albatross data pull rds file
#'
#' @examples
#' \dontrun{
#' # create the ecodata::survey_shannon indicator
#' create_aggregate_biomass(inputPathAlbatros = "path/to/albatross.rds"),
#'                          inputPathBigelow = "path/to/bigelow.rds")
#'
#' }
#'
#'
#' @return ecodata::survey_shannon data frame
#'
#' @export

create_survey_shannon <- function(inputPathAlbatross,inputPathBigelow) {
  
  end.year <- format(Sys.Date(),"%Y")
  #Grab Albatross time series
  al.catch <- readRDS(inputPathAlbatross)$survdat
  #Grab data without Bigelow conversions
  big.catch <- readRDS(inputPathBigelow)$survdat
  
  #Need station data and community matrix
  #Station data----
  data.table::setkey(al.catch, CRUISE6, STRATUM, STATION)
  al.stations <- unique(al.catch, by = data.table::key(al.catch))
  al.stations <- al.stations[, list(YEAR, SEASON, CRUISE6, STATION, STRATUM, LAT, LON)]
  
  data.table::setkey(big.catch, CRUISE6, STRATUM, STATION)
  big.stations <- unique(big.catch, by = data.table::key(big.catch))
  big.stations <- big.stations[, list(YEAR, SEASON, CRUISE6, STATION, STRATUM, LAT, LON)]
  
  #Combine data sets
  stations <- data.table::rbindlist(list(al.stations, big.stations))
  
  #Community matrix----
  #Merge sexed species
  data.table::setkey(al.catch, YEAR, SEASON, CRUISE6, STRATUM, STATION, SVSPP)
  al.catch <- al.catch[, sum(ABUNDANCE, na.rm = T), by = data.table::key(al.catch)]
  data.table::setnames(al.catch, 'V1', 'ABUNDANCE')
  
  data.table::setkey(big.catch, YEAR, SEASON, CRUISE6, STRATUM, STATION, SVSPP)
  big.catch <- big.catch[, sum(ABUNDANCE, na.rm = T), by = data.table::key(big.catch)]
  data.table::setnames(big.catch, 'V1', 'ABUNDANCE')
  
  #Combine data sets
  catch <- data.table::rbindlist(list(al.catch, big.catch))
  
  # COMBINE ALL STATIONS BY YEAR AND CALCULATE ANNUAL INDEX
  
  
  #Cast wide and remove station info
  catch.wide <- data.table::dcast(catch, YEAR + SEASON + CRUISE6 + STRATUM + STATION ~ SVSPP,
                                  fun.aggregate = sum, value.var = 'ABUNDANCE')
  col.names <- names(catch.wide)[which(!names(catch.wide) %in%
                                         c('YEAR', 'SEASON', 'CRUISE6', 'STRATUM', 'STATION'))]
  comm.mat <- catch.wide[, .SD, .SDcols = col.names]
  
  # shannon index by station
  shannon <- vegan::diversity(comm.mat,index = "shannon")
  num.sp <- vegan::specnumber(comm.mat)
  
  #Add Shannon index to station data
  stations[, Shannon := shannon]
  stations[, n := num.sp]
  stations <- stations |>
    dplyr::filter(!is.na(LAT))
  
  #Test shannon shelfwide from 1992 on
  trop <- stations[YEAR >= 1992, ]
  trop.mean <- trop[, mean(Shannon), by = c('YEAR', 'SEASON')]
  data.table::setnames(trop.mean, 'V1', 'Shannon')
  
  trop.sd <- trop[, sd(Shannon), by = c('YEAR', 'SEASON')]
  data.table::setnames(trop.sd, 'V1', 'SD')
  
  trop.all <- merge(trop.mean, trop.sd, by = c('YEAR', 'SEASON'))
  
  #Determine which stations are in which EPU----
  #Stratify data by EPU
  EPU <- sf::st_read(dsn = system.file("extdata", "EPU.shp", package = "survdat"),
                     quiet = T)
  stations.epu <- survdat:::post_strat(stations, EPU, "EPU")
  
  #Calculate mean shannon index
  shannon.mean <- stations.epu[, mean(Shannon), by = c('YEAR', 'SEASON', 'EPU')]
  species.mean <- stations.epu[, mean(n), by = c('YEAR', 'SEASON', 'EPU')]
  data.table::setnames(shannon.mean, 'V1', 'Shannon')
  data.table::setnames(species.mean, 'V1', 'n')
  
  #merge
  diversity <- merge(shannon.mean, species.mean, by = c('YEAR', 'SEASON', 'EPU'))
  
  #Convert to tidy format for SOE
  shannon.mean[, Var := paste('NEFSC survey species diversity - ', SEASON)]
  data.table::setnames(shannon.mean, c('YEAR', 'Shannon'), c('Time', 'Value'))
  shannon.mean[, c('SEASON') := NULL]
  shannon.mean[, Units := 'effective Shannon']
  
  # fill in Time, Var, EPUs that are missing
  expanded <- expand.grid(Time = min(shannon.mean$Time):end.year,
                          Var = unique(shannon.mean$Var),
                          EPU = unique(shannon.mean$EPU))
  
  shannon.mean <- expanded |>
    dplyr::left_join(shannon.mean, by = c("Time", "Var", "EPU")) |>
    dplyr::as_tibble() |> 
    dplyr::relocate(Time,Var,Value,EPU,Units)
  
  
  return(shannon.mean)
  
}
