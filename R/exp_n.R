#' Calculates expected number of species for automated workflow
#'
#' This uses the survdat data pull from the survey package. 
#' It is formatted exactly like the ecodata data object
#' This calculates the expected number of species per tow in the NEFSC Bottom Trawl Survey for Fall and Spring
#'
#' @param inputPathBigelow Character string. Full path to the Bigelow data pull rds file
#' @param inputPathAlbatross Character string. Full path to the Albatross data pull rds file
#'
#' @examples
#' \dontrun{
#' # create the ecodata::exp_n indicator
#' create_exp_n(inputPathAlbatros = "path/to/albatross.rds"),
#'                          inputPathBigelow = "path/to/bigelow.rds")
#'
#' }
#' 
#' @return ESn.epu, ecodata::exp_n data frame
#'
#' @export

create_exp_n <- function(inputPathAlbatross,inputPathBigelow) {

  end.year <- format(Sys.Date(),"%Y")
  #Grab Albatross time series
  al.catch <- readRDS(inputPathAlbatross)$survdat
  #Grab data without Bigelow conversions
  big.catch <- readRDS(inputPathBigelow)$survdat
  
  #Merge sexed species
  data.table::setkey(al.catch, YEAR, SEASON, CRUISE6, STATION, STRATUM, SVSPP, LAT, LON) 
  al.catch <- al.catch[, sum(ABUNDANCE, na.rm = T), by = data.table::key(al.catch)] 
  al.catch <- data.table::setnames(al.catch, 'V1', 'ABUNDANCE')
  
  data.table::setkey(big.catch, YEAR, SEASON, CRUISE6, STATION, STRATUM, SVSPP, LAT, LON) 
  big.catch <- big.catch[, sum(ABUNDANCE, na.rm = T), by = data.table::key(big.catch)] 
  big.catch <- data.table::setnames(big.catch, 'V1', 'ABUNDANCE')
  
  #Calculate the expected number of species
  data.table::setkey(al.catch,
         CRUISE6,
         STATION,
         STRATUM)
  
  al.catch[which(is.na(ABUNDANCE)), ABUNDANCE := 1]
  al.catch[ABUNDANCE == 0, ABUNDANCE := 1]
  al.catch[, ABUNDANCE := round(ABUNDANCE)]
  
  data.table::setkey(big.catch,
         CRUISE6,
         STATION,
         STRATUM)
  
  big.catch[which(is.na(ABUNDANCE)), ABUNDANCE := 1]
  big.catch[ABUNDANCE == 0, ABUNDANCE := 1]
  big.catch[, ABUNDANCE := round(ABUNDANCE)]
  
  al.catch[, ESn := vegan::rarefy(ABUNDANCE, 1000), by = data.table::key(al.catch)]
  
  big.catch[, ESn := vegan::rarefy(ABUNDANCE, 1000), by = data.table::key(big.catch)]
  
  #Remove individual species info
  al.station <- unique(al.catch, by = data.table::key(al.catch))
  al.station[, c('SVSPP', 'ABUNDANCE', 'LAT', 'LON') := NULL]
  
  big.station <- unique(big.catch, by = data.table::key(big.catch))
  big.station[, c('SVSPP', 'ABUNDANCE', 'LAT', 'LON') := NULL]
  
  # remove stations with lat = NA
  big.catch <- big.catch |>
    dplyr::filter(!is.na(LAT))
  
  #Merge back the station data
  al.station <- merge(al.station,
                      unique(al.catch[, list(CRUISE6, STATION, STRATUM, LAT, LON)]),
                      by = data.table::key(al.catch))
  
  big.station <- merge(big.station,
                       unique(big.catch[, list(CRUISE6, STATION, STRATUM, LAT, LON)]),
                       by = data.table::key(big.catch))

  
  #Stratify the data by EPU
  EPU <- sf::st_read(dsn = system.file("extdata", "EPU.shp", package = "survdat"),
                     quiet = T)
  ESn.al.sta  <- survdat:::post_strat(al.station,  EPU, "EPU")
  ESn.big.sta <- survdat:::post_strat(big.station, EPU, "EPU")
  
  #calculate the mean ESn
  data.table::setkey(ESn.al.sta, YEAR, SEASON, EPU)
  ESn.al.epu <- ESn.al.sta[, mean(ESn), by = data.table::key(ESn.al.sta)]
  data.table::setnames(ESn.al.epu, 'V1', 'Value')
  
  data.table::setkey(ESn.big.sta, YEAR, SEASON, EPU)
  ESn.big.epu <- ESn.big.sta[, mean(ESn), by = data.table::key(ESn.big.sta)]
  data.table::setnames(ESn.big.epu, 'V1', 'Value')
  
  #calculate the standard deviation ESn
  data.table::setkey(ESn.al.sta, YEAR, SEASON, EPU)
  ESn.al.epu.stdev <- ESn.al.sta[, sd(ESn), by = data.table::key(ESn.al.sta)]
  data.table::setnames(ESn.al.epu.stdev, 'V1', 'Value')
  
  data.table::setkey(ESn.big.sta, YEAR, SEASON, EPU)
  ESn.big.epu.stdev <- ESn.big.sta[, sd(ESn), by = data.table::key(ESn.big.sta)]
  data.table::setnames(ESn.big.epu.stdev, 'V1', 'Value')
  
  #Get in correct long format for SOE
  ESn.al.epu[, Var := paste(SEASON, EPU, 'Expected Number of Species - Albatross')]
  data.table::setnames(ESn.al.epu, 'YEAR', 'Time')
  ESn.al.epu[, c('SEASON', 'EPU') := NULL]
  ESn.al.epu[, Units := 'n species per 1000 ind']
  
  ESn.big.epu[, Var := paste(SEASON, EPU, 'Expected Number of Species - Bigelow')]
  data.table::setnames(ESn.big.epu, 'YEAR', 'Time')
  ESn.big.epu[, c('SEASON', 'EPU') := NULL]
  ESn.big.epu[, Units := 'n species per 1000 ind']
  
  ESn.al.epu.stdev[, Var := paste(SEASON, EPU, 'Expected Number of Species Standard Deviation - Albatross')]
  data.table::setnames(ESn.al.epu.stdev, 'YEAR', 'Time')
  ESn.al.epu.stdev[, c('SEASON', 'EPU') := NULL]
  ESn.al.epu.stdev[, Units := 'n species per 1000 ind']
  
  ESn.big.epu.stdev[, Var := paste(SEASON, EPU, 'Expected Number of Species Standard Deviation - Bigelow')]
  data.table::setnames(ESn.big.epu.stdev, 'YEAR', 'Time')
  ESn.big.epu.stdev[, c('SEASON', 'EPU') := NULL]
  ESn.big.epu.stdev[, Units := 'n species per 1000 ind']
  
  ESn.epu <- data.table::rbindlist(list(ESn.al.epu, ESn.big.epu, ESn.al.epu.stdev, ESn.big.epu.stdev))
  
  # fill in Time, Var, EPUs that are missing
  expanded <- expand.grid(Time = min(ESn.epu$Time):end.year,
                          Var = unique(ESn.epu$Var))
  ESn.epu <- expanded |>
    dplyr::left_join(ESn.epu, by = c("Time", "Var"))
  
  return(ESn.epu)
}
