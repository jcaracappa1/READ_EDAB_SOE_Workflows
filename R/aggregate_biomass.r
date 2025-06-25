#' Calculates aggregate_biomass data set for automated workflow
#'
#' This uses the survdat data pull from the survey package and creates EPU
#' and shelfwide indicators. It is formatted exactly like the ecodata data object
#'
#' @param inputPathSurvey Character string. Full path to the survdat data pull rds file
#' @param inputPathSpecies Character string. Full path to the species list data pull rds file
#'
#' @examples
#' \dontrun{
#' # create the ecodata::aggregate_biomass indicator for 2025
#' create_aggregate_biomass(inputPathSurvey = "path/to/survdatData.rds"),
#'                          inputPathSpecies = "path/to/species.rds")
#'
#' }
#'
#'
#' @return ecodata::aggregate_biomass data frame
#'
#' @export



create_aggregate_biomass <- function(inputPathSurvey, inputPathSpecies) {
  
  end.year <- format(Sys.Date(),"%Y")
  # Add some checks (maybe create a check function to be used by other functions)
  # Currently checks are duplicated in create_aggregate_biomass_shelfwide and create_aggregate_biomass_epu
  
  # processes EPU level indicators and shelfwide indicators
  epubio <- create_aggregate_biomass_epu(inputPathSurvey, inputPathSpecies, end.year)
  shelfbio <- create_aggregate_biomass_shelfwide(inputPathSurvey, inputPathSpecies, end.year)
  
  # Combine the two data frames
  combined_data <- rbind(epubio, shelfbio) |>
    dplyr::as_tibble() |>
    dplyr::relocate(Time, Var, Value, EPU, Units)
  
  return(combined_data)
  
}


#'Calculates Aggregate biomass data set by epu for automated workflow
#'
#' @param inputPathSurvey Input survey data full file path
#' @param inputPathSpecies Input species list full file path
#' @param end.year End year for the data
#'
#' @importFrom data.table `:=`
#'
#' @return data frame
#'
#' @export


create_aggregate_biomass_epu <- function(inputPathSurvey, inputPathSpecies, end.year) {
  
  ####################################################
  # read in data from RDS files
  # check to make sure data is in the right format
  survey <- readRDS(inputPathSurvey)
  if(!is.data.frame(survey$survdat)) {
    stop("Input data is not a data frame")
  }
  
  survdat <- survey$survdat |>
    dplyr::filter(YEAR <= end.year) |>
    data.table::as.data.table()
  #Grab species list
  species <- readRDS(inputPathSpecies) |>
    dplyr::as_tibble() |>
    data.table::as.data.table()

  uniqueSVSPP <- species |>
      dplyr::select(SVSPP, SOE.24, Fed.Managed) |>
      dplyr::distinct()
  #######################################################
  #######################################################

  #Merge to get species group and fed managed status
  # survdat <- merge(survdat, unique(species[, list(SVSPP, SOE.24, Fed.Managed)]),
  #                  by = 'SVSPP', all.x = T)
  #Merge to get species group and fed managed status
  survdat <- merge(survdat, uniqueSVSPP,
                   by = 'SVSPP', all.x = T)
  #Combine agg group and manage status
  survdat[, SOE.Managed := paste(SOE.24, Fed.Managed)]
  
  #Change seasons from all caps
  survdat[SEASON == 'FALL',   SEASON := 'Fall']
  survdat[SEASON == 'SPRING', SEASON := 'Spring']
  
  #Calculate stratified means
  #Strata sets
  EPU <- c('MAB', 'GB', 'GOM', 'SS')
  MAB <- c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
  GB  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
  GOM <- c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)
  SS  <- c(1300:1352, 3840:3990)
  
  survey.data <- c()
  
  for(iepu in 1:length(EPU)){
    epu.strata <- get(EPU[iepu])
    #Separate inshore/offshore
    epu.inshore  <- epu.strata[which(epu.strata >= 2000)]
    epu.offshore <- epu.strata[which(epu.strata <  2000)]
    
    #Calculate stratified means
    all <- survdat::calc_stratified_mean(survdat,
                                         filterByArea = epu.strata,
                                         filterBySeason = c('Fall', 'Spring'),
                                         groupDescription = 'SOE.24',
                                         tidy = T)
    all.fmc <- survdat::calc_stratified_mean(survdat,
                                             filterByArea = epu.strata,
                                             filterBySeason = c('Fall', 'Spring'),
                                             groupDescription = 'SOE.Managed',
                                             tidy = T)
    inshore.all <- survdat::calc_stratified_mean(survdat,
                                                 filterByArea = epu.inshore,
                                                 filterBySeason = c('Fall', 'Spring'),
                                                 groupDescription = 'SOE.24',
                                                 tidy = T)
    offshore.all <- survdat::calc_stratified_mean(survdat,
                                                  filterByArea = epu.offshore,
                                                  filterBySeason = c('Fall', 'Spring'),
                                                  groupDescription = 'SOE.24',
                                                  tidy = T)
    inshore.fmc <- survdat::calc_stratified_mean(survdat,
                                                 filterByArea = epu.inshore,
                                                 filterBySeason = c('Fall', 'Spring'),
                                                 groupDescription = 'SOE.Managed',
                                                 tidy = T)
    offshore.fmc <- survdat::calc_stratified_mean(survdat,
                                                  filterByArea = epu.offshore,
                                                  filterBySeason = c('Fall', 'Spring'),
                                                  groupDescription = 'SOE.Managed',
                                                  tidy = T)
    
    #Correct variable names before combining into one dataset
    #stratified biomass
    all[variable == 'strat.biomass', Var := paste(SOE.24, SEASON, 'Biomass Index')]
    all.fmc[variable == 'strat.biomass', Var := paste(SOE.Managed, 'managed species -',
                                                      SEASON, 'Biomass Index')]
    inshore.all[variable == 'strat.biomass', Var := paste(SOE.24, SEASON,
                                                          'Biomass Index - inshore')]
    inshore.fmc[variable == 'strat.biomass', Var := paste(SOE.Managed, 'managed species -',
                                                          SEASON,
                                                          'Biomass Index - inshore')]
    offshore.all[variable == 'strat.biomass', Var := paste(SOE.24, SEASON,
                                                           'Biomass Index - offshore')]
    offshore.fmc[variable == 'strat.biomass', Var := paste(SOE.Managed, 'managed species -',
                                                           SEASON,
                                                           'Biomass Index - offshore')]
    
    #Standard Error
    all[variable == 'biomass.SE', Var := paste(SOE.24, SEASON,
                                               'Biomass Standard Error')]
    all.fmc[variable == 'biomass.SE', Var := paste(SOE.Managed, 'managed species -',
                                                   SEASON,
                                                   'Biomass Standard Error')]
    inshore.all[variable == 'biomass.SE', Var := paste(SOE.24, SEASON,
                                                       'Biomass Standard Error - inshore')]
    inshore.fmc[variable == 'biomass.SE', Var := paste(SOE.Managed, 'managed species -',
                                                       SEASON,
                                                       'Biomass Standard Error - inshore')]
    offshore.all[variable == 'biomass.SE', Var := paste(SOE.24, SEASON,
                                                        'Biomass Standard Error - offshore')]
    offshore.fmc[variable == 'biomass.SE', Var := paste(SOE.Managed, 'managed species -',
                                                        SEASON,
                                                        'Biomass Standard Error - offshore')]
    
    #Combine into one dataset
    epu.all <- data.table::rbindlist(list(all, all.fmc, inshore.all, inshore.fmc, offshore.all,
                                          offshore.fmc), use.names = F)
    epu.all[, Region := EPU[iepu]]
    
    #Combine with other EPUs
    survey.data <- data.table::rbindlist(list(survey.data, epu.all))
  }
  
  #Remove unnecessary columns/rows
  survey.data[, c('SOE.24', 'SEASON', 'variable') := NULL]
  survey.data <- survey.data[!is.na(Var), ]
  
  #Rename columns and add new columns
  data.table::setnames(survey.data, c('YEAR', 'value', 'units'), c('Time', 'Value', 'Units'))
  survey.data[, Source := 'NEFSC bottom trawl survey (survdat)']
  
  #Set column order
  data.table::setcolorder(survey.data, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
  
  
  # formats to ecodata format
  survey.data <- survey.data |>
    dplyr::rename(EPU = Region) |>
    dplyr::select(Time,Var,Value,EPU,Units) |>
    dplyr::relocate(Time,Var,Value,EPU,Units) |>
    dplyr::as_tibble()
  
  # fill in Time, Var, EPUs that are missing
  expanded <- expand.grid(Time = min(survey.data$Time):end.year,
                          Var = unique(survey.data$Var),
                          EPU = unique(survey.data$EPU))
  
  survey.data <- expanded |>
    dplyr::left_join(survey.data, by = c("Time", "Var", "EPU"))
  
  return(survey.data)
}


#' Calculates shelfwide aggregate biomass data set for automated workflow
#'
#'@inheritParams create_aggregate_biomass_epu
#'
#' @examples
#' \dontrun{
#' # create the ecodata::aggregate_biomass shelfwide component for 2025
#' create_aggregate_biomass_shelfwide(inputSurvey = "path/to/survdatData.rds"),
#'                          inputSpecies = "path/to/species.rds"),
#'                          end.year = 2024)
#'
#' }
#'
#' @importFrom data.table `:=`
#' 
#' @return data frame
#'
#' @export

create_aggregate_biomass_shelfwide <- function(inputPathSurvey, inputPathSpecies, end.year){
  
  ####################################################
  # read in data from RDS files
  # check to make sure data is in the right format
  survey <- readRDS(inputPathSurvey)
  if(!is.data.frame(survey$survdat)) {
    stop("Input data is not a data frame")
  }
  
  survdat <- survey$survdat |>
    dplyr::filter(YEAR <= end.year) |>
    data.table::as.data.table()
  #Grab species list
  species <- readRDS(inputPathSpecies) |>
    data.table::as.data.table()
  
  #######################################################
  #######################################################
  
  #Merge to get species group and fed managed status
  survdat <- merge(survdat, unique(species[, list(SVSPP, SOE.24, Fed.Managed)]),
                   by = 'SVSPP', all.x = T)
  #Combine agg group and manage status
  survdat[, SOE.Managed := paste(SOE.24, Fed.Managed)]
  
  #Change seasons from all caps
  survdat[SEASON == 'FALL',   SEASON := 'Fall']
  survdat[SEASON == 'SPRING', SEASON := 'Spring']
  
  #Calculate stratified means
  strata <- unique(survdat[, STRATUM])
  #Separate inshore/offshore
  inshore  <- strata[which(strata >= 2000)]
  offshore <- strata[which(strata <  2000)]
  
  #Calculate stratified means
  all <- survdat::calc_stratified_mean(survdat,
                                       filterByArea = strata,
                                       filterBySeason = c('Fall', 'Spring'),
                                       groupDescription = 'SOE.24',
                                       tidy = T)
  all.fmc <- survdat::calc_stratified_mean(survdat,
                                           filterByArea = strata,
                                           filterBySeason = c('Fall', 'Spring'),
                                           groupDescription = 'SOE.Managed',
                                           tidy = T)
  inshore.all <- survdat::calc_stratified_mean(survdat,
                                               filterByArea = inshore,
                                               filterBySeason = c('Fall', 'Spring'),
                                               groupDescription = 'SOE.24',
                                               tidy = T)
  offshore.all <- survdat::calc_stratified_mean(survdat,
                                                filterByArea = offshore,
                                                filterBySeason = c('Fall', 'Spring'),
                                                groupDescription = 'SOE.24',
                                                tidy = T)
  inshore.fmc <- survdat::calc_stratified_mean(survdat,
                                               filterByArea = inshore,
                                               filterBySeason = c('Fall', 'Spring'),
                                               groupDescription = 'SOE.Managed',
                                               tidy = T)
  offshore.fmc <- survdat::calc_stratified_mean(survdat,
                                                filterByArea = offshore,
                                                filterBySeason = c('Fall', 'Spring'),
                                                groupDescription = 'SOE.Managed',
                                                tidy = T)
  
  #Correct variable names before combining into one dataset
  #stratified biomass
  all[variable == 'strat.biomass', Var := paste(SOE.24, SEASON, 'Biomass Index')]
  all.fmc[variable == 'strat.biomass', Var := paste(SOE.Managed, 'managed species -',
                                                    SEASON, 'Biomass Index')]
  inshore.all[variable == 'strat.biomass', Var := paste(SOE.24, SEASON,
                                                        'Biomass Index - inshore')]
  inshore.fmc[variable == 'strat.biomass', Var := paste(SOE.Managed, 'managed species -',
                                                        SEASON,
                                                        'Biomass Index - inshore')]
  offshore.all[variable == 'strat.biomass', Var := paste(SOE.24, SEASON,
                                                         'Biomass Index - offshore')]
  offshore.fmc[variable == 'strat.biomass', Var := paste(SOE.Managed, 'managed species -',
                                                         SEASON,
                                                         'Biomass Index - offshore')]
  
  #Standard Error
  all[variable == 'biomass.SE', Var := paste(SOE.24, SEASON,
                                             'Biomass Standard Error')]
  all.fmc[variable == 'biomass.SE', Var := paste(SOE.Managed, 'managed species -',
                                                 SEASON,
                                                 'Biomass Standard Error')]
  inshore.all[variable == 'biomass.SE', Var := paste(SOE.24, SEASON,
                                                     'Biomass Standard Error - inshore')]
  inshore.fmc[variable == 'biomass.SE', Var := paste(SOE.Managed, 'managed species -',
                                                     SEASON,
                                                     'Biomass Standard Error - inshore')]
  offshore.all[variable == 'biomass.SE', Var := paste(SOE.24, SEASON,
                                                      'Biomass Standard Error - offshore')]
  offshore.fmc[variable == 'biomass.SE', Var := paste(SOE.Managed, 'managed species -',
                                                      SEASON,
                                                      'Biomass Standard Error - offshore')]
  
  #Combine into one dataset
  survey.data <- data.table::rbindlist(list(all, all.fmc, inshore.all, inshore.fmc, offshore.all,
                                            offshore.fmc), use.names = F)
  survey.data[, Region := 'All']
  
  #Remove unnecessary columns/rows
  survey.data[, c('SOE.24', 'SEASON', 'variable') := NULL]
  survey.data <- survey.data[!is.na(Var), ]
  
  #Rename columns and add new columns
  data.table::setnames(survey.data, c('YEAR', 'value', 'units'), c('Time', 'Value', 'Units'))
  survey.data[, Source := 'NEFSC bottom trawl survey (survdat)']
  
  #Set column order
  data.table::setcolorder(survey.data, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
  
  # formats to ecodata format
  survey.data <- survey.data |>
    dplyr::rename(EPU = Region) |>
    dplyr::select(Time,Var,Value,EPU,Units) |>
    dplyr::relocate(Time,Var,Value,EPU,Units) |>
    dplyr::as_tibble()
  
  # fill in Time, Var, EPUs that are missing
  expanded <- expand.grid(Time = min(survey.data$Time):end.year,
                          Var = unique(survey.data$Var),
                          EPU = unique(survey.data$EPU))
  
  survey.data <- expanded |>
    dplyr::left_join(survey.data, by = c("Time", "Var", "EPU"))
  
  return(survey.data)
}



