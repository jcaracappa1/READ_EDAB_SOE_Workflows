#' create the bennet indicator
#'
#' @param inputPathBennet Character string. Full path to the comland data pull rds file
#' @param inputPathSpecies Character string. Full path to the species list data pull rds file
#'
#' @return ecodata::bennet data frame
#' 
#' 
#' History, R code to construct Bennet Indicator for Ecosystem Project
#' Author: John Walden
#' Date: October 4, 2017
#'
#' Revised January 18, 2018 to calculate the indicator relative to average conditions
#' during each time period.
#'
#' Modified by Sean Lucey 12/16/2019
#'
#' For 2019 report on standardized to 2015 as base year rather than average
#'
#' Revised October 24, 2023. The year 1985, which is the first year in the series
#' is the base year. This may need to be modified in the future if the length of
#' the time series changes
#'
#' Revised by John Walden 01/11/2024 to document changes and post final version
#' to Github
#'
#' Revised by ABeet 10/2024 for 2025 report
#' @export

create_bennet <- function(inputPathBennet, inputPathSpecies) {
  
  end.year <- format(Sys.Date(), "%Y")
  # read in comland data
  comland.data <- readRDS(inputPathBennet)$comland |>
    dplyr::filter(YEAR <= end.year)
  # Load species data
  # This may need to be updated if the species in the feeding guilds changes
  species <- readRDS(inputPathSpecies)

  #Only take U.S. landings where the value is greater than zero
  ecosys2 <- base::subset(comland.data, US == 'TRUE' & YEAR >= 1982 & SPPVALUE >= 0)
  ecosys2[, NESPP3 := as.numeric(NESPP3)]
  #Note, Next Line is to take out Eastern Oysters from the 2024 Report.
  #This may need to be revised once the problem with Eastern Oyster value
  #can be determined. NESPP3=789
  ecosys2 <- base::subset(ecosys2, NESPP3 != 789)
  #############################################################################
  # eliminates two species with non-unique NESPP3 codes
  species <- species[(species$ITISSPP != 169537 & species$ITISSPP != 172413), ]
  ##############################################################################

  ##############################################################################
  spp2 <- dplyr::select(species, NESPP3, SOE.24, Fed.Managed)
  spp2 <- base::unique(spp2[NESPP3 > 0, list(NESPP3, SOE.24, Fed.Managed)])
  ################################################################################
  #End of External Data Pulls
  ################################################################################
  sp_combine <- base::merge(ecosys2, spp2, by = "NESPP3", all.x = TRUE)

  add.apex <- data.table::data.table(
    NESPP3 = 000,
    YEAR = 1971,
    MONTH = 0,
    NEGEAR = 0,
    TONCL2 = 1,
    UTILCD = 0,
    MARKET_CODE = 'NA',
    MESHCAT = 'NA',
    US = TRUE,
    EPU = 'GB',
    SPPLIVMT = 0,
    SPPVALUE = 0,
    SOE.24 = 'Apex Predator',
    Fed.Managed = NA
  )
  sp_combine <- data.table::rbindlist(list(sp_combine, add.apex), use.names = T)

  sp_combine <- sp_combine[(SPPVALUE > 0), ]
  #Subset data into Georges Bank group
  GB <- sp_combine[EPU == 'GB' & !is.na(SOE.24), ]
  GOM <- sp_combine[EPU == 'GOM' & !is.na(SOE.24), ]
  MAB <- sp_combine[EPU == 'MAB' & !is.na(SOE.24), ]
  MAB.managed <- sp_combine[
    !is.na(SOE.24) & EPU == 'MAB' & Fed.Managed == 'MAFMC'
  ]

  #Set Up data Table
  gb.landsum <- GB[,
    base::lapply(.SD, sum, na.rm = T),
    by = c('YEAR', 'SOE.24'),
    .SDcols = c('SPPVALUE', 'SPPLIVMT')
  ]
  gb.landsum[, PRICE := SPPVALUE / SPPLIVMT]
  gb.landsum[is.na(PRICE), PRICE := 0]

  gom.landsum <- GOM[,
    base::lapply(.SD, sum, na.rm = T),
    by = c('YEAR', 'SOE.24'),
    .SDcols = c('SPPVALUE', 'SPPLIVMT')
  ]
  gom.landsum[, PRICE := SPPVALUE / SPPLIVMT]
  gom.landsum[is.na(PRICE), PRICE := 0]

  mab.landsum <- MAB[,
    base::lapply(.SD, sum, na.rm = T),
    by = c('YEAR', 'SOE.24'),
    .SDcols = c('SPPVALUE', 'SPPLIVMT')
  ]
  mab.landsum[, PRICE := SPPVALUE / SPPLIVMT]
  mab.landsum[is.na(PRICE), PRICE := 0]
  mab.landsum <- mab.landsum[(mab.landsum$YEAR >= 1982), ]

  #Look at mid-managed
  mab.landsum.managed <- MAB.managed[,
    base::lapply(.SD, sum, na.rm = T),
    by = c('YEAR', 'SOE.24'),
    .SDcols = c('SPPVALUE', 'SPPLIVMT')
  ]
  mab.landsum.managed[, PRICE := SPPVALUE / SPPLIVMT]
  mab.landsum.managed[is.na(PRICE), PRICE := 0]
  mab.landsum.managed <- mab.landsum.managed[
    (mab.landsum.managed$YEAR >= 1982),
  ]

  #The next lines are to calculate base year values for landings
  #and value for the time series by feeding guild
  gb.baseval <- gb.landsum[YEAR == 1982, list(SOE.24, SPPVALUE, SPPLIVMT)]
  data.table::setnames(gb.baseval, c('SPPVALUE', 'SPPLIVMT'), c('BASEV', 'BASEQ'))
  gb.baseval[, BASEP := BASEV / BASEQ]

  gom.baseval <- gom.landsum[YEAR == 1982, list(SOE.24, SPPVALUE, SPPLIVMT)]
  data.table::setnames(gom.baseval, c('SPPVALUE', 'SPPLIVMT'), c('BASEV', 'BASEQ'))
  gom.baseval[, BASEP := BASEV / BASEQ]

  mab.baseval <- mab.landsum[YEAR == 1982, list(SOE.24, SPPVALUE, SPPLIVMT)]
  data.table::setnames(mab.baseval, c('SPPVALUE', 'SPPLIVMT'), c('BASEV', 'BASEQ'))
  mab.baseval[, BASEP := BASEV / BASEQ]

  #Managed Mid
  mab.baseval.managed <- mab.landsum.managed[
    YEAR == 1982,
    list(SOE.24, SPPVALUE, SPPLIVMT)
  ]
  data.table::setnames(mab.baseval.managed, c('SPPVALUE', 'SPPLIVMT'), c('BASEV', 'BASEQ'))
  mab.baseval.managed[, BASEP := BASEV / BASEQ]

  #Merge Value data frame with Base Year Value Data Frame
  gb.value <- base::merge(gb.landsum, gb.baseval, by = 'SOE.24')
  gom.value <- base::merge(gom.landsum, gom.baseval, by = 'SOE.24')
  mab.value <- base::merge(mab.landsum, mab.baseval, by = 'SOE.24')
  mab.value.managed <- base::merge(
    mab.landsum.managed,
    mab.baseval.managed,
    by = 'SOE.24'
  )

  #Construct price and Volume Indicators
  #NOTE: ALL values are normalized to $1,000,000
  gb.value[, VI := ((0.5 * (BASEP + PRICE)) * (SPPLIVMT - BASEQ)) / 1000000]
  gb.value[, PI := ((0.5 * (BASEQ + SPPLIVMT)) * (PRICE - BASEP)) / 1000000]
  data.table::setkey(gb.value, 'YEAR')

  gom.value[, VI := ((0.5 * (BASEP + PRICE)) * (SPPLIVMT - BASEQ)) / 1000000]
  gom.value[, PI := ((0.5 * (BASEQ + SPPLIVMT)) * (PRICE - BASEP)) / 1000000]
  data.table::setkey(gom.value, 'YEAR')

  mab.value[, VI := ((0.5 * (BASEP + PRICE)) * (SPPLIVMT - BASEQ)) / 1000000]
  mab.value[, PI := ((0.5 * (BASEQ + SPPLIVMT)) * (PRICE - BASEP)) / 1000000]
  data.table::setkey(mab.value, 'YEAR')

  mab.value.managed[,
    VI := ((0.5 * (BASEP + PRICE)) * (SPPLIVMT - BASEQ)) / 1000000
  ]
  mab.value.managed[,
    PI := ((0.5 * (BASEQ + SPPLIVMT)) * (PRICE - BASEP)) / 1000000
  ]
  data.table::setkey(mab.value.managed, 'YEAR')

  #The next Data table sets up the yearly aggregate Bennet PI and VI
  gb.biyear <- gb.value[,
    base::lapply(.SD, sum),
    by = 'YEAR',
    .SDcols = c("VI", "PI", "BASEV", "SPPVALUE")
  ]
  gb.biyear[, revchange := (SPPVALUE - BASEV) / 1000000]
  gb.biyear[, BI := VI + PI]

  gom.biyear <- gom.value[,
    base::lapply(.SD, sum),
    by = 'YEAR',
    .SDcols = c("VI", "PI", "BASEV", "SPPVALUE")
  ]
  gom.biyear[, revchange := (SPPVALUE - BASEV) / 1000000]
  gom.biyear[, BI := VI + PI]

  mab.biyear <- mab.value[,
    base::lapply(.SD, sum),
    by = 'YEAR',
    .SDcols = c("VI", "PI", "BASEV", "SPPVALUE")
  ]
  mab.biyear[, revchange := (SPPVALUE - BASEV) / 1000000]
  mab.biyear[, BI := VI + PI]

  mab.biyear.managed <- mab.value.managed[,
    base::lapply(.SD, sum),
    by = 'YEAR',
    .SDcols = c("VI", "PI", "BASEV", "SPPVALUE")
  ]
  mab.biyear.managed[, revchange := (SPPVALUE - BASEV) / 1000000]
  mab.biyear.managed[, BI := VI + PI]

  #Organize for SOE
  regions <- c('gb', 'gom', 'mab')
  region.abb <- c('GB', 'GOM', 'MAB')
  bennet <- c()
  for (i in 1:3) {
    region.value <- base::get(paste0(regions[i], '.value'))

    #Value Index per feeding guild
    vi.region <- region.value[, list(YEAR, VI, SOE.24)]
    vi.region[, Var := paste(SOE.24, 'Volume Indicator - Bennet')]
    vi.region[, SOE.24 := NULL]
    data.table::setnames(vi.region, c('YEAR', 'VI'), c('Time', 'Value'))
    vi.region[, Units := 'Million Dollars U.S.']
    vi.region[, Region := region.abb[i]]
    vi.region[, Source := 'Commercial Fisheries Database (comland)']
    bennet <- data.table::rbindlist(list(bennet, vi.region))

    #Price Index per feeding guild
    pi.region <- region.value[, list(YEAR, PI, SOE.24)]
    pi.region[, Var := paste(SOE.24, 'Price Indicator - Bennet')]
    pi.region[, SOE.24 := NULL]
    data.table::setnames(pi.region, c('YEAR', 'PI'), c('Time', 'Value'))
    pi.region[, Units := 'Million Dollars U.S.']
    pi.region[, Region := region.abb[i]]
    pi.region[, Source := 'Commercial Fisheries Database (comland)']
    bennet <- data.table::rbindlist(list(bennet, pi.region))

    #By year
    region.biyear <- base::get(paste0(regions[i], '.biyear'))

    #Volume
    vi <- region.biyear[, list(YEAR, VI)]
    data.table::setnames(vi, c('YEAR', 'VI'), c('Time', 'Value'))
    vi[, Var := 'Total Volume Indicator - Bennet']
    vi[, Units := 'Million Dollars U.S.']
    vi[, Region := region.abb[i]]
    vi[, Source := 'Commercial Fisheries Database (comland)']
    bennet <- data.table::rbindlist(list(bennet, vi))

    #Price
    pi <- region.biyear[, list(YEAR, PI)]
    data.table::setnames(pi, c('YEAR', 'PI'), c('Time', 'Value'))
    pi[, Var := 'Total Price Index - Bennet']
    pi[, Units := 'Million Dollars U.S.']
    pi[, Region := region.abb[i]]
    pi[, Source := 'Commercial Fisheries Database (comland)']
    bennet <- data.table::rbindlist(list(bennet, pi))

    #Revenue Change
    rev <- region.biyear[, list(YEAR, revchange)]
    data.table::setnames(rev, c('YEAR', 'revchange'), c('Time', 'Value'))
    rev[, Var := 'Total Revenue Change - Bennet']
    rev[, Units := 'Million Dollars U.S.']
    rev[, Region := region.abb[i]]
    rev[, Source := 'Commercial Fisheries Database (comland)']
    bennet <- data.table::rbindlist(list(bennet, rev))
  }

  # restructure for ecodata submission
  bennet <- bennet |>
    dplyr::rename(EPU = Region) |>
    tibble::as_tibble() |>
    dplyr::select(Time, Var, Value, EPU, Units)

  return(bennet)
}
