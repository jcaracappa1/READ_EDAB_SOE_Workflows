#' Pull NAFO data from 21A database
#'
#' Joins data to EPUs and add NAFO species codes.
#' Removes USA landings
#'
#' This was created because the NAFO 21B data set (which is used in comlandr::get_foregin_data) ended in 2018
#' So we were missing data
#'
#' @param isplot Boolean. Plot summary of catch data by EPU (aggregation over species)
#' @param staticPath Character string. Path to folder for static files
#'
#' @return list of 2 objects (data frame and ggplot object)
#' \item{Year}{Year}
#' \item{EPU}{EPU}
#' \item{Species}{NAFO Species Name}
#' \item{Code}{NAFO Species Code}
#' \item{Abbreviation}{NAFO Species Abbreviation}
#' \item{MT}{Metric Tons}
#'
#' @section Source
#'
#' NAFO data files are pulled from nafo.int.
#' The catch data is from database 21A (https://www.nafo.int/Data/STATLANT-21A)
#' The supporting division codes and species text files are from database 21B (https://www.nafo.int/Data/Catch-Statistics-STATLANT-21B)
#' 
#' @importFrom dplyr filter rename select left_join group_by summarise distinct mutate
#' @importFrom tidyr separate
#' @importFrom readr read_csv read_delim
#' @importFrom tibble tibble as_tibble
#' @importFrom ggplot2 ggplot geom_line aes
#' @importFrom utils download.file unzip
#' @importFrom stringr str_remove
#'
#' @export


# # using data.table ----------------------
# # copied code from https://github.com/NOAA-EDAB/SOE_data/blob/main/R/get_nafo_21a_soe.R
# 
# get_nafo_21a_soe <- function(isplot = F, pathToFiles=NULL) {
# 
#   if(is.null(pathToFiles)) {
#     stop("Please specify a path to catch data from 21A.")
#   }
# 
#   # map EPU to nafo division codes
#   neusAreaCodes <- data.frame(EPU = c("SS","GOM","GB","MAB","GB","GB","GB","MAB","MAB","MAB"), AreaCode = c(47,51:56,61:63))
#   # read in nafo division codes
#   temp <- tempfile()
#   download.file("https://www.nafo.int/Portals/0/Stats/nafo-21b-2010-18.zip",destfile=temp,quiet=TRUE)
# 
#   # division codes. supporting table
#   divisions <- readr::read_csv(unz(temp,"NAFO-21B-2010-18/divisions.txt"),col_names = F,show_col_types = F)  |>
#     dplyr::rename(AreaCode = X1,
#                   Div = X2)
#   # country codes. supporting table
#   #countryCodes <- readr::read_csv(unz(temp, "nafo-21b-2010-16/country.txt")) |>
#   nafosp <- readr::read_delim(unz(temp, "NAFO-21B-2010-18/species.txt"),delim = "\t",show_col_types = F) |>
#     tibble::as_tibble() |>
#     dplyr::rename(Species = "Longname") |>
#     dplyr::select(Code,Abbreviation,Species) |>
#     dplyr::filter(Code > 3)
# 
# 
# 
#   # read in 21A and join with division table and species table
#   nafo <- readr::read_csv(pathToFiles,show_col_types = F) |>
#     dplyr::left_join(divisions,by = c("Division"="Div")) |>
#     dplyr::rename(SpeciesName = `Species Name`,
#                   MT = `Metric Tonnes`) |>
#     tidyr::separate(col = SpeciesName,into=c("Species","Abbreviation"),sep=" - ") |>
#     dplyr::filter(!grepl("USA",Country)) |>
#     dplyr::filter(AreaCode %in% neusAreaCodes$AreaCode) |>
#     dplyr::left_join(neusAreaCodes,by = "AreaCode") |>
#     dplyr::left_join(nafosp, by = c("Abbreviation","Species")) |>
#     dplyr::group_by(Year,EPU,Species,Code,Abbreviation) |>
#     dplyr::summarise(MT = sum(MT,na.rm = T),
#                      .groups = "drop")
# 
# 
#   if (isplot) {
#     aggnafo <- nafo |>
#       dplyr::group_by(Year,EPU) |>
#       dplyr::summarise(MT = sum(MT),
#                        .groups = "drop")
#     p <- ggplot2::ggplot(data=aggnafo) +
#       ggplot2::geom_line(ggplot2::aes(x=Year,y=MT,color= EPU))
# 
# 
#     print(p)
#   }  else {
#     p = NULL
#   }
# 
# 
#   return(list(data=nafo,plot = p))
# }


# get_nafo_21a_soe in tidyverse ---------------
get_nafo_21a_soe_data <- function(plot_summary = FALSE, staticPath = NULL) {
  
  # --- Input Validation ---
  if (is.null(staticPath)) {
    stop("`staticPath` must be specified. Please provide the full path to the NAFO 21A catch data CSV file.")
  }
  if (!file.exists(staticPath)) {
    stop(paste0("NAFO 21A catch data file not found: '", staticPath, "'."))
  }
  if (!is.logical(plot_summary) || length(plot_summary) != 1) {
    stop("`plot_summary` must be a single logical value (TRUE/FALSE).")
  }
  
  #  Define EPU to NAFO Area Code Mapping (using tibble) ----
  # Using snake_case for consistency
  neus_area_codes <- tibble::tibble(
    EPU = c("SS", "GOM", "GB", "MAB", "GB", "GB", "GB", "MAB", "MAB", "MAB"),
    AreaCode = c(47, 51:56, 61:63)
  )
  
  #  Download and Read NAFO Supporting Data ----
  # WARNING: Direct download from URL can affect reproducibility if URL changes
  # or content is updated. For strict reproducibility, consider downloading
  # these files once and storing them locally in `data-raw/` under version control.
  nafo_zip_url <- "https://www.nafo.int/Portals/0/Stats/nafo-21b-2010-18.zip"
  temp_zip_file <- tempfile(fileext = ".zip")
  
  # Use a try-catch block for download in case of network issues
  tryCatch({
    download.file(nafo_zip_url, destfile = temp_zip_file, quiet = TRUE, mode = "wb")
  }, error = function(e) {
    stop(paste0("Failed to download NAFO supporting data from '", nafo_zip_url, "'. Error: ", e$message))
  })
  
  # Extract files from the downloaded zip
  unzip_dir <- tempdir()
  unzip(temp_zip_file, exdir = unzip_dir, junkpaths = TRUE) # junkpaths extracts to unzip_dir directly
  
  # Paths to extracted files
  divisions_file <- file.path(unzip_dir, "divisions.txt")
  species_file <- file.path(unzip_dir, "species.txt")
  
  if (!file.exists(divisions_file) || !file.exists(species_file)) {
    stop("Required supporting files (divisions.txt or species.txt) not found in the downloaded NAFO zip.")
  }
  
  # Read NAFO Division Codes
  divisions <- readr::read_csv(
    divisions_file,
    col_names = c("AreaCode", "Div"), # Explicitly name columns
    show_col_types = FALSE
  )
  
  # Read NAFO Species Codes
  nafo_species <- readr::read_delim(
    species_file,
    delim = "\t",
    show_col_types = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(Species = "Longname") |> # Renamed "Longname" to "Species"
    dplyr::select(Code, Abbreviation, Species) |>
    dplyr::filter(Code > 3) # Original script filter
  
  # Clean up temporary files
  unlink(temp_zip_file)
  unlink(file.path(unzip_dir, c("divisions.txt", "species.txt"))) # Clean up extracted files
  
  
  #  Read and Process NAFO 21A Catch Data ----
  nafo_catch_data <- readr::read_csv(input_path_to_files, show_col_types = FALSE) |>
    dplyr::left_join(divisions, by = c("Division" = "Div")) |>
    dplyr::rename(SpeciesName = `Species Name`, MT = `Metric Tonnes`) |>
    tidyr::separate(col = SpeciesName, into = c("Species", "Abbreviation"), sep = " - ", remove = FALSE) |> # Keep SpeciesName for now if needed, but remove=FALSE will retain it
    dplyr::filter(!grepl("USA", Country)) |> # Remove USA landings
    dplyr::filter(AreaCode %in% neus_area_codes$AreaCode) |> # Filter to NEUS relevant areas
    dplyr::left_join(neus_area_codes, by = "AreaCode") |>
    dplyr::left_join(nafo_species, by = c("Abbreviation", "Species")) |> # Join with NAFO species info
    dplyr::group_by(Year, EPU, Species, Code, Abbreviation) |>
    dplyr::summarise(MT = sum(MT, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(Year, EPU, Species) # Order for consistent output
  
  #  Generate Plot (if requested) ----
  p_output <- NULL
  if (plot_summary) {
    agg_nafo_for_plot <- nafo_catch_data |>
      dplyr::group_by(Year, EPU) |>
      dplyr::summarise(MT = sum(MT, na.rm = TRUE), .groups = "drop")
    
    p_output <- ggplot2::ggplot(data = agg_nafo_for_plot) +
      ggplot2::geom_line(ggplot2::aes(x = Year, y = MT, color = EPU)) +
      ggplot2::labs(
        title = "NAFO Catch Data by EPU (excluding USA)",
        x = "Year",
        y = "Metric Tons (MT)",
        color = "EPU"
      ) +
      ggplot2::theme_minimal()
  }
  
  #  Return Results ----
  return(list(data = nafo_catch_data, plot = p_output))
}


#' Create data for ecodata::comdat submission
#' SOE revenue data
#'#SML
#'Used for 2023 SOE after Sean Left
#' Original file name = Revenue_landings_pull.r
#'
#' @param inputPathSpecies Character string. Full path to the species list data pull rds file.
#' @param soe_report_year Numeric. The State of the Ecosystem report year. Used for file naming.
#' @param end_year Numeric. The last year of data to include in the pull and calculations.
#' @param save_to_file Logical. If `TRUE`, the processed data will be saved as
#'   an RDS and RData file in `data/` and `data-raw/` respectively, named
#'   `Commercial_data_pull_YY.rds/RData` where YY is the last two digits of `soe_report_year`.
#'   If `FALSE`, the data frame is returned.
#'
#' @importFrom dplyr rename select mutate filter group_by summarise arrange distinct left_join bind_rows
#' @importFrom tidyr expand_grid
#' @importFrom here here
#' @importFrom readr read_csv
#' @importFrom comlandr get_comland_data
#'
#' @examples
#' \dontrun{
#' # Example usage (requires a database connection and raw data files)
#' #
#' # # To create and save the data for the 2025 SOE report, up to data for 2024
#' # create_comdat(
#' #   channel = channel,
#' #   soe_report_year = 2025,
#' #   end_year = 2024,
#' #   save_to_file = TRUE
#' # )
#' #
#' # # To just return the data frame
#' # com_data <- create_comdat_pipeline(
#' #   channel = channel,
#' #   soe_report_year = 2025,
#' #   end_year = 2024,
#' #   save_to_file = FALSE
#' # )
#' }
#'
#' @return A `tibble` data frame containing the processed commercial data,
#'   or invisibly saves the data to files if `save_to_file = TRUE`.
#'
#' @export

# # using data.table ----------------------
# # copied code from https://github.com/NOAA-EDAB/SOE_data/blob/main/R/create_comdat.R
# 
# create_comdat <- function(channel, SOEreportYear, end.year, saveToFile = F) {
#   
#   
#  Set up EPU definitions
#   gom <- data.table(AREA = c(500, 510, 512:515), EPU = 'GOM')
#   gb  <- data.table(AREA = c(521:526, 551, 552, 561, 562), EPU = 'GB')
#   mab <- data.table(AREA = c(537, 539, 600, 612:616, 621, 622, 625, 626, 631, 632),
#                     EPU = 'MAB')
#   ss  <- data.table(AREA = c(463:467, 511), EPU = 'SS')
#   
#   epuAreas <- rbindlist(list(gom, gb, mab, ss))
#   epuAreas[, NESPP3 := 1]
#   epuAreas[, MeanProp := 1]
#   
#   
#   #after switch unknowns to start of code
#   comland <- comlandr::get_comland_data(channel, filterByYear = 1964:end.year,
#                                         refYear = end.year, refMonth = 1, aggArea = T,
#                                         userAreas = epuAreas, applyProp = F, aggGear = F,
#                                         unkVar = c('MONTH', 'NEGEAR', 'AREA'),
#                                         knStrata = c('HY', 'QY', 'MONTH', 'NEGEAR', 'TONCL2',
#                                                      'AREA'))
#   ## Remove menhaden
#   comland$comland <- comland$comland[NESPP3 != 221, ]
#   saveRDS(comland,here::here("data",SOEreportYear,"comlandpull_comdatbennet.rds"))
#   #comland <- readRDS(here::here("data",SOEreportYear,""))
#   
#   if(0) {
#     comland <- readRDS(here::here("data/pre2025/comlandtemp.rds"))
#   }
#   # Add Herring here
#   # epuAreasHerring <- epuAreas
#   # epuAreasHerring$NESPP3 <- NULL
#   # epuAreasHerring$MeanProp <- NULL
#   # herring2022 <- readr::read_csv(here::here("data-raw/maine_herring_catch_2022.csv")) |>
#   #   dplyr::select(Year,Month,STATAREA,GEAR_CODE,MT) |>
#   #   dplyr::rename(YEAR = Year,
#   #                 MONTH = Month,
#   #                 AREA = STATAREA,
#   #                 NEGEAR = GEAR_CODE,
#   #                 SPPLIVMT = MT) |>
#   #   dplyr::left_join(epuAreasHerring,by="AREA") |>
#   #   dplyr::select(-AREA) |>
#   #   dplyr::mutate(TONCL2 = 30,
#   #                 NESPP3 = 168,
#   #                 UTILCD = 0,
#   #                 MARKET_CODE = "UN",
#   #                 MESHCAT = "LG",
#   #                 US = "TRUE",
#   #                 SPPVALUE = NA) |>
#   #   dplyr::arrange(YEAR, MONTH, NEGEAR, TONCL2, NESPP3, UTILCD, MARKET_CODE, MESHCAT,   US,   EPU,     SPPLIVMT,     SPPVALUE)
#   #
#   # comland$comland <- rbind(comland$comland,herring2022)
#   
#   # Add Nafo 21A data from 2019 - 2022 to total landings only
#   a <- get_nafo_21a_soe(pathToFiles = here::here("data/pre2025/NAFO21A_2023.csv"),isplot = F)
#   
#   nafolandings2019plus <- a$data |>
#     dplyr::filter(Year > 2018,!(EPU == "SS")) |>
#     dplyr::group_by(Year,EPU) |>
#     dplyr::summarise(V1 = sum(MT),
#                      .groups = "drop") |>
#     dplyr::mutate(Fed.Managed = NA,
#                   SOE.24 = "Other") |>
#     dplyr::rename(YEAR = Year) |>
#     dplyr::relocate(YEAR,EPU,SOE.24,Fed.Managed,V1)
#   
#   
#   # remove eastern oyster. but only because the assigning algorithm doesnt work as expected
#   comland$comland <- comland$comland[NESPP3 != 789, ]
#   
#   # intermediate SAVE
#   #save(comland, file = here::here('data-raw', 'comland.rda'))
#   
#   # If using existing data
#   #load(here::here('data-raw', 'comland_oldway.rda'))
#   load(here::here('data-raw', 'SOE_species_list_24.RData'))
#   
#   # Extract just data
#   comland.data <- comland$comland
#   comland.data[, NESPP3 := as.numeric(NESPP3)]
#   
#   #Assign unknown EPU to Other
#   comland.data[is.na(EPU), EPU := 'OTHER']
#   
#   #Fix scallops in foreign landings to be meat weight
#   comland.data[NESPP3 == 800 & US == F, SPPLIVMT := SPPLIVMT / 8.33]
#   
#   #Add Menhaden directly
#   menhaden <- data.table::as.data.table(readRDS(here::here('data-raw/data', 'menhadenEOF.rds')))
#   #Get in same format as comland
#   #Mid-Atlantic
#   mid.men <- menhaden[, list(year, MABcatch)]
#   data.table::setnames(mid.men, c('year', 'MABcatch'), c('YEAR', 'SPPLIVMT'))
#   mid.men[, MONTH := 1]
#   mid.men[, NESPP3 := 221]
#   mid.men[, NEGEAR := 0] #Double check
#   mid.men[, TONCL2 := NA] #Double check
#   mid.men[, EPU := 'MAB']
#   mid.men[, UTILCD := 9]
#   mid.men[, MARKET_CODE := 'UN']
#   mid.men[, MESHCAT := NA]
#   mid.men[, SPPVALUE := 0]
#   mid.men[, US := T]
#   comland.data <- data.table::rbindlist(list(comland.data, mid.men), use.names = T)
#   
#   #GOM
#   gom.men <- menhaden[, list(year, GOMcatch)]
#   data.table::setnames(gom.men, c('year', 'GOMcatch'), c('YEAR', 'SPPLIVMT'))
#   gom.men[, NESPP3 := 221]
#   gom.men[, MONTH := 1]
#   gom.men[, NEGEAR := 0]
#   gom.men[, TONCL2 := NA]
#   gom.men[, EPU := 'GOM']
#   gom.men[, UTILCD := 7]
#   gom.men[, MARKET_CODE := 'UN']
#   gom.men[, MESHCAT := NA]
#   gom.men[, SPPVALUE := 0]
#   gom.men[, US := T]
#   comland.data <- data.table::rbindlist(list(comland.data, gom.men), use.names = T)
#   
#   #Aggregate by EBFM codes
#   comland.agg <- merge(comland.data, unique(species[!is.na(NESPP3), list(NESPP3, SOE.24, Fed.Managed)]),
#                        by = 'NESPP3', all.x = T)
#   
#   #Fix NA codes
#   comland.agg[is.na(SOE.24), SOE.24 := 'Other']
#   
#   
#   
#   #Landings
#   landings <- comland.agg[, sum(SPPLIVMT, na.rm = T),
#                           by = c('YEAR', 'EPU', 'SOE.24', 'Fed.Managed')]
#   #add nafo to landings
#   landings <- rbind(landings,nafolandings2019plus) |>
#     dplyr::group_by(YEAR, EPU, SOE.24, Fed.Managed) |>
#     dplyr::summarise(V1 = sum(V1),
#                      .groups = "drop") |>
#     data.table::as.data.table()
#   
#   
#   landings[, Total := sum(V1, na.rm = T), by = c('YEAR', 'EPU', 'SOE.24')]
#   landings[, Prop.managed := V1 / Total]
#   landings[, Total.all := sum(V1, na.rm = T), by = c('YEAR', 'EPU')]
#   
#   #US Landings only - affects Georges Bank
#   land.us <- comland.agg[US == T, sum(SPPLIVMT, na.rm = T),
#                          by = c('YEAR', 'EPU', 'SOE.24', 'Fed.Managed')]
#   land.us[, Total := sum(V1, na.rm = T), by = c('YEAR', 'EPU', 'SOE.24')]
#   land.us[, Prop.managed := V1 / Total]
#   land.us[, Total.all := sum(V1, na.rm = T), by = c('YEAR', 'EPU')]
#   
#   #Total
#   land.tot <- copy(landings)
#   land.tot[, Var := paste('Landings')]
#   setnames(land.tot, c('YEAR', 'EPU', 'Total.all'), c('Time', 'Region', 'Value'))
#   land.tot[, c('SOE.24', 'V1', 'Prop.managed', 'Fed.Managed', 'Total') := NULL]
#   land.tot[, Units  := 'metric tons']
#   land.tot[, Source := 'Commercial Fisheries Database (comland)']
#   setcolorder(land.tot, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   land.tot <- unique(land.tot)
#    
#   #Total - US only
#   land.tot.us <- copy(land.us)
#   land.tot.us[, Var := paste('Landings - US only')]
#   setnames(land.tot.us, c('YEAR', 'EPU', 'Total.all'), c('Time', 'Region', 'Value'))
#   land.tot.us[, c('SOE.24', 'V1', 'Prop.managed', 'Fed.Managed', 'Total') := NULL]
#   land.tot.us[, Units  := 'metric tons']
#   land.tot.us[, Source := 'Commercial Fisheries Database (comland)']
#   setcolorder(land.tot.us, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   land.tot.us <- unique(land.tot.us)
#   
#   #Total agg
#   land.agg <- copy(landings)
#   land.agg[, Var := paste(SOE.24, 'Landings')]
#   setnames(land.agg, c('YEAR', 'EPU', 'Total'), c('Time', 'Region', 'Value'))
#   land.agg[, c('SOE.24', 'V1', 'Prop.managed', 'Fed.Managed', 'Total.all') := NULL]
#   land.agg[, Units  := 'metric tons']
#   land.agg[, Source := 'Commercial Fisheries Database (comland)']
#   setcolorder(land.agg, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   land.agg <- unique(land.agg)
#   
#   #Total - US only 
#   land.agg.us <- copy(land.us)
#   land.agg.us[, Var := paste(SOE.24, 'Landings - US only')]
#   setnames(land.agg.us, c('YEAR', 'EPU', 'Total'), c('Time', 'Region', 'Value'))
#   land.agg.us[, c('SOE.24', 'V1', 'Prop.managed', 'Fed.Managed', 'Total.all') := NULL]
#   land.agg.us[, Units  := 'metric tons']
#   land.agg.us[, Source := 'Commercial Fisheries Database (comland)']
#   setcolorder(land.agg.us, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   land.agg.us <- unique(land.agg.us)
#   
#   #Landings managed 
#   land.man <- copy(landings)
#   land.man[is.na(Fed.Managed), Fed.Managed := 'Other']
#   land.man[, Var := paste(SOE.24, Fed.Managed, 'managed species - Landings weight')]
#   setnames(land.man, c('YEAR', 'EPU', 'V1'), c('Time', 'Region', 'Value'))
#   land.man[, c('SOE.24', 'Prop.managed', 'Total', 'Fed.Managed', 'Total.all') := NULL]
#   land.man[, Units  := 'metric tons']
#   land.man[, Source := 'Commercial Fisheries Database (comland)']
#   setcolorder(land.man, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   
#   #Landings managed 
#   land.man.us <- copy(land.us)
#   land.man.us[is.na(Fed.Managed), Fed.Managed := 'Other']
#   land.man.us[, Var := paste(SOE.24, Fed.Managed, 'managed species - Landings weight - US only')]
#   setnames(land.man.us, c('YEAR', 'EPU', 'V1'), c('Time', 'Region', 'Value'))
#   land.man.us[, c('SOE.24', 'Prop.managed', 'Total', 'Fed.Managed', 'Total.all') := NULL]
#   land.man.us[, Units  := 'metric tons']
#   land.man.us[, Source := 'Commercial Fisheries Database (comland)']
#   setcolorder(land.man.us, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   
#   # #Proportion managed 
#   # land.prop <- copy(landings)
#   # land.prop[is.na(Fed.Managed), Fed.Managed := 'Other']
#   # land.prop[, Var := paste(SOE.24, Fed.Managed, 'managed species - Landings prop')]
#   # setnames(land.prop, c('YEAR', 'EPU', 'Prop.managed'), c('Time', 'Region', 'Value'))
#   # land.prop[, c('SOE.24', 'V1', 'Total', 'Fed.Managed') := NULL]
#   # land.prop[, Units  := 'proportion']
#   # land.prop[, Source := 'Commercial Fisheries Database (comland)']
#   # setcolorder(land.prop, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   #
#   # #Proportion managed 
#   # land.prop.us <- copy(land.us)
#   # land.prop.us[is.na(Fed.Managed), Fed.Managed := 'Other']
#   # land.prop.us[, Var := paste(SOE.24, Fed.Managed, 'managed species - Landings prop - US only')]
#   # setnames(land.prop.us, c('YEAR', 'EPU', 'Prop.managed'), c('Time', 'Region', 'Value'))
#   # land.prop.us[, c('SOE.24', 'V1', 'Total', 'Fed.Managed') := NULL]
#   # land.prop.us[, Units  := 'proportion']
#   # land.prop.us[, Source := 'Commercial Fisheries Database (comland)']
#   # setcolorder(land.prop.us, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   
#   #Revenue 
#   revenue <- comland.agg[, sum(SPPVALUE, na.rm = T),
#                          by = c('YEAR', 'EPU', 'SOE.24', 'Fed.Managed')]
#   revenue[, Total := sum(V1, na.rm = T), by = c('YEAR', 'EPU', 'SOE.24')]
#   revenue[, Prop.managed := V1 / Total]
#   revenue[, Total.all := sum(V1, na.rm = T), by = c('YEAR', 'EPU')]
#   
#   #Revenue - US only 
#   revenue.us <- comland.agg[US == T, sum(SPPVALUE, na.rm = T),
#                             by = c('YEAR', 'EPU', 'SOE.24', 'Fed.Managed')]
#   revenue.us[, Total := sum(V1, na.rm = T), by = c('YEAR', 'EPU', 'SOE.24')]
#   revenue.us[, Prop.managed := V1 / Total]
#   revenue.us[, Total.all := sum(V1, na.rm = T), by = c('YEAR', 'EPU')]
#   
#   #Total
#   rev.tot <- copy(revenue)
#   rev.tot[, Var := paste('Revenue')]
#   setnames(rev.tot, c('YEAR', 'EPU', 'Total.all'), c('Time', 'Region', 'Value'))
#   rev.tot[, c('SOE.24', 'V1', 'Prop.managed', 'Fed.Managed', 'Total') := NULL]
#   rev.tot[, Units  := 'US dollars']
#   rev.tot[, Source := 'Commercial Fisheries Database (comland)']
#   setcolorder(rev.tot, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   rev.tot <- unique(rev.tot)
#   
#   #Total - US only
#   rev.tot.us <- copy(revenue.us)
#   rev.tot.us[, Var := paste('Revenue - US only')]
#   setnames(rev.tot.us, c('YEAR', 'EPU', 'Total.all'), c('Time', 'Region', 'Value'))
#   rev.tot.us[, c('SOE.24', 'V1', 'Prop.managed', 'Fed.Managed', 'Total') := NULL]
#   rev.tot.us[, Units  := 'US dollars']
#   rev.tot.us[, Source := 'Commercial Fisheries Database (comland)']
#   setcolorder(rev.tot.us, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   rev.tot.us <- unique(rev.tot.us)
#   
#   #Total agg 
#   rev.agg <- copy(revenue)
#   rev.agg[, Var := paste(SOE.24, 'Revenue')]
#   setnames(rev.agg, c('YEAR', 'EPU', 'Total'), c('Time', 'Region', 'Value'))
#   rev.agg[, c('SOE.24', 'V1', 'Prop.managed', 'Fed.Managed', 'Total.all') := NULL]
#   rev.agg[, Units  := 'US dollars']
#   rev.agg[, Source := 'Commercial Fisheries Database (comland)']
#   setcolorder(rev.agg, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   rev.agg <- unique(rev.agg)
#   
#   #Total - US only 
#   rev.agg.us <- copy(revenue.us)
#   rev.agg.us[, Var := paste(SOE.24, 'Revenue - US only')]
#   setnames(rev.agg.us, c('YEAR', 'EPU', 'Total'), c('Time', 'Region', 'Value'))
#   rev.agg.us[, c('SOE.24', 'V1', 'Prop.managed', 'Fed.Managed', 'Total.all') := NULL]
#   rev.agg.us[, Units  := 'US dollars']
#   rev.agg.us[, Source := 'Commercial Fisheries Database (comland)']
#   setcolorder(rev.agg.us, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   rev.agg.us <- unique(rev.agg.us)
#   
#   #Revenue managed 
#   rev.man <- copy(revenue)
#   rev.man[is.na(Fed.Managed), Fed.Managed := 'Other']
#   rev.man[, Var := paste(SOE.24, Fed.Managed, 'managed species - Revenue')]
#   setnames(rev.man, c('YEAR', 'EPU', 'V1'), c('Time', 'Region', 'Value'))
#   rev.man[, c('SOE.24', 'Prop.managed', 'Total', 'Fed.Managed', 'Total.all') := NULL]
#   rev.man[, Units  := 'US dollars']
#   rev.man[, Source := 'Commercial Fisheries Database (comland)']
#   setcolorder(rev.man, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   
#   #Revenue managed - US only 
#   rev.man.us <- copy(revenue.us)
#   rev.man.us[is.na(Fed.Managed), Fed.Managed := 'Other']
#   rev.man.us[, Var := paste(SOE.24, Fed.Managed, 'managed species - Revenue - US only')]
#   setnames(rev.man.us, c('YEAR', 'EPU', 'V1'), c('Time', 'Region', 'Value'))
#   rev.man.us[, c('SOE.24', 'Prop.managed', 'Total', 'Fed.Managed', 'Total.all') := NULL]
#   rev.man.us[, Units  := 'US dollars']
#   rev.man.us[, Source := 'Commercial Fisheries Database (comland)']
#   setcolorder(rev.man.us, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   
#   # #Proportion managed 
#   # rev.prop <- copy(revenue)
#   # rev.prop[is.na(Fed.Managed), Fed.Managed := 'Other']
#   # rev.prop[, Var := paste(SOE.24, Fed.Managed, 'managed species - Revenue prop')]
#   # setnames(rev.prop, c('YEAR', 'EPU', 'Prop.managed'), c('Time', 'Region', 'Value'))
#   # rev.prop[, c('SOE.24', 'V1', 'Total', 'Fed.Managed') := NULL]
#   # rev.prop[, Units  := 'proportion']
#   # rev.prop[, Source := 'Commercial Fisheries Database (comland)']
#   # setcolorder(rev.prop, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   #
#   # #Proportion managed - US only 
#   # rev.prop.us <- copy(revenue.us)
#   # rev.prop.us[is.na(Fed.Managed), Fed.Managed := 'Other']
#   # rev.prop.us[, Var := paste(SOE.24, Fed.Managed, 'managed species - Revenue prop - US only')]
#   # setnames(rev.prop.us, c('YEAR', 'EPU', 'Prop.managed'), c('Time', 'Region', 'Value'))
#   # rev.prop.us[, c('SOE.24', 'V1', 'Total', 'Fed.Managed') := NULL]
#   # rev.prop.us[, Units  := 'proportion']
#   # rev.prop.us[, Source := 'Commercial Fisheries Database (comland)']
#   # setcolorder(rev.prop.us, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   
#   #Proportion of seafood
#   util.code <- data.table(UTILCD = c(0, 2, 3, 4, 5, 7, 8, 9),
#                           DESC = c('food fish or unknown',
#                                    'aquaculture',
#                                    'canned pet food',
#                                    'Biomedical',
#                                    'animal food',
#                                    'bait',
#                                    'industrial - other',
#                                    'industrial - reduction'))
#   seafood <- comland.agg[US == T & UTILCD == 0, sum(SPPLIVMT, na.rm = T),
#                          by = c('YEAR', 'EPU', 'SOE.24', 'Fed.Managed', 'UTILCD')]
#   seafood[is.na(Fed.Managed), Fed.Managed := 'Other']
#   seafood[, Total := sum(V1, na.rm = T), by = c('YEAR', 'EPU', 'SOE.24')]
#   seafood[, Total.all := sum(V1, na.rm = T), by = c('YEAR', 'EPU')]
#   
#   # for(icode in util.code[, UTILCD]){
#   #   seafood <- seafood[UTILCD == icode, Var := paste(SOE.24, Fed.Managed,
#   #                                                    'managed species used for',
#   #                                                    util.code[UTILCD == icode, DESC])]
#   # }
#   # setnames(seafood, c('YEAR', 'EPU', 'V1'), c('Time', 'Region', 'Value'))
#   # seafood[, c('SOE.24', 'UTILCD', 'Fed.Managed') := NULL]
#   # seafood[, Units  := 'metric tons']
#   # seafood[, Source := 'Commercial Fisheries Database (comland)']
#   # setcolorder(seafood, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   
#   #Seafood
#   sea.tot <- copy(seafood)
#   sea.tot[, Var := paste('Seafood Landings')]
#   setnames(sea.tot, c('YEAR', 'EPU', 'Total.all'), c('Time', 'Region', 'Value'))
#   sea.tot[, c('SOE.24', 'V1', 'Fed.Managed', 'Total', 'UTILCD') := NULL]
#   sea.tot[, Units  := 'metric tons']
#   sea.tot[, Source := 'Commercial Fisheries Database (comland)']
#   setcolorder(sea.tot, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   sea.tot <- unique(sea.tot)
#   
#   sea.agg <- copy(seafood)
#   sea.agg[, Var := paste(SOE.24, 'Seafood Landings')]
#   setnames(sea.agg, c('YEAR', 'EPU', 'Total'), c('Time', 'Region', 'Value'))
#   sea.agg[, c('SOE.24', 'V1', 'Fed.Managed', 'Total.all', 'UTILCD') := NULL]
#   sea.agg[, Units  := 'metric tons']
#   sea.agg[, Source := 'Commercial Fisheries Database (comland)']
#   setcolorder(sea.agg, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   sea.agg <- unique(sea.agg)
#   
#   sea.man <- copy(seafood)
#   sea.man[, Var := paste(SOE.24, Fed.Managed, 'managed species - Seafood Landings')]
#   setnames(sea.man, c('YEAR', 'EPU', 'V1'), c('Time', 'Region', 'Value'))
#   sea.man[, c('SOE.24', 'Total', 'Fed.Managed', 'Total.all', 'UTILCD') := NULL]
#   sea.man[, Units  := 'metric tons']
#   sea.man[, Source := 'Commercial Fisheries Database (comland)']
#   setcolorder(sea.man, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))
#   sea.man <- unique(sea.man)
#   
#   commercial <- rbindlist(list(land.tot, rev.tot, sea.tot,
#                                land.agg.us, land.agg, rev.agg, sea.agg,
#                                land.man.us, land.man, rev.man, sea.man))
#   
#   if (saveToFile)  {
#     yr <- substring(SOEreportYear,3,4)
#     saveRDS(commercial, file = here::here("data-raw", paste0("Commercial_data_pull_",yr,".rds")))
#     save(commercial, file = here::here("data", paste0("Commercial_data_pull_",yr,".rds")))
#   }
#   
#   return(commercial)
#   
# }



#' Commercial fisheries data for the NES.
#'
#' This function loads raw commercial fisheries data, cleans and formats it,
#' and adds relevant metadata. It prepares the data to match
#' the structure of `ecodata` package objects.
#'
#' @param input_path_commercial_comdat Character string. Full path to the comlandr data pull rds file
#' @param inputPathSpecies Character string. Full path to the species list data pull rds file
#' @param save_clean Logical. If `TRUE`, the processed `comdat` object will be
#'   saved into the `data/` directory of the R package (assuming a package
#'   structure) using `usethis::use_data`. If `FALSE`, the processed data
#'   frame is returned.
#'   
#' @importFrom dplyr rename select arrange
#' @importFrom here here
#' @importFrom tibble as_tibble
#' @importFrom usethis use_data
#'
#' @examples
#' \dontrun{
#' # Assuming input_path_commercial_comdat has been pulled by get_commercial_data.R
#' # within your project, and renv is initialized.
#'
#' # Create the clean comdat data set
#' # get_comdat(
#' #   input_path_commercial_comdat <- "/home/<user>/EDAB_Dev/beet/commercial_comdat.rds",
#' #   save_clean = TRUE
#' # )
#'
#' # Or, to return the data frame without saving:
#' # comdat_df <- get_comdat(
#' #   input_path_commercial_comdat <- "/home/<user>/EDAB_Dev/beet/commercial_comdat.rds",
#' #   save_clean = FALSE
#' # )
#' }
#'
#' @return A `tibble` data frame containing the processed commercial data,
#'   or invisibly saves the data to `data/comdat.rda` if `save_clean = TRUE`.
#'
#' @export




# create_comdat in tidyverse ----------------------
create_comdat <- function(soe_report_year, end_year, save_to_file = FALSE) {
  
  # --- Input Validation ---
  if (!is.numeric(soe_report_year) || length(soe_report_year) != 1) {
    stop("`soe_report_year` must be a single numeric value.")
  }
  if (!is.numeric(end_year) || length(end_year) != 1) {
    stop("`end_year` must be a single numeric value.")
  }
  
  # Define EPU areas (using tibble for consistency) ----
  epu_areas <- dplyr::bind_rows(
    tibble::tibble(AREA = c(500, 510, 512:515), EPU = 'GOM'),
    tibble::tibble(AREA = c(521:526, 551, 552, 561, 562), EPU = 'GB'),
    tibble::tibble(AREA = c(537, 539, 600, 612:616, 621, 622, 625, 626, 631, 632), EPU = 'MAB'),
    tibble::tibble(AREA = c(463:467, 511), EPU = 'SS')
  ) |> 
    dplyr::mutate(NESPP3 = 1, MeanProp = 1)
  
  
  # Integrate NAFO 21A Data ----
  nafo_21a_raw_path <- here::here("data", "pre2025", "NAFO21A_2023.csv") # Assuming this path is static
  if (!file.exists(nafo_21a_raw_path)) {
    stop(paste0("NAFO 21A data file not found: '", nafo_21a_raw_path, "'."))
  }
  nafo_data_pull <- get_nafo_21a_soe(pathToFiles = nafo_21a_raw_path, isplot = FALSE)
  
  nafo_landings_2019_plus <- nafo_data_pull$data |>
    dplyr::filter(Year > 2018, !(EPU == "SS")) |>
    dplyr::group_by(Year, EPU) |>
    dplyr::summarise(V1 = sum(MT), .groups = "drop") |>
    dplyr::mutate(Fed.Managed = NA, SOE.24 = "Other") |>
    dplyr::rename(YEAR = Year) |>
    dplyr::relocate(YEAR, EPU, SOE.24, Fed.Managed, V1)
  
  
  # Add Menhaden Data ----
  menhaden_raw_path <- here::here("data-raw", "data", "menhadenEOF.rds") # Check this path if it's correct
  if (!file.exists(menhaden_raw_path)) {
    stop(paste0("Menhaden data file not found: '", menhaden_raw_path, "'."))
  }
  menhaden_data <- readRDS(menhaden_raw_path) |>
    tibble::as_tibble() # Convert to tibble from data.table
  
  # Process Mid-Atlantic Menhaden
  mid_men <- menhaden_data |>
    dplyr::select(YEAR = year, SPPLIVMT = MABcatch) |>
    dplyr::mutate(
      MONTH = 1,
      NESPP3 = 221,
      NEGEAR = 0, # Double check this value
      TONCL2 = NA_integer_, # Double check this value, use NA_integer_ for integer NA
      EPU = 'MAB',
      UTILCD = 9,
      MARKET_CODE = 'UN',
      MESHCAT = NA_character_, # Use NA_character_ for character NA
      SPPVALUE = 0,
      US = TRUE
    )
  
  # Process GOM Menhaden
  gom_men <- menhaden_data |>
    dplyr::select(YEAR = year, SPPLIVMT = GOMcatch) |>
    dplyr::mutate(
      MONTH = 1,
      NESPP3 = 221,
      NEGEAR = 0,
      TONCL2 = NA_integer_,
      EPU = 'GOM',
      UTILCD = 7,
      MARKET_CODE = 'UN',
      MESHCAT = NA_character_,
      SPPVALUE = 0,
      US = TRUE
    )
  
  # Combine all comland data including Menhaden
  comland_combined <- dplyr::bind_rows(comland_df, mid_men, gom_men)
  
  # Load Species List ----
  if (!file.exists(inputPathSpecies)) {
    stop(paste0("Species list file not found: '", inputPathSpecies, "'."))
  }
  load(inputPathSpecies, envir = loaded_env <- new.env())
  species <- loaded_env$species |> 
    tibble::as_tibble()
  
  # Prepare Final Commercial Data for Aggregation ----
  # Assign unknown EPU to 'OTHER'
  comland_combined_clean <- comland_combined  |> 
    dplyr::mutate(EPU = tidyr::replace_na(EPU, 'OTHER')) |> 
    # Fix scallops in foreign landings to be meat weight (NESPP3 == 800 & US == F)
    dplyr::mutate(SPPLIVMT = ifelse(NESPP3 == 800 & US == FALSE, SPPLIVMT / 8.33, SPPLIVMT))
  
  # Aggregate by EBFM codes and merge species info
  species_for_merge <- species  |> 
    dplyr::filter(!is.na(NESPP3))  |> 
    dplyr::distinct(NESPP3, SOE.24, Fed.Managed)
  
  comland_agg <- comland_combined_clean  |> 
    dplyr::left_join(species_for_merge, by = 'NESPP3')  |> 
    # Fix NA codes for SOE.24
    dplyr::mutate(SOE.24 = tidyr::replace_na(SOE.24, 'Other'))
  
  # Calculate Landings Indicators ----
  # Landings
  landings <- comland_agg |>
    dplyr::group_by(YEAR, EPU, SOE.24, Fed.Managed) |>
    dplyr::summarise(V1 = sum(SPPLIVMT, na.rm = TRUE), .groups = "drop") |>
    dplyr::bind_rows(nafo_landings_2019_plus) |> # Add NAFO data
    dplyr::group_by(YEAR, EPU, SOE.24, Fed.Managed) |> # Re-group after binding
    dplyr::summarise(V1 = sum(V1, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::mutate(Total = sum(V1, na.rm = TRUE)) |>
    dplyr::ungroup() |> # Ungroup for subsequent calculations if needed
    dplyr::group_by(YEAR, EPU) |>
    dplyr::mutate(Total.all = sum(V1, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Prop.managed = V1 / Total) # Calculate proportion after totals
  
  # US Landings only
  land_us <- comland_agg |>
    dplyr::filter(US == TRUE) |>
    dplyr::group_by(YEAR, EPU, SOE.24, Fed.Managed) |>
    dplyr::summarise(V1 = sum(SPPLIVMT, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::mutate(Total = sum(V1, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::group_by(YEAR, EPU) |>
    dplyr::mutate(Total.all = sum(V1, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Prop.managed = V1 / Total)
  
  # Helper function to format landings data for output
  format_landings <- function(df, var_prefix, use_us_only = FALSE) {
    df |>
      dplyr::mutate(
        Var = case_when(
          !is.na(SOE.24) & !is.na(Fed.Managed) ~
            paste(SOE.24, Fed.Managed, 'managed species -', var_prefix),
          !is.na(SOE.24) & is.na(Fed.Managed) ~
            paste(SOE.24, var_prefix),
          TRUE ~ paste(var_prefix) # This case handles Total.all
        ),
        Units = 'metric tons',
        Source = 'Commercial Fisheries Database (comland)',
        # Handle NA in Fed.Managed for formatting consistency for managed species
        Fed.Managed = tidyr::replace_na(Fed.Managed, 'Other'),
        # Adjust Var names where Fed.Managed is not applicable for Total/Total.all
        Var = if_else(grepl("Total", Var),
                      paste0(str_remove(Var, " Other managed species - "),
                             if (use_us_only) " - US only" else ""),
                      Var)
      ) |>
      dplyr::select(
        Time = YEAR,
        Value = V1, # For managed/agg, V1 is the value, for total, Total.all
        Var,
        Units,
        Region = EPU,
        Source,
        Total, # Keep for calculations, will drop later
        Total.all, # Keep for calculations, will drop later
        SOE.24, # Keep for calculations, will drop later
        Fed.Managed # Keep for calculations, will drop later
      ) |>
      dplyr::arrange(Time, Region, Var)
  }
  
  # Format specific landings outputs
  land_tot_formatted <- landings |>
    dplyr::select(YEAR, EPU, Total.all) |>
    dplyr::distinct() |>
    dplyr::mutate(Var = 'Landings') |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total.all) |>
    dplyr::mutate(Units = 'metric tons', Source = 'Commercial Fisheries Database (comland)')
  
  land_tot_us_formatted <- land_us |>
    dplyr::select(YEAR, EPU, Total.all) |>
    dplyr::distinct() |>
    dplyr::mutate(Var = 'Landings - US only') |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total.all) |>
    dplyr::mutate(Units = 'metric tons', Source = 'Commercial Fisheries Database (comland)')
  
  land_agg_formatted <- landings |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::summarise(Total_agg = sum(V1, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Var = paste(SOE.24, 'Landings')) |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total_agg) |>
    dplyr::select(-SOE.24) |> # Drop SOE.24 as it's in Var
    dplyr::mutate(Units = 'metric tons', Source = 'Commercial Fisheries Database (comland)')
  
  land_agg_us_formatted <- land_us |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::summarise(Total_agg = sum(V1, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Var = paste(SOE.24, 'Landings - US only')) |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total_agg) |>
    dplyr::select(-SOE.24) |>
    dplyr::mutate(Units = 'metric tons', Source = 'Commercial Fisheries Database (comland)')
  
  land_man_formatted <- landings |>
    dplyr::mutate(Fed.Managed = tidyr::replace_na(Fed.Managed, 'Other')) |>
    dplyr::mutate(Var = paste(SOE.24, Fed.Managed, 'managed species - Landings weight')) |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = V1) |>
    dplyr::select(-SOE.24, -Fed.Managed, -Total, -Prop.managed, -Total.all) |>
    dplyr::mutate(Units = 'metric tons', Source = 'Commercial Fisheries Database (comland)')
  
  land_man_us_formatted <- land_us |>
    dplyr::mutate(Fed.Managed = tidyr::replace_na(Fed.Managed, 'Other')) |>
    dplyr::mutate(Var = paste(SOE.24, Fed.Managed, 'managed species - Landings weight - US only')) |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = V1) |>
    dplyr::select(-SOE.24, -Fed.Managed, -Total, -Prop.managed, -Total.all) |>
    dplyr::mutate(Units = 'metric tons', Source = 'Commercial Fisheries Database (comland)')
  
  
  # Calculate Revenue Indicators ----
  # Revenue
  revenue <- comland_agg |>
    dplyr::group_by(YEAR, EPU, SOE.24, Fed.Managed) |>
    dplyr::summarise(V1 = sum(SPPVALUE, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::mutate(Total = sum(V1, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::group_by(YEAR, EPU) |>
    dplyr::mutate(Total.all = sum(V1, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Prop.managed = V1 / Total)
  
  # Revenue - US only
  revenue_us <- comland_agg |>
    dplyr::filter(US == TRUE) |>
    dplyr::group_by(YEAR, EPU, SOE.24, Fed.Managed) |>
    dplyr::summarise(V1 = sum(SPPVALUE, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::mutate(Total = sum(V1, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::group_by(YEAR, EPU) |>
    dplyr::mutate(Total.all = sum(V1, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Prop.managed = V1 / Total)
  
  # Helper function to format revenue data for output
  format_revenue <- function(df, var_prefix, use_us_only = FALSE) {
    df |>
      dplyr::mutate(
        Var = case_when(
          !is.na(SOE.24) & !is.na(Fed.Managed) ~
            paste(SOE.24, Fed.Managed, 'managed species -', var_prefix),
          !is.na(SOE.24) & is.na(Fed.Managed) ~
            paste(SOE.24, var_prefix),
          TRUE ~ paste(var_prefix) # This case handles Total.all
        ),
        Units = 'US dollars',
        Source = 'Commercial Fisheries Database (comland)',
        Fed.Managed = tidyr::replace_na(Fed.Managed, 'Other'),
        Var = if_else(grepl("Total", Var),
                      paste0(str_remove(Var, " Other managed species - "),
                             if (use_us_only) " - US only" else ""),
                      Var)
      ) |>
      dplyr::select(
        Time = YEAR,
        Value = V1, # For managed/agg, V1 is the value, for total, Total.all
        Var,
        Units,
        Region = EPU,
        Source,
        Total, Total.all, SOE.24, Fed.Managed # Keep for calculations, will drop later
      ) |>
      dplyr::arrange(Time, Region, Var)
  }
  
  # Format specific revenue outputs
  rev_tot_formatted <- revenue |>
    dplyr::select(YEAR, EPU, Total.all) |>
    dplyr::distinct() |>
    dplyr::mutate(Var = 'Revenue') |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total.all) |>
    dplyr::mutate(Units = 'US dollars', Source = 'Commercial Fisheries Database (comland)')
  
  rev_tot_us_formatted <- revenue_us |>
    dplyr::select(YEAR, EPU, Total.all) |>
    dplyr::distinct() |>
    dplyr::mutate(Var = 'Revenue - US only') |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total.all) |>
    dplyr::mutate(Units = 'US dollars', Source = 'Commercial Fisheries Database (comland)')
  
  rev_agg_formatted <- revenue |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::summarise(Total_agg = sum(V1, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Var = paste(SOE.24, 'Revenue')) |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total_agg) |>
    dplyr::select(-SOE.24) |>
    dplyr::mutate(Units = 'US dollars', Source = 'Commercial Fisheries Database (comland)')
  
  rev_agg_us_formatted <- revenue_us |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::summarise(Total_agg = sum(V1, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Var = paste(SOE.24, 'Revenue - US only')) |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total_agg) |>
    dplyr::select(-SOE.24) |>
    dplyr::mutate(Units = 'US dollars', Source = 'Commercial Fisheries Database (comland)')
  
  rev_man_formatted <- revenue |>
    dplyr::mutate(Fed.Managed = tidyr::replace_na(Fed.Managed, 'Other')) |>
    dplyr::mutate(Var = paste(SOE.24, Fed.Managed, 'managed species - Revenue')) |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = V1) |>
    dplyr::select(-SOE.24, -Fed.Managed, -Total, -Prop.managed, -Total.all) |>
    dplyr::mutate(Units = 'US dollars', Source = 'Commercial Fisheries Database (comland)')
  
  rev_man_us_formatted <- revenue_us |>
    dplyr::mutate(Fed.Managed = tidyr::replace_na(Fed.Managed, 'Other')) |>
    dplyr::mutate(Var = paste(SOE.24, Fed.Managed, 'managed species - Revenue - US only')) |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = V1) |>
    dplyr::select(-SOE.24, -Fed.Managed, -Total, -Prop.managed, -Total.all) |>
    dplyr::mutate(Units = 'US dollars', Source = 'Commercial Fisheries Database (comland)')
  
  # Calculate Seafood Landings Indicators (US Only) ----
  seafood <- comland_agg |>
    dplyr::filter(US == TRUE, UTILCD == 0) |> # US only and food fish/unknown
    dplyr::group_by(YEAR, EPU, SOE.24, Fed.Managed, UTILCD) |>
    dplyr::summarise(V1 = sum(SPPLIVMT, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Fed.Managed = tidyr::replace_na(Fed.Managed, 'Other')) |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::mutate(Total = sum(V1, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::group_by(YEAR, EPU) |>
    dplyr::mutate(Total.all = sum(V1, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  # Formatting for seafood is similar
  sea_tot_formatted <- seafood |>
    dplyr::select(YEAR, EPU, Total.all) |>
    dplyr::distinct() |>
    dplyr::mutate(Var = 'Seafood Landings') |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total.all) |>
    dplyr::mutate(Units = 'metric tons', Source = 'Commercial Fisheries Database (comland)')
  
  sea_agg_formatted <- seafood |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::summarise(Total_agg = sum(V1, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Var = paste(SOE.24, 'Seafood Landings')) |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total_agg) |>
    dplyr::select(-SOE.24) |>
    dplyr::mutate(Units = 'metric tons', Source = 'Commercial Fisheries Database (comland)')
  
  sea_man_formatted <- seafood |>
    dplyr::mutate(Var = paste(SOE.24, Fed.Managed, 'managed species - Seafood Landings')) |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = V1) |>
    dplyr::select(-SOE.24, -Fed.Managed, -Total, -Total.all, -UTILCD) |>
    dplyr::mutate(Units = 'metric tons', Source = 'Commercial Fisheries Database (comland)')
  
  # Combine All Indicators ----
  commercial_all_indicators <- dplyr::bind_rows(
    land_tot_formatted,
    rev_tot_formatted,
    sea_tot_formatted,
    land_agg_formatted,
    land_agg_us_formatted, # Note: original script had this but no matching rev/sea for US agg
    rev_agg_formatted,
    sea_agg_formatted,
    land_man_formatted,
    land_man_us_formatted,
    rev_man_formatted,
    rev_man_us_formatted,
    sea_man_formatted
  ) |>
    dplyr::arrange(Time, Region, Var) |>
    dplyr::distinct() # Remove any exact duplicate rows after combining and formatting
  
  # Save Output (if save_to_file is TRUE) ----
  if (save_to_file) {
    yr_suffix <- stringr::str_sub(as.character(soe_report_year), 3, 4)
    file_name_prefix <- paste0("Commercial_data_pull_", yr_suffix)
    
    # Save as RDS (more robust for R objects)
    saveRDS(commercial_all_indicators, file = here::here("data-raw", paste0(file_name_prefix, ".rds")))
    # Save as RData (for compatibility with previous workflows if necessary)
    commercial <- commercial_all_indicators # Rename for consistency with original save()
    save(commercial, file = here::here("data", paste0(file_name_prefix, ".RData")))
    invisible(commercial_all_indicators) # Return invisibly as saving is the primary action
  } else {
    return(commercial_all_indicators)
  }
}




get_comdat <- function(input_path_commercial_comdat, save_clean = FALSE) {
  
  # --- Input Validation ---
  if (!file.exists(input_path_commercial_comdat)) {
    stop(paste0("Input file not found: '", input_path_commercial_comdat,
                "'. Please ensure the file exists at the specified path."))
  }
  
  # --- Load Data ---
  loaded_env <- new.env() # Load into a new environment to avoid polluting global env
  load(input_path_commercial_comdat, envir = loaded_env)
  
  # Check if 'comdat' object exists in the loaded environment
  if (!"comdat" %in% ls(envir = loaded_env)) {
    stop(paste0("Expected 'comdat' object not found in '",
                basename(input_path_rdata),
                "'. Please check the content of your file."))
  }
  comdat <- loaded_env$comdat # Assign the loaded object to 'comdat' variable
  
  
  # --- Data Processing ---
  comdat <- comdat  |> 
    dplyr::rename(EPU = Region)  |> 
    dplyr::select(-Source)  |> 
    tibble::as_tibble()  |>
    dplyr::select(Time, Var, Value, EPU, Units) |> 
    dplyr::arrange(Var, Time)
  
  # --- Add Metadata as Attributes ---
  attr(comdat, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/comdat.html"
  attr(comdat, "data_files") <- list(
    comdat_RData = basename(input_path_commercial_comdat)
  )
  attr(comdat, "data_steward") <- c(
    "Sean Lucey <sean.lucey@noaa.gov>"
  )
  attr(comdat, "plot_script") <- list(
    `hd_MAB_comm-revenue` = "human_dimensions_MAB.Rmd-comdat-comm-revenue.R",
    `hd_MAB_commercial-landings` = "human_dimensions_MAB.Rmd-comdat-commercial-landings.R",
    `hd_MAB_total-landings` = "human_dimensions_MAB.Rmd-comdat-total-landings.R",
    `hd_NE_comm-revenue` = "human_dimensions_NE.Rmd-comdat-comm-revenue.R",
    `hd_NE_commercial-landings` = "human_dimensions_NE.Rmd-comdat-commercial-landings.R",
    `hd_NE_commercial-landings-gb` = "human_dimensions_NE.Rmd-comdat-commercial-landings-gb.R",
    `hd_NE_commercial-landings-gom` = "human_dimensions_NE.Rmd-comdat-commercial-landings-gom.R",
    `hd_NE_total-landings` = "human_dimensions_NE.Rmd-comdat-total-landings.R"
  )
  
  # --- Save or Return Data ---
  if (save_clean) {
    usethis::use_data(comdat, overwrite = TRUE, internal = FALSE)
    invisible(comdat) # Return invisibly as a side effect (saving) is primary
  } else {
    return(comdat)
  }
}
