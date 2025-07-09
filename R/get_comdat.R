# get_nafo_21a_soe -------------------

#' Process NAFO 21A Catch Data
#'
#' @description
#' Reads NAFO 21A catch data, joins it with EPU (Ecological Production Unit)
#' spatial data, and attaches NAFO species codes. Landings from the USA are
#' excluded from the final summary.
#'
#' This function was created to supplement NAFO 21B data, which concluded in 2018.
#' 
#' Using as a template: https://github.com/NOAA-EDAB/SOE_data/blob/main/R/get_nafo_21a_soe.R
#'
#' @param path_to_files A character string providing the full file path to the
#'   NAFO 21A catch CSV file.
#' @param create_plot A boolean value. If `TRUE`, a ggplot object summarizing
#'   catch by EPU over time is generated and returned.
#'
#' @return A list containing two objects:
#' \describe{
#'   \item{data}{A data frame with columns: `Year`, `EPU`, `Species`, `Code`,
#'   `Abbreviation`, and total catch in `MT` (Metric Tons).}
#'   \item{plot}{A ggplot object if `create_plot = TRUE`; otherwise, `NULL`.}
#' }
#'
#' @section Source:
#' NAFO data files are sourced from nafo.int.
#' - Catch Data: 21A Database (https://www.nafo.int/Data/STATLANT-21A)
#' - Supporting Files: 21B Database (https://www.nafo.int/Data/Catch-Statistics-STATLANT-21B)
#' 
#' @importFrom dplyr rename select filter left_join group_by summarise
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal
#' @importFrom readr read_csv read_delim
#' @importFrom tibble tribble
#' @importFrom tidyr separate
#' @importFrom utils download.file
#'
#' @export

process_nafo_catch_data <- function(path_to_files = NULL, create_plot = FALSE) {
  
  # 1. Input Validation ----
  if (is.null(path_to_files)) {
    stop("Error: Please provide a valid file path for the NAFO 21A catch data via the 'path_to_files' argument.")
  }
  
  # 2. Define Helper Data and Constants ----
  # Map NAFO division codes to corresponding Ecological Production Units (EPUs)
  epu_division_map <- tibble::tribble(
    ~EPU,   ~AreaCode,
    "SS",   47,
    "GOM",  51, "GOM", 52, "GOM", 53, "GOM", 54, "GOM", 55, "GOM", 56,
    "GB",   61, "GB", 62, "GB", 63,
    "MAB",  61, "MAB", 62, "MAB", 63
  )
  
  # 3. Download and Read NAFO Supporting Data ----
 # Will want a hard copy at some point for reproducibility
  # a change in URL would break this script currently
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
  
  
  #  4. Read and Process NAFO 21A Catch Data ----
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
  
  # 5. Generate Plot (if requested) ----
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
  
  #  6. Return Results ----
  return(list(data = nafo_catch_data, plot = p_output))
}



# create_comdat -------------------



#' Summarize Commercial Metrics in Multiple Ways
#'
#' Core helper function to calculate total, species-group, and managed-status
#' metrics for a given value (e.g., landings or revenue).
#'
#' @param data The input tibble (e.g., all data, US-only data).
#' @param value_col The unquoted column name to summarize (SPPLIVMT or SPPVALUE).
#' @param metric_name A string describing the metric (e.g., "Landings", "Revenue").
#' @param unit_name A string for the units (e.g., "metric tons", "US dollars").
#' @return A tibble containing all summarized metrics.

summarize_metrics <- function(data, value_col, metric_name, unit_name) {
  
  # 1. Total metric by EPU
  total_summary <- data |>
    dplyr::group_by(YEAR, EPU) |>
    dplyr::summarise(Value = sum({{ value_col }}, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Var = metric_name)
  
  # 2. Metric by SOE Species Group
  group_summary <- data |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::summarise(Value = sum({{ value_col }}, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Var = paste(SOE.24, metric_name))
  
  # 3. Metric by Management Status
  managed_summary <- data |>
    dplyr::group_by(YEAR, EPU, SOE.24, Fed.Managed) |>
    dplyr::summarise(Value = sum({{ value_col }}, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Var = paste(SOE.24, Fed.Managed, "managed species -", metric_name))
  
  # Combine all summaries into one tibble
  dplyr::bind_rows(total_summary, group_summary, managed_summary) |>
    dplyr::rename(Time = YEAR, Region = EPU) |>
    dplyr::mutate(Units = unit_name, Source = "Commercial Fisheries Database (comland)") |>
    dplyr::select(Time, Region, Var, Value, Units, Source) |>
    dplyr::distinct()
}


#' Create data for ecodata::comdat submission
#'
#' Processes and combines commercial landings, NAFO foreign landings, and
#' Menhaden data to produce a summary of landings and revenue metrics for the
#' State of the Ecosystem report.
#'
#' @param channel An ODBC database connection object.
#' @param report_year The year of the SOE report (e.g., 2025).
#' @param end_year The last year of data to include.
#' @param nafo_path Path to the NAFO 21A CSV file.
#' @param species_list_path Path to the 'SOE_species_list_24.RData' file.
#' @param menhaden_path Path to the menhaden data from SEFSC.
#' @param save_to_file Boolean. If TRUE, saves the final output to disk.
#'
#' @return A single tibble containing all summarized commercial data.
#' 
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr bind_rows bind_cols case_when distinct filter group_by left_join mutate rename select summarise tribble
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble
#' @importFrom here here
#' @importFrom data.table as.data.table
#' @importFrom comlandr get_comland_data
#' @importFrom usethis use_data
#'
#' @export

create_comdat <- function(channel,
                          report_year,
                          end_year,
                          nafo_path,
                          species_list_path,
                          menhaden_path,
                          save_to_file = FALSE) {
  
  # 1. Define EPU Areas for comlandr ----
  epu_areas <- dplyr::tribble(
    ~AREA, ~EPU,
    500, "GOM", 510, "GOM", 512, "GOM", 513, "GOM", 514, "GOM", 515, "GOM",
    521, "GB",  522, "GB",  523, "GB",  524, "GB",  525, "GB",  526, "GB",
    551, "GB",  552, "GB",  561, "GB",  562, "GB",
    537, "MAB", 539, "MAB", 600, "MAB", 612, "MAB", 613, "MAB", 614, "MAB",
    615, "MAB", 616, "MAB", 621, "MAB", 622, "MAB", 625, "MAB", 626, "MAB",
    631, "MAB", 632, "MAB",
    463, "SS",  464, "SS",  465, "SS",  466, "SS",  467, "SS",  511, "SS"
  ) |>
    dplyr::mutate(NESPP3 = 1, MeanProp = 1) |>
    data.table::as.data.table() # comlandr requires a data.table
  
  # 2. Load and Process All Data Sources ----
  
  # Load commercial data from database
  comland_raw <- comlandr::get_comland_data(
    channel = channel, filterByYear = 1964:end_year, refYear = end_year, 
    refMonth = 1, aggArea = T, userAreas = epu_areas, 
    unkVar = c('MONTH', 'NEGEAR', 'AREA'),
    knStrata = c('HY', 'QY', 'MONTH', 'NEGEAR', 'TONCL2', 'AREA')
  )$comland |>
    tibble::as_tibble() |>
    dplyr::filter(NESPP3 != 221, NESPP3 != 789) # Remove Menhaden and Eastern Oyster
  
  # Load NAFO foreign landings
  
  source(here::here("R/get_nafo_21a_soe.r"))
  
  nafo_landings <- get_nafo_21a_soe(pathToFiles = nafo_path, isplot = FALSE)$data |>
    dplyr::filter(Year > 2018, EPU != "SS") |>
    dplyr::group_by(Year, EPU) |>
    dplyr::summarise(SPPLIVMT = sum(MT, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      SOE.24 = "Other", Fed.Managed = NA, US = FALSE, SPPVALUE = 0,
      NESPP3 = 999 # Assign a code for "other foreign landings"
    ) |>
    dplyr::rename(YEAR = Year)
  
  # Load Menhaden data

  ## call in raw data
  ### year
    col_a <- readxl::read_excel(
      path = menhaden_path,
      sheet = 1,
      range = "A7:A65",
      col_names = TRUE
    )
  ### mid data (mt)
    col_e <- readxl::read_excel(
      path = menhaden_path,
      sheet = 1,
      range = "E7:E65",
      col_names = TRUE
    )
  ### gom data (mt)  
    col_f <- readxl::read_excel(
      path = menhaden_path,
      sheet = 1,
      range = "F7:F65",
      col_names = TRUE
    )
    
  
  ## Mid-Atlantic  
    mid.men <- dplyr::bind_cols(col_a, col_e) |> 
      dplyr::rename(YEAR = year, SPPLIVMT = 'NC Border northward to MA Border') |> 
      dplyr::mutate(MONTH = 1, NESPP3 = 221, NEGEAR = 0, TONCL2 = NA, EPU = 'MAB', UTILCD = 9,
                    MARKET_CODE = 'UN', MESHCAT = NA, SPPVALUE = 0, US = T)
    
    
    
  ## GOM
    gom.men <- dplyr::bind_cols(col_a, col_f) |> 
      dplyr::rename(YEAR = year, SPPLIVMT = 'MA Border and North') |> 
      dplyr::mutate(NESPP3 = 221, MONTH = 1, NEGEAR = 0, TONCL2 = NA, EPU = 'GOM',
                    UTILCD = 7, MARKET_CODE = 'UN', MESHCAT = NA, 
                    SPPVALUE = 0, US = T)
  
  
  
  # Load species list for grouping
  load(species_list_path) # Loads object `species`
  species_codes <- species |>
    tibble::as_tibble() |>
    dplyr::select(NESPP3, SOE.24, Fed.Managed) |>
    dplyr::distinct()
  
  # 3. Combine and Clean Base Data ----
  base_data <- dplyr::bind_rows(comland_raw, menhaden_data, nafo_landings) |>
    dplyr::mutate(NESPP3 = as.numeric(NESPP3)) |>
    dplyr::left_join(species_codes, by = "NESPP3") |>
    dplyr::mutate(
      EPU = ifelse(is.na(EPU), "OTHER", EPU),
      SOE.24 = ifelse(is.na(SOE.24), "Other", SOE.24),
      Fed.Managed = ifelse(is.na(Fed.Managed), "Other", Fed.Managed),
      # Correct scallop weight for foreign landings
      SPPLIVMT = ifelse(NESPP3 == 800 & !US, SPPLIVMT / 8.33, SPPLIVMT)
    )
  
  # 4. Generate All Summaries ----
  
  # Create data subsets for specific summaries
  us_data <- dplyr::filter(base_data, US)
  seafood_data <- dplyr::filter(us_data, UTILCD %in% c(0, NA)) # Food fish or unknown
  
  # Calculate metrics for landings and revenue
  landings <- summarize_metrics(base_data, SPPLIVMT, "Landings", "metric tons")
  landings_us <- summarize_metrics(us_data, SPPLIVMT, "Landings - US only", "metric tons")
  
  revenue <- summarize_metrics(base_data, SPPVALUE, "Revenue", "US dollars")
  revenue_us <- summarize_metrics(us_data, SPPVALUE, "Revenue - US only", "US dollars")
  
  # Calculate metrics for seafood-only landings
  seafood <- summarize_metrics(seafood_data, SPPLIVMT, "Seafood Landings", "metric tons")
  
  # 5. Combine Final Results and Save ----
  commercial_summary <- dplyr::bind_rows(
    landings, landings_us,
    revenue, revenue_us,
    seafood
  )
  
  if (save_to_file) {
    yr <- substring(report_year, 3, 4)
    save_path <- here::here("data-raw", paste0("Commercial_data_pull_", yr, ".rds"))
    saveRDS(commercial_summary, file = save_path)
    # Also save to the data/ folder for package use
    usethis::use_data(commercial_summary, overwrite = TRUE)
  }
  
  return(commercial_summary)
}

# get_comdat ------------------



#' Process Commercial Fisheries Data
#'
#' @description
#' Loads and cleans commercial fisheries data derived from the Commercial
#' Fisheries Database Biological Sample. The function selects and renames
#' relevant columns, sets data attributes for metadata, and provides an
#' option to save the cleaned data.
#' Using as a template: https://github.com/NOAA-EDAB/ecodata/blob/dev/data-raw/get_comdat.R
#' 
#' #' @param save_clean A boolean value. If `TRUE`, the cleaned data frame is saved
#'   as `comdat.rda` in the `data/` directory using `usethis::use_data()`.
#'   If `FALSE` (the default), the function returns the data frame to the console.
#'
#' @return If `save_clean` is `FALSE`, returns a tibble with the cleaned
#'   commercial data. If `TRUE`, the function saves the data and returns
#'   nothing.
#'
#' @section Source:
#' More information about these data are available at
#' https://noaa-edab.github.io/tech-doc/comdat.html
#'
#' @importFrom dplyr rename select arrange
#' @importFrom here here
#' @importFrom tibble as_tibble
#' @importFrom usethis use_data
#'
#' @export

get_comdat <- function(save_clean = FALSE) {
  
  # 1. Define File Paths and Constants ----
  raw_data_dir <- here::here("data-raw")
  comdat_rdata_file <- "Commercial_data_pull_25.RData"
  
  # 2. Load Raw Data ----
  # The `load()` function loads the object `comdat` into the environment
  load(file.path(raw_data_dir, comdat_rdata_file))
  
  # 3. Clean and Process Data ----
  comdat_clean <- comdat |>
    dplyr::rename(EPU = Region) |>
    dplyr::select(-Source) |>
    tibble::as_tibble() |>
    dplyr::select(Time, Var, Value, EPU, Units) |>
    dplyr::arrange(Var, Time)
  
  # 4. Attach Metadata as Attributes ----
  attr(comdat_clean, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/comdat.html"
  attr(comdat_clean, "data_files")   <- list(comdat_RData = comdat_rdata_file)
  attr(comdat_clean, "data_steward") <- "Sean Lucey <sean.lucey@noaa.gov>"
  attr(comdat_clean, "plot_script")  <- list(
    `hd_MAB_comm-revenue` = "human_dimensions_MAB.Rmd-comdat-comm-revenue.R",
    `hd_MAB_commercial-landings` = "human_dimensions_MAB.Rmd-comdat-commercial-landings.R",
    `hd_MAB_total-landings` = "human_dimensions_MAB.Rmd-comdat-total-landings.R",
    `hd_NE_comm-revenue` = "human_dimensions_NE.Rmd-comdat-comm-revenue.R",
    `hd_NE_commercial-landings` = "human_dimensions_NE.Rmd-comdat-commercial-landings.R",
    `hd_NE_commercial-landings-gb` = "human_dimensions_NE.Rmd-comdat-commercial-landings-gb.R",
    `hd_NE_commercial-landings-gom` = "human_dimensions_NE.Rmd-comdat-commercial-landings-gom.R",
    `hd_NE_total-landings` = "human_dimensions_NE.Rmd-comdat-total-landings.R"
  )
  
  # 5. Save or Return Data ----
  if (save_clean) {
    # Save the data object 'comdat_clean' to the data/ folder for use in a package
    usethis::use_data(comdat_clean, overwrite = TRUE)
  } else {
    # Return the cleaned data frame
    return(comdat_clean)
  }
}