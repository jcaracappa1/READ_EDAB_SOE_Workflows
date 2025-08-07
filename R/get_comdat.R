
# helper function ---------------

#' Helper function for the summaries in get_comdat
#' 
#' @param data specify if all data, US only, seafood only should be used
#' @param metric_name metric to be summarized
#' @param unit_name description of what is being summarized and in what units
#' 
#' @importFrom dplyr group_by summarise mutate select distinct rename
summarize_metrics <- function(data, value_col, metric_name, unit_name) {
  
  total_summary <- data |>
    dplyr::group_by(YEAR, EPU) |>
    dplyr::summarise(Value = sum({{ value_col }}, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Var = metric_name)
  
  group_summary <- data |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::summarise(Value = sum({{ value_col }}, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Var = paste(SOE.24, metric_name))
  
  managed_summary <- data |>
    dplyr::group_by(YEAR, EPU, SOE.24, Fed.Managed) |>
    dplyr::summarise(Value = sum({{ value_col }}, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Var = paste(SOE.24, Fed.Managed, "managed species -", metric_name))
  
  dplyr::bind_rows(total_summary, group_summary, managed_summary) |>
    dplyr::rename(Time = YEAR, Region = EPU) |>
    dplyr::mutate(Units = unit_name, Source = "Commercial Fisheries Database (comland)") |>
    dplyr::select(Time, Region, Var, Value, Units, Source) |>
    dplyr::distinct()
}



# get_comdat -------------------

#' Create data for ecodata::comdat 
#'
#' Processes and combines commercial landings and Menhaden data to produce a 
#' summary of landings and revenue metrics for the State of the Ecosystem report.
#'
#' @param comdat_path Character string. Path to commercial_comdat.rds
#' @param input_path_species Character string. Path to the 'SOE_species_list_24.RData' file.
#' @param menhaden_path Character string. Path to the menhaden data output by data-raw/create_menhaden_input.R
#' @param outputPathDataSets Character string. Path to folder where data pull should be saved
#'
#' @return A single tibble containing all summarized commercial data.
#' 
#'
#' @importFrom dplyr bind_rows case_when distinct filter group_by left_join mutate rename select summarise tribble
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble
#' @importFrom here here
#' @importFrom data.table as.data.table
#'
#' @export

get_comdat <- function(comdat_path,
                          input_path_species,
                          menhaden_path,
                          outputPathDataSets) {
  
  
  # Check if the input files exist ---------------------------
  if (!(file.exists(comdat_path) && file.exists(input_path_species) && file.exists(menhaden_path))) {
    stop("One or more of the input files are not present in the location specified")
  }
  
  
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
  
##comlandr data -------------
  # Load commercial data
  comland_list <- readRDS(comdat_path)
  
  comland_raw <- comland_list$comland |> 
    tibble::as_tibble() |>
    dplyr::filter(NESPP3 != 221, NESPP3 != 789) # Remove Menhaden and Eastern Oyster
  
## menhaden data -------------
  # Read in and tidy Menhaden data
  menhaden_data <- readRDS(here::here(menhaden_path)) |>
    as_tibble() |>
    # Reshape the data from wide to long format.
    # This creates one row per EPU per year, which is much easier to work with.
    tidyr::pivot_longer(
      cols = c(MABcatch, GOMcatch),
      names_to = "EPU",
      values_to = "SPPLIVMT"
    )
  
  menhaden_processed <- menhaden_data |>
    mutate(
      # Clean the EPU name by removing "catch"
      EPU = sub("catch", "", EPU),
      # Rename 'year' to 'YEAR' for consistency
      YEAR = year,
      # Add all the new columns with their constant or conditional values
      MONTH = 1,
      NESPP3 = 221,
      NEGEAR = 0,
      TONCL2 = NA,
      # Use case_when() to assign UTILCD based on the EPU
      UTILCD = case_when(
        EPU == "MAB" ~ 9,
        EPU == "GOM" ~ 7,
        TRUE ~ NA_real_ # A fallback for any other cases
      ),
      MARKET_CODE = 'UN',
      MESHCAT = NA,
      SPPVALUE = 0,
      US = TRUE
    ) |>
    # Select only the columns you need in the final desired order
    select(
      YEAR, MONTH, EPU, NESPP3, SPPLIVMT, SPPVALUE,
      NEGEAR, TONCL2, UTILCD, MARKET_CODE, MESHCAT, US
    )
  
## species list ---------------
  # Load species list for grouping
  species <- readRDS(input_path_species) # Loads object `species`
  species_codes <- species |>
    tibble::as_tibble() |>
    dplyr::select(NESPP3, SOE.24, Fed.Managed) |>
    dplyr::distinct()
  
  # 3. Combine and Clean Base Data ----
  base_data <- dplyr::bind_rows(comland_raw, menhaden_processed) |>
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
  
  # 6. Renaming step from get_comdat ----------
 comdat <-  commercial_summary |>
    # The final object in `ecodata` uses 'EPU' instead of 'Region'
    dplyr::rename(EPU = Region) |>
    # Select and arrange the final columns required for the package
    dplyr::select(Time, Var, Value, EPU, Units) |>
    dplyr::arrange(Var, Time) |>
    tibble::as_tibble()
  
  
  
# Save or return data --------------  
  if (!is.null(outputPathDataSets)) {
    saveRDS(comdat, file.path(outputPathDataSets, "comdat.rds"))
  }
  
  return(comdat)
}

