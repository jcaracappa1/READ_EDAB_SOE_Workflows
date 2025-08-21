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
#' 

create_comdat <- function(comdat_path,
                       input_path_species,
                       menhaden_path,
                       outputPathDataSets) {
  
  # Check if the input files exist ---------------------------
  if (!(file.exists(comdat_path) && file.exists(input_path_species) && file.exists(menhaden_path))) {
    stop("One or more of the input files are not present in the location specified")
  }
  
  # 1. Define EPU Areas ----
  # Define each EPU area as a tibble
  gom <- tibble::tibble(AREA = c(500, 510, 512:515), EPU = "GOM")
  gb  <- tibble::tibble(AREA = c(521:526, 551, 552, 561, 562), EPU = "GB")
  mab <- tibble::tibble(AREA = c(537, 539, 600, 612:616, 621, 622, 625, 626, 631, 632), EPU = "MAB")
  ss  <- tibble::tibble(AREA = c(463:467, 511), EPU = "SS")
  
  # Combine all into one tibble
  epuAreas <- dplyr::bind_rows(gom, gb, mab, ss) |> 
    dplyr::mutate(
      NESPP3 = 1,
      MeanProp = 1
    )
  
  
  # 2. Load and Process All Data Sources ----
  comland_list <- readRDS(comdat_path)
  comland_raw <- comland_list$comland |>
    tibble::as_tibble() |>
    dplyr::filter(NESPP3 != 221, NESPP3 != 789)
  
  
  menhaden_data <- readRDS(here::here(menhaden_path)) |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(cols = c(MABcatch, GOMcatch),
                        names_to = "EPU", values_to = "SPPLIVMT") |>
    dplyr::mutate(
      EPU = sub("catch", "", EPU),
      YEAR = year,
      MONTH = 1,
      NESPP3 = 221,
      NEGEAR = 0,
      TONCL2 = NA,
      UTILCD = dplyr::case_when(EPU == "MAB" ~ 9, EPU == "GOM" ~ 7, TRUE ~ NA_real_),
      MARKET_CODE = 'UN',
      MESHCAT = NA,
      SPPVALUE = 0,
      US = TRUE
    ) |>
    dplyr::select(YEAR, MONTH, EPU, NESPP3, SPPLIVMT, SPPVALUE,
                  NEGEAR, TONCL2, UTILCD, MARKET_CODE, MESHCAT, US)
  
  species <- readRDS(input_path_species)
  species_codes <- species |>
    dplyr::distinct(NESPP3, .keep_all = TRUE) |> 
    tibble::as_tibble() |>
    dplyr::select(NESPP3, SOE.24, Fed.Managed) |>
    dplyr::distinct()
  
  # 3. Combine and Clean Base Data ----
 comland.agg <- dplyr::bind_rows(comland_raw, menhaden_data) |>
    dplyr::mutate(NESPP3 = as.numeric(NESPP3)) |>
    dplyr::left_join(species_codes, by = "NESPP3") |>
    dplyr::mutate(
      EPU = ifelse(is.na(EPU), "OTHER", EPU),
      SOE.24 = ifelse(is.na(SOE.24), "Other", SOE.24),
      SPPLIVMT = ifelse(NESPP3 == 800 & !US, SPPLIVMT / 8.33, SPPLIVMT)
    )
  
  # 4. Landings section from create_comdat ----------
  # Not adding NAFO landings like was done previously
  
  # Summarize comland.agg landings
  landings <- comland.agg |>
    dplyr::group_by(YEAR, EPU, SOE.24, Fed.Managed) |>
    dplyr::summarise(V1 = sum(SPPLIVMT, na.rm = TRUE), .groups = "drop")
  
  landings <- landings |>
    dplyr::group_by(YEAR, EPU, SOE.24, Fed.Managed) |>
    dplyr::summarise(V1 = sum(V1, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::mutate(
      Total = sum(V1, na.rm = TRUE),
      Prop.managed = V1 / Total
    ) |>
    dplyr::group_by(YEAR, EPU) |>
    dplyr::mutate(Total.all = sum(V1, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  
  # US Landings only - affects Georges Bank
  land.us <- comland.agg |> 
    dplyr::filter(US == TRUE) |> 
    dplyr::group_by(YEAR, EPU, SOE.24, Fed.Managed) |> 
    dplyr::summarise(V1 = sum(SPPLIVMT, na.rm = TRUE), .groups = "drop") |> 
    dplyr::group_by(YEAR, EPU, SOE.24) |> 
    dplyr::mutate(
      Total = sum(V1, na.rm = TRUE),
      Prop.managed = V1 / Total
      ) |> 
    dplyr::group_by(YEAR, EPU) |> 
    dplyr::mutate(Total.all = sum(V1, na.rm = TRUE)) |> 
    dplyr::ungroup()
  
  
  # Total
  land.tot <- landings |> 
    dplyr::mutate(
      Var = "Landings",
      Time = YEAR,
      Region = EPU,
      Units = "metric tons",
      Source = "Commercial Fisheries Database (comland)"
    )  |> 
    # Remove unwanted columns
    dplyr::select(Time, Value = Total.all, Var, Units, Region, Source)  |> 
    # Remove duplicate rows
    dplyr::distinct()
  
  # Total - US only
  land.tot.us <- land.us  |> 
    dplyr::mutate(
      Var = "Landings - US only",
      Time = YEAR,
      Region = EPU,
      Units = "metric tons",
      Source = "Commercial Fisheries Database (comland)"
    ) |> 
    # Remove unnecessary columns
    dplyr::select(Time, Value = Total.all, Var, Units, Region, Source) |> 
    # Drop duplicates
    dplyr::distinct()
  
  # Total agg
  land.agg <- landings |> 
    dplyr::mutate(
      Var = paste(SOE.24, "Landings"),
      Time = YEAR,
      Region = EPU,
      Units = "metric tons",
      Source = "Commercial Fisheries Database (comland)"
    ) |> 
    # Drop unnecessary columns
    dplyr::select(Time, Value = Total, Var, Units, Region, Source) |> 
    # Remove duplicate rows
    dplyr::distinct()
  
  # Total - US only
  land.agg.us <- land.us |> 
    dplyr::mutate(
      Var = paste(SOE.24, "Landings - US only"),
      Time = YEAR,
      Region = EPU,
      Units = "metric tons",
      Source = "Commercial Fisheries Database (comland)"
    ) |> 
    dplyr::select(Time, Value = Total, Var, Units, Region, Source) |> 
    # Remove duplicates
    dplyr::distinct()
  
  # Landings managed
  land.man <- landings  |> 
    dplyr::mutate(
      Fed.Managed = tidyr::replace_na(Fed.Managed, "Other"),
      Var = paste(SOE.24, Fed.Managed, "managed species - Landings weight"),
      Time = YEAR,
      Region = EPU,
      Units = "metric tons",
      Source = "Commercial Fisheries Database (comland)"
    )  |> 
    dplyr::select(Time, Value = V1, Var, Units, Region, Source)
  
  # Landings managed
  land.man.us <- land.us  |> 
    dplyr::mutate(
      Fed.Managed = tidyr::replace_na(Fed.Managed, "Other"),
      Var = paste(SOE.24, Fed.Managed, "managed species - Landings weight - US only"),
      Time = YEAR,
      Region = EPU,
      Units = "metric tons",
      Source = "Commercial Fisheries Database (comland)"
    ) |> 
    dplyr::select(Time, Value = V1, Var, Units, Region, Source)
  
  
  # 5. Revenue section from create_comdat
  # Revenue (all)
  revenue <- comland.agg |>
    dplyr::group_by(YEAR, EPU, SOE.24, Fed.Managed) |>
    dplyr::summarise(
      V1 = sum(SPPVALUE, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::mutate(
      Total = sum(V1, na.rm = TRUE),
      Prop.managed = V1 / Total
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(YEAR, EPU) |>
    dplyr::mutate(
      Total.all = sum(V1, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
  
  # Revenue - US only
  revenue.us <- comland.agg |>
    dplyr::filter(US == TRUE) |>
    dplyr::group_by(YEAR, EPU, SOE.24, Fed.Managed) |>
    dplyr::summarise(
      V1 = sum(SPPVALUE, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::mutate(
      Total = sum(V1, na.rm = TRUE),
      Prop.managed = V1 / Total
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(YEAR, EPU) |>
    dplyr::mutate(
      Total.all = sum(V1, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
  
  # Total revenue (all)
  
  rev.tot <- revenue |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total.all) |>
    dplyr::mutate(
      Var = "Revenue",
      Units = "US dollars",
      Source = "Commercial Fisheries Database (comland)"
    ) |>
    dplyr::select(Time, Value, Var, Units, Region, Source) |>
    dplyr::distinct()
  
  # Total revenue US only
  rev.tot.us <- revenue.us |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total.all) |>
    dplyr::mutate(
      Var = "Revenue - US only",
      Units = "US dollars",
      Source = "Commercial Fisheries Database (comland)"
    ) |>
    dplyr::select(Time, Value, Var, Units, Region, Source) |>
    dplyr::distinct()
  
  # Total aggregated revenue (all)
  rev.agg <- revenue |>
    dplyr::mutate(Var = paste(SOE.24, "Revenue"),
           Units = "US dollars",
           Source = "Commercial Fisheries Database (comland)") |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total) |>
    dplyr::select(Time, Value, Var, Units, Region, Source) |>
    dplyr::distinct()
  
  # Total aggregated revenue US only
  rev.agg.us <- revenue.us |>
    dplyr::mutate(Var = paste(SOE.24, "Revenue - US only"),
           Units = "US dollars",
           Source = "Commercial Fisheries Database (comland)") |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total) |>
    dplyr::select(Time, Value, Var, Units, Region, Source) |>
    dplyr::distinct()
  
  # Managed revenue (all)
  rev.man <- revenue |>
    dplyr::mutate(
      Fed.Managed = tidyr::replace_na(Fed.Managed, "Other"),
      Var = paste(SOE.24, Fed.Managed, "managed species - Revenue"),
      Units = "US dollars",
      Source = "Commercial Fisheries Database (comland)"
      ) |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = V1) |> 
    dplyr::select(Time, Value, Var, Units, Region, Source) |>
    dplyr::distinct()
  
  # Managed revenue US only
  rev.man.us <- revenue.us |>
    dplyr::mutate(
      Fed.Managed = tidyr::replace_na(Fed.Managed, "Other"),
      Var = paste(SOE.24, Fed.Managed, "managed species - Revenue - US only"),
      Units = "US dollars",
      Source = "Commercial Fisheries Database (comland)") |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = V1) |>  
    dplyr::select(Time, Value, Var, Units, Region, Source) |>
    dplyr::distinct()
  
  
  # Proportion of seafood
  # util.code as a tibble
  util.code <- tibble::tibble(
    UTILCD = c(0, 2, 3, 4, 5, 7, 8, 9),
    DESC = c('food fish or unknown',
             'aquaculture',
             'canned pet food',
             'Biomedical',
             'animal food',
             'bait',
             'industrial - other',
             'industrial - reduction')
  )
  
  # seafood aggregation
  seafood <- comland.agg |>
    dplyr::filter(US == TRUE, UTILCD == 0) |>
    dplyr::group_by(YEAR, EPU, SOE.24, Fed.Managed, UTILCD) |>
    dplyr::summarise(V1 = sum(SPPLIVMT, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      Fed.Managed = tidyr::replace_na(Fed.Managed, "Other")
    ) |>
    dplyr::group_by(YEAR, EPU, SOE.24) |>
    dplyr::mutate(Total = sum(V1, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::group_by(YEAR, EPU) |>
    dplyr::mutate(Total.all = sum(V1, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  # sea.tot
  sea.tot <- seafood |>
    dplyr::distinct(YEAR, EPU, Total.all) |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total.all) |>
    dplyr::mutate(
      Var = "Seafood Landings",
      Units = "metric tons",
      Source = "Commercial Fisheries Database (comland)"
    ) |>
    dplyr::select(Time, Value, Var, Units, Region, Source)
  
  # sea.agg
  sea.agg <- seafood |>
    dplyr::distinct(YEAR, EPU, SOE.24, Total) |>
    dplyr::mutate(
      Var = paste(SOE.24, "Seafood Landings")
    ) |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = Total) |>
    dplyr::mutate(
      Units = "metric tons",
      Source = "Commercial Fisheries Database (comland)"
    ) |>
    dplyr::select(Time, Value, Var, Units, Region, Source) |>
    dplyr::distinct()
  
  # sea.man
  sea.man <- seafood |>
    dplyr::mutate(
      Var = paste(SOE.24, Fed.Managed, "managed species - Seafood Landings")
    ) |>
    dplyr::rename(Time = YEAR, Region = EPU, Value = V1) |>
    dplyr::mutate(
      Units = "metric tons",
      Source = "Commercial Fisheries Database (comland)"
    ) |>
    dplyr::select(Time, Value, Var, Units, Region, Source) |>
    dplyr::distinct()
  
  
  # 5. Summarize to pass to get_comdat ------------
  commercial_summary  <- dplyr::bind_rows(
    land.tot, rev.tot, sea.tot,
    land.agg.us, land.agg, rev.agg, sea.agg,
    land.man.us, land.man, rev.man, sea.man
  )
  
  

  # 6. old get_comdat function -------------
  # just tidying
  comdat <- commercial_summary |>
    dplyr::rename(EPU = Region) |>
    dplyr::select(Time, Var, Value, EPU, Units) |>
    dplyr::arrange(Var, Time) |>
    tibble::as_tibble()
  
  if (!is.null(outputPathDataSets)) {
    saveRDS(comdat, file.path(outputPathDataSets, "comdat.rds"))
  }
  
  return(comdat)
}