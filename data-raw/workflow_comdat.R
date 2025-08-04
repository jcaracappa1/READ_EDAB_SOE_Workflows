#' Creates comdat indicator for SOE
#' 
#' @description
#' Creates a data frame of commercial fisheries data from the Northeast Shelf (NES) based on comlandr data.
#' 
#' 
#' 
#' @param input_path_commercial_comdat Character string. Full path to the comlandr data pull rds file
#' @param outputPath Character string. Path to folder where data pull should be saved
#' @param input_path_species Character string. Full path to the SOE species list .rds file
#' @param menhaden_path Character string. Full path to the menhaden data .rds file
#' @param report_year Numeric. The year of the State of the Ecosystem report (e.g., 2025)
#' @param end_year Numeric. The last year of data to include in the analysis (e.g., 2024)
#' 
#' @return Nothing. An .rds file is exported to 'outputPath'
#' 
#' @section Dependencies:
#' 
#' This workflow assumes that the three required input data files have been
#' created and reside in their specified paths.
#' 
#' @examples
#' \dontrun{
#'   # Define paths
#'   outputPath <- here::here("data-raw", "indicators")
#'   inputComdatPath <- here::here("data-raw", "data", "commercial_comdat.rds")
#'   inputSpeciesPath <- here::here("data-raw", "data", "SOE_species_list_24.rds")
#'   inputMenhadenPath <- here::here("data-raw", "data", "menhaden_landings.rds")
#'
#'   # Run the workflow
#'   workflow_comdat(
#'     outputPath = outputPath,
#'     input_path_commercial_comdat = input_path_commercial_comdat,
#'     input_path_species = input_path_species,
#'     menhaden_path = menhaden_path,
#'     report_year = 2025,
#'     end_year = 2024
#'   )
#' }
#'

workflow_comdat <- function(outputPath,
                            input_path_commercial_comdat,
                            input_path_species,
                            menhaden_path,
                            report_year,
                            end_year) {
  
  # 1. Check that all required input files exist ----
  if (file.exists(input_path_commercial_comdat) &&
      file.exists(input_path_species) &&
      file.exists(menhaden_path) &&
      !is.null(outputPath)) {
    
    # 2. Run the full data processing pipeline ----
    # The output of create_comdat is piped directly into get_comdat.

    comdat <- SOEworkflows::create_comdat(
      comdat_path = input_path_commercial_comdat,
      report_year = report_year,
      end_year = end_year,
      input_path_species = input_path_species,
      menhaden_path = menhaden_path
    ) |>
      SOEworkflows::get_comdat(save_for_package = FALSE)
    
    # 3. Write data to files ----
    
    # Save the final object as an .rds file to the specified output path
    saveRDS(comdat, file.path(outputPath, "comdat.rds"))
    
    
    # Print a confirmation message
    message("Successfully created and saved 'comdat.rds' to ", outputPath)
    
  } else {
    # If any file is missing, print a helpful message and stop.
    message("Workflow skipped: One or more of the input files were not found.")
  }
}
