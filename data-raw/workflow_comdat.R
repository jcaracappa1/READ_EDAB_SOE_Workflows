#' Calculates comdat data set for automated workflow
#'
#' This uses the commercial data pull from the comlandr package.
#'
#' @param comdat_path Character string. Full path to the commercial data rds file for bennet indicator
#' @param input_path_species Character string. Full path to the species list data pull rds file
#' @param outputPathDataSets Character string. Path to folder where data pull should be saved
#' @param menhaden_path Character string. Full path to the menhaden data .rds file
#' @param report_year Numeric. The year of the State of the Ecosystem report (e.g., 2025)
#' @param end_year Numeric. The last year of data to include in the analysis (e.g., 2024)
#'
#' @example
#' \dontrun{
#' # create the ecodata::comdat indicator
#' workflow_comdat(comdat_path = "path/to/commerical_comdat.rds",
#'                       input_path_species = "path/to/species/data/.rds",
#'                       menhaden_path = "path/to/menhaden/data/.rds",
#'                       report_year = 2025
#'                       end_year = 2024
#'                       outputPathDataSets = "path/to/output/folder")
#'
#' }
#'
#'
#' @return ecodata::comdat data frame
#'
#' @section Dependencies:
#' 
#' This assumes that the commercial data has been pulled and resides in the path `inputPathBennet`
#' and that create_menhaden_input.R has been run and outputs saved to `menhaden_path`
#'
#' @export



workflow_comdat <- function(comdat_path,
                            report_year,
                            end_year,
                            input_path_species,
                            menhaden_path,
                            outputPathDataSets = NULL) {
  
  
  # Add check to skip running workflow if data not present
  if(file.exists(comdat_path) && file.exists(input_path_species) && (!is.null(outputPathDataSets))) {
    
    indicatorData <- SOEworkflows::create_comdat(comdat_path = comdat_path,
                                                 input_path_species =  input_path_species,
                                                 menhaden_path = menhaden_path)
    # write data to file
    saveRDS(indicatorData,paste0(outputPathDataSets,"/comdat.rds"))
  } else {
    #
    message("One or more of the input files are not present in the location specified")
  }
}
