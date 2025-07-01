#' Creates comdat indicator for SOE
#' 
#' @description
#' Creates a data frame of commercial fisheries data from the Northeast Shelf (NES) based on comlandr data.
#' 
#' 
#' 
#' @param input_path_commercial_comdat Character string. Full path to the comlandr data pull rds file
#' @param outputPath Character string. Path to folder where data pull should be saved
#' 
#' @return comdat data frame used in ecodata 
#' 
#' @section Dependencies:
#' 
#' This assumes that the commercial data has been pulled and resides in the path `input_path_commercial_comdat`
#' 
#' @examples
#' \dontrun{
#'   outputPath <- here::here()
#'   input_path_commercial_comdat <- "/home/<user>/EDAB_Dev/beet/commercial_comdat.rds"
#'   workflow_comdat(outputPath,input_path_commercial_comdat)
#' }
#' 

workflow_comdat <- function(outputPath,input_path_commercial_comdat) {
  
  # Assumes that comlandr data has been pulled and is located in input_path_commercial_comdat
  
  # Add check to skip running workflow if data not present
  if(file.exists(input_path_commercial_comdat)) {
    
    SOEworkflows::get_comdat(input_path_commercial_comdat = input_path_commercial_comdat,
                                              outputPath = outputPath)
  } else {
    # 
    message("The input file is not present in the location specified")
  }
}