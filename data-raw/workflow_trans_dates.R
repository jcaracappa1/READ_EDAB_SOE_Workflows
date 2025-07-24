#' Calculates trans_dates data set for automated workflow
#'
#' This uses a static input file from Kevin Friedland. 
#' This is expected to be replaced by a dynamically-produced SST input file that
#'  Kim H is tasked with. Scripts included here will be refactored to accept the 
#'  new SST input when available.
#' It is formatted exactly like the ecodata data object
#'
#' @param inputPath Character string. Full path to the SST input file from Kevin Friedland
#' @param outputPath Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::trans_dates indicator
#' workflow_trans_dates(inputPath = "path/to/input/data.csv",
#'                      outputPath = "path/to/output/folder")
#'
#' }
#'
#'
#' @return ecodata::trans_dates data frame
#'
#' @section Dependencies:
#' 
#' This assumes that the input data file from Kevin Friedland has been provided and resides in the path `inputPath`
#'
#' @export



workflow_trans_dates <- function(inputPath, outputPath = NULL) {
  
  # Assumes that input data has been provided
  
  # Add check to skip running workflow if data not present
  if(file.exists(inputPath) && (!is.null(outputPath))) {
    
    indicatorData <- SOEworkflows::create_trans_dates(inputPath = inputPath)

    # write data to file
    saveRDS(indicatorData,paste0(outputPath,"/trans_dates.rds"))
  } else {
    #
    message("The input file is not present in the location specified")
  }
}
