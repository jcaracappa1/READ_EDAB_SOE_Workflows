#' Calculates bennet data set for automated workflow
#'
#' This uses the commercial data pull from the comlandr package.
#' It is formatted exactly like the ecodata data object
#'
#' @param inputPathBennet Character string. Full path to the commercial data rds file for bennet indicator
#' @param inputPathSpecies Character string. Full path to the species list data pull rds file
#' @param outputPath Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::bennet indicator
#' workflow_bennet(inputPathBennet = "path/to/commerical_bennet.rds",
#'                       inputPathSpecies = "path/to/species/data/.rds",
#'                       outputPath = "path/to/output/folder")
#'
#' }
#'
#'
#' @return ecodata::bennet data frame
#'
#' @section Dependencies:
#' 
#' This assumes that the commercial data has been pulled and resides in the path `inputPathBennet`
#'
#' @export



workflow_bennet <- function(inputPathBennet, inputPathSpecies, outputPath = NULL) {
  
  # Assumes that commercial data has been pulled
  #get_commercial_data(channel,outputPathDatasets = outputPath)
  
  # Add check to skip running workflow if data not present
  if(file.exists(inputPathBennet) && file.exists(inputPathSpecies) && (!is.null(outputPath))) {
    
    indicatorData <- SOEworkflows::create_bennet(inputPathBennet = inputPathBennet,
                                                 inputPathSpecies = inputPathSpecies)
    # write data to file
    saveRDS(indicatorData,paste0(outputPath,"/bennet.rds"))
  } else {
    #
    message("One or more of the input files are not present in the location specified")
  }
}
