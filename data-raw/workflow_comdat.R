#' Calculates comdat data set for automated workflow
#'
#' This uses the commercial data pull from the comlandr package.
#'
#' @param comdat_path Character string. Full path to the commercial data rds file for comdat indicator
#' @param input_path_species Character string. Full path to the species list data pull rds file
#' @param outputPathDataSets Character string. Path to folder where data pull should be saved
#' @param menhaden_path Character string. Full path to the menhaden data .rds file
#'
#' @return ecodata::comdat data frame
#'
#' @example
#' \dontrun{
#' workflow_comdat(
#'    comdat_path = "path/to/commerical_comdat.rds",
#'    input_path_species = "path/to/species/data/.rds",
#'    menhaden_path = "path/to/menhaden/data/.rds",
#'    outputPathDataSets = "path/to/output/folder"
#'    )
#' }
#'
#' @section Dependencies:
#' 
#' This assumes that the commercial data has been pulled and resides in the path `comdat_path`
#' and that create_menhaden_input.R has been run and outputs saved to `menhaden_path`
#'
#' @export



workflow_comdat <- function(comdat_path,
                            input_path_species,
                            menhaden_path,
                            outputPathDataSets) {
  
  
  # Add check to skip running workflow if data not present
  if(file.exists(comdat_path) && 
     file.exists(input_path_species) && 
     file.exists(menhaden_path) &&
     !is.null(outputPathDataSets)) {
    
    indicatorData <- SOEworkflows::create_comdat(
      comdat_path = comdat_path,
      input_path_species =  input_path_species,
      menhaden_path = menhaden_path,
      outputPathDataSets = outputPathDataSets
      )
    
    return(indicatorData)
    
  } else {
    message("One or more of the input files are missing or the output path is NULL.")
    return(NULL)
  }
}
