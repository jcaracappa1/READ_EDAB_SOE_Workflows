#' Calculates stock status for automated workflow
#'
#' This uses stock assessment data from the `stocksmart` R package.
#' It is formatted exactly like the ecodata data object
#'
#' @param inputPath Character string. Full path to a csv lookup table that joins species abbreviations with their formal stock names
#' @param outputPath Character string. Path to folder where data pull should be saved
#'
#' @example
#' \dontrun{
#' # create the ecodata::stock_status indicator
#' workflow_stock_status(inputPath = "path/to/decoder.csv",
#'                       outputPath = "path/to/output/folder")
#'
#' }
#'
#'
#' @return ecodata::stock_status data frame
#'
#' @section Dependencies:
#'
#' This assumes that the `stocksmart` R package has been updated with recent assessment data
#'
#' @export

workflow_stock_status <- function(inputPath, outputPath = NULL) {
  # Add check to skip inputPath workflow if data not present
  if (file.exists(inputPath) && (!is.null(outputPath))) {
    indicatorData <- SOEworkflows::create_stock_status(
      data = stocksmart::stockAssessmentSummary,
      decode = utils::read.csv(inputPath)
    )
    # write data to file
    saveRDS(indicatorData, paste0(outputPath, "/stock_status.rds"))
  } else {
    #
    message("The input file is not present in the location specified, or the output path is not specified.
            Please check the inputPath and outputPath parameters.")
  }
}
