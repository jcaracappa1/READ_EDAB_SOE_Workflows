#' Creates the stock status data frame for the SOE
#'
#' This function creates a stock status data frame that is formatted for inclusion in the `ecodata` R package.
#'
#' @param data the stock status data frame, typically from `stocksmart::stockAssessmentSummary`
#' @param decode a data frame that matches the stock names with a code abbreviation to use in plotting, typically read from a CSV file. If set to `FALSE`, the function will return a data frame in a less processed form, like Sarah used to provide in the `assess.csv` file.
#' @return a tibble
#'
#' @importFrom rlang .data
#' @export

create_stock_status <- function(data = stocksmart::stockAssessmentSummary,
                                decode) {
  # this will wrangle stocksmart data only (not PDB data)
  if ("Science Center" %in% names(data)) {
    data <- data |>
      dplyr::filter(.data$`Science Center` == "NEFSC") |>
      dplyr::rename(
        "Entity.Name" = "Stock Name",
        "Assessment.Year" = "Assessment Year",
        "F.Fmsy" = "F/Fmsy",
        "B.Bmsy" = "B/Bmsy"
      )
  }

  #   # remove eastern georges -- it's a management area associated with transboundary stocks,
  #   # not a biological stock
  assess <- data |>
    dplyr::filter(!stringr::str_detect(.data$Entity.Name, "Eastern Georges"))

  if (!isFALSE(decode)) {
    output <- assess |>
      SOEworkflows::join_decoder(decoder = decode)
    return(output)
  }

  return(assess)
}

#' Joins the stock status stock abbreviation lookup table
#'
#' This function joins the stock status stock abbreviation lookup table.
#'
#' @param data the stock status data frame, typically from `stocksmart::stockAssessmentSummary`. Must have columns named `Entity.Name`, `Assessment.Year`, `F.Fmsy`, and `B.Bmsy`.
#' @param decoder a data frame that links the stock names with a code abbreviation to use in plotting, typically read from a CSV file. If set to `FALSE`, the function will return a data frame in a less processed form, like Sarah used to provide in the `assess.csv` file.
#' @return a tibble
#'
#' @importFrom rlang .data
#' @export

join_decoder <- function(data,
                         decoder) {
  output <- data |>
    dplyr::group_by(.data$Entity.Name) |>
    dplyr::filter(.data$Assessment.Year == max(.data$Assessment.Year)) |>
    # Find last year assessment occurred for each stock
    dplyr::ungroup() |>
    dplyr::left_join(decoder, by = "Entity.Name") |> # Join in list of managed species
    dplyr::select(.data$Entity.Name, .data$Assessment.Year, .data$F.Fmsy, .data$B.Bmsy, .data$Council, .data$Code) |>
    tidyr::pivot_longer(
      cols = c(.data$F.Fmsy, .data$B.Bmsy),
      names_to = "Var",
      values_to = "Value"
    ) |>
    dplyr::mutate(Assessment.Year = as.integer(Assessment.Year)) |> 
    dplyr::rename(
      "Last assessment" = "Assessment.Year",
      "Stock" = "Entity.Name"
    ) |> # rename variables for clarity
    dplyr::mutate(
      Units = "unitless",
      EPU = dplyr::case_when(
        Council == "MAFMC" ~ "MAB",
        Council == "NEFMC" ~ "NE",
        TRUE ~ Council
      )
    )

  missing_codes <- dplyr::anti_join(
    output,
    decoder,
    by = c("Stock" = "Entity.Name")
  ) |>
    dplyr::select(.data$Stock, .data$Council, .data$Code)

  if (nrow(missing_codes) > 0) {
    warning(
      "Some stocks do not have matches in the decode table: \n",
      paste0(missing_codes$Stock |> unique(), collapse = ", ")
    )
  }
  return(output)
}
