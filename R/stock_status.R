#' Creates stocksmart-like object from files received from PDB
#'
#' This function reads in stock status files from PDB and returns a stocksmart-like object. It was created based on the files for spring 2024 MT assessments, which were sent via email from Kristan Blackhart to Scott. Dan Hennen created the files.
#'
#' @param dirfname A character string representing the directory where the stock status files are stored
#'
#' @return a stocksmart-like tibble
#'
#' @export

create_pdb <- function(dirfname) {
  # need ABBR name of stock from filename then read in other data
  fname <- stringr::str_split_i(dirfname, "/", -1) # last character in split
  ABBR <- stringr::str_remove(fname, "db.RData.csv")

  statdat <- read.csv(dirfname, header = FALSE)
  statdat[statdat == "?"] <- NA
  statdat <- statdat |>
    dplyr::filter(V2 != "") |>
    tidyr::pivot_wider(names_from = V1, values_from = V2) |>
    dplyr::mutate(`Stock / Entity Name` = ABBR) |>
    dplyr::rename(
      Entity.Name = `Stock / Entity Name`,
      # Fmsy = FMSY,
      F.Fmsy = `F/FMSY`,
      # Estimated.F = `Best F Estimate`,
      # Bmsy = BMSY,
      B.Bmsy = `B/BMSY` # ,
      # Estimated.B =
    )

  return(statdat)
}

#' Formats data received from PDB for merging with stocksmart data
#'
#' This function reads in stock status files from PDB and returns the data formatted for merging with stocksmart data.
#'
#' @param stocknames a data frame with stock names and their abbreviations, used to replace the the PDB stock `ABBR` with a stocksmart `Entity.Name` in the output
#' @param fnames a character vector of PDB file names to read in
#' @param year a numeric value representing the assessment year, which will be added to the output data frame
#'
#' @return a tibble
#'
#' @export

build_pdb <- function(stocknames,
                      fnames,
                      year) {
  # purrr map the list of directory paths with create_pdb into a dataframe
  output <- purrr::map_dfr(fnames, create_pdb) |>
    # add assessment year
    dplyr::mutate(Assessment.Year = year) |>
    # ensure numbers are numeric
    type.convert(as.is = TRUE)

  # replace stock ABBR with stocksmart name using lookup
  output$Entity.Name <- stocknames$Stock.Name[match(unlist(output$Entity.Name), stocknames$ABBR)]

  return(output)
}

#' Prepare stock status data for plotting
#'
#' This function wrangles the stock status data into a format suitable for plotting in the stock status vignette.
#'
#' @param data the stock status data frame
#' @param council a character vector of councils to filter by, default is c("MAFMC", "NEFMC"), which will return all stocks. Can specify only one council, e.g., "MAFMC" or "NEFMC", to filter to data for one council only.
#'
#' @return a tibble
#'
#' @export

prep_for_plotting <- function(data,
                              council = c("MAFMC", "NEFMC")) {
  output <- data |>
    mutate(Code = recode(Code, "Dogfish" = "Sp. Dogfish")) |>
    # spread(.,Var,Value) |>
    tidyr::pivot_wider(
      names_from = Var,
      values_from = Value
    ) |>
    filter(Council %in% c(council, "Both")) |>
    group_by(Stock) |>
    mutate(score = case_when(
      (B.Bmsy < 0.5) ~ "a",
      (F.Fmsy > 1) ~ "a",
      (F.Fmsy < 1 & B.Bmsy > 0.5 & B.Bmsy < 1) ~ "b",
      (F.Fmsy < 1 & B.Bmsy > 1) ~ "c"
    ))

  return(output)
}

#' Plot stock status data
#'
#' This function creates a Kobe plot of stock status. Data must be prepared with `prep_for_plotting()`
#'
#' @param data the stock status data frame (processed with `prep_for_plotting()`
#' @param xmax the maximum value for the x-axis
#' @param ymax the maximum value for the y-axis
#'
#' @return a ggplot
#'
#' @export

plot_stocksmart <- function(data, xmax, ymax) {
  plt <- data |>
    ggplot() +
    geom_vline(xintercept = 1, linetype = "dotted") +
    geom_vline(xintercept = 0.5, linetype = "dashed") +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_point(aes(
      x = B.Bmsy,
      y = F.Fmsy,
      shape = Council,
      color = score
    )) +
    geom_text_repel(
      aes(
        x = B.Bmsy, # geom_text_repel auto-jitters text around points
        y = F.Fmsy,
        label = Code,
        color = score
      ),
      show.legend = FALSE, nudge_y = -0.01, nudge_x = 0.05,
      max.overlaps = 20
    ) +
    scale_color_brewer(
      palette = "Dark2",
      breaks = stock_status$score
    ) +
    ylim(0, ymax) +
    xlim(0, xmax) +
    xlab(expression(~ B / B[msy])) +
    ylab(expression(~ F / F[msy])) +
    guides(color = FALSE) +
    theme_ts()

  return(plt)
}

#' Creates the stock status data frame for the SOE
#'
#' This function creates a stock status data frame that is formatted for inclusion in the `ecodata` R package.
#'
#' @param data the stock status data frame, typically from `stocksmart::stockAssessmentSummary`
#' @param decode a data frame that joins the stock names with a code abbreviation to use in plotting, typically read from a CSV file. If set to `FALSE`, the function will return a data frame in a less processed form, like Sarah used to provide in the `assess.csv` file.
#' @param merge_data an optional data frame to merge with the additional stock assessment data from PDB, e.g., for updates or additional stocks. *This data must be formatted in a stocksmart-like data frame*
#' @return a tibble
#'
#' @export

create_stock_status <- function(data = stocksmart::stockAssessmentSummary,
                                decode = read.csv(here::here("2024decoder.csv")),
                                merge_data = NULL) {
  assess <- data |>
    dplyr::filter(`Science Center` == "NEFSC") |>
    # janitor::clean_names() |>
    # dplyr::select(`Stock Name`, `Assessment Year`, `B/Bmsy`, `F/Fmsy`) |>
    dplyr::rename(
      Entity.Name = `Stock Name`,
      Assessment.Year = `Assessment Year`,
      F.Fmsy = `F/Fmsy`,
      B.Bmsy = `B/Bmsy`
    )

  if (!is.null(merge_data)) {
    # if we have to merge in data updates, add it to the assessment data
    # only add data that isn't already in stocksmart
    assess <- dplyr::left_join(assess,
      merge_data |>
        dplyr::select(`Entity.Name`, `Assessment.Year`, `B.Bmsy`, `F.Fmsy`),
      by = c("Entity.Name", "Assessment.Year")
    ) |>
      # drop values that are repeats (keep stocksmart values)
      dplyr::select(-c("B.Bmsy.y", "F.Fmsy.y")) |>
      dplyr::rename(
        B.Bmsy = B.Bmsy.x,
        F.Fmsy = F.Fmsy.x
      )
  }

  if (!isFALSE(decode)) {
    output <- assess |>
      dplyr::group_by(Entity.Name) |>
      dplyr::filter(Assessment.Year == max(Assessment.Year)) |>
      # Find last year assessment occurred for each stock
      dplyr::ungroup() |>
      dplyr::left_join(decode, by = "Entity.Name") |> # Join in list of managed species
      dplyr::select(Entity.Name, Assessment.Year, F.Fmsy, B.Bmsy, Council, Code) |>
      tidyr::pivot_longer(
        cols = c(F.Fmsy, B.Bmsy),
        names_to = "Var",
        values_to = "Value"
      ) |>
      dplyr::rename(
        `Last assessment` = Assessment.Year,
        Stock = Entity.Name
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
      decode,
      by = c("Stock" = "Entity.Name")
    ) |>
      dplyr::select(Stock, Council, Code)

    if (nrow(missing_codes) > 0) {
      warning(
        "Some stocks do not have matches in the decode table: \n",
        paste0(missing_codes$Stock |> unique(), collapse = ", ")
      )
    }

    return(output)
  }

  return(assess)
}
