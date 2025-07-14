# Example of how to test on the Rstudio container 
outputPathDataSets <- "/home/mgrezlik/EDAB_Dev/beet"
outputPath <- "/home/mgrezlik/EDAB_Dev/beet"
input_path_commercial_comdat <- "/home/mgrezlik/EDAB_Dev/beet/commercial_comdat.rds"
inputPathSurvey <- "/home/mgrezlik/EDAB_Dev/beet/surveyNoLengths.rds"
inputPathSpecies <- "/home/mgrezlik/EDAB_Datasets/SOE_species_list_24.rds"
# ditching camel case moving forward
input_path_species <- "/home/mgrezlik/EDAB_Datasets/SOE_species_list_24.rds"
inputPathAlbatross <- "/home/mgrezlik/EDAB_Dev/beet/albatrossData.rds"
inputPathBigelow <- "/home/mgrezlik/EDAB_Dev/beet/bigelowData.rds"
staticPath <-  "/home/mgrezlik/EDAB_Resources/"
menhaden_path <- "/home/mgrezlik/EDAB_Datasets/AM Landings by Gaichas Regions 1967-2024.xlsx"
comdat_path <- '/home/mgrezlik/EDAB_Dev/beet/commercial_comdat.rds'

source(here::here("data-raw/workflow_species_dist.R"))

# channel <- dbutils::connect_to_database("NEFSC_USERS","mgrezlik")

# rawData <- SOEworkflows::get_survey_data(channel,outputPathDataSets)

# indD <- workflow_species_distribution(outputPath = outputPath,
#                                    inputPathSurvey = inputPathSurvey,
#                                    inputPathSpecies = inputPathSpecies)


# testing comdat ---------------------

commercial_summary <- create_comdat(
  comdat_path <- '/home/mgrezlik/EDAB_Dev/beet/commercial_comdat.rds',
  report_year = 2025,
  end_year = 2024,
  input_path_species <- "/home/mgrezlik/EDAB_Datasets/SOE_species_list_24.rds",
  menhaden_path <- "/home/mgrezlik/EDAB_Datasets/AM Landings by Gaichas Regions 1967-2024.xlsx"
)

comdat <- get_comdat(
  processed_comdat = commercial_summary, 
  save_for_package = FALSE # Set to FALSE to see the result directly
)

## comparing my comdat outputs to old comdat ---------------
comdat_max <- comdat |> 
                dplyr::mutate(source = 'max')

comdat_ecodata <- ecodata::comdat |> 
                    dplyr::mutate(source = 'ecodata')

comdat_compare <- dplyr::bind_rows(comdat_max, comdat_ecodata)

difference_summary <- comdat_compare |>
  tidyr::pivot_wider(
    names_from = source,
    values_from = Value
  ) |>
  # Calculate both absolute and percentage difference
  dplyr::mutate(
    absolute_diff = max - ecodata,
    percent_diff = (max - ecodata) / ecodata * 100
  )


# Pipeline method has more observations than are found in ecodata (34037 compared to 26141)
# anti join to find observations in comdat_max that are not in comdat_ecodata

missing_from_ecodata <- dplyr::anti_join(
  comdat_max,
  comdat_ecodata,
  by = c("Time", "Var", "EPU")
)

# comparison plots using ecodata::plot_comdat() as a template
library(stringr)
library(dplyr)
library(ggplot2)
library(ecodata)

    
    # Filter for relevant variables, keeping the source distinct
    total_landings <- comdat_compare |>
      dplyr::filter(
        str_detect(Var, "Landings"),
        !str_detect(Var, "Seafood|US only"), # Exclude seafood-only and US-only aggregates
        Time >= 1982
      ) |>
      dplyr::mutate(
        feeding.guild = str_extract(Var, "^\\w+"),
        grouping = "Total"
      )
    
    managed_landings <- comdat_compare |>
      dplyr::filter(
        str_detect(Var, "managed species - Landings"),
        !str_detect(Var, "US only"),
        Time >= 1982,
        str_detect(Var, paste0("JOINT|", setup$council_abbr))
      ) |>
      dplyr::mutate(
        feeding.guild = str_extract(Var, "^\\w+"),
        grouping = "Council Managed"
      )
    
    # Prepare data for the "guild" plot
    guilddat <- dplyr::bind_rows(total_landings, managed_landings) |>
      dplyr::filter(!feeding.guild %in% c("Apex", "Other", "Landings")) |>
      dplyr::group_by(Time, EPU, source, feeding.guild, grouping) |>
      dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(feeding.guild = factor(feeding.guild, levels = setup$feeding.guilds))
    
    
    plot_guild_landings <- guilddat |>
      ggplot(aes(x = Time, y = Value, color = source, linetype = grouping)) +
      geom_line(linewidth = setup$lwd) +
      facet_grid(feeding.guild ~ EPU, scales = "free_y")
    
    
    
    # Prepare data for the "total" plot
    totdat <- guilddat |>
      dplyr::group_by(Time, EPU, source, grouping) |>
      dplyr::summarise(Value = sum(Value, na.rm = TRUE) / 1000, .groups = "drop") |>
      dplyr::rename(Var = grouping)
    
    plot_total_landings <- totdat |>
      ggplot(aes(x = Time, y = Value, color = source, linetype = Var)) +
      geom_line(linewidth = setup$lwd) +
      geom_point(aes(shape = Var), size = setup$pcex) +
      facet_wrap(~EPU, scales = "free_y")

    land_ylabdat <- expression("Revenue (10"^6 * " USD)")
    
  # DATA WRANGLING FOR REVENUE ----
    
    # Filter for relevant variables, keeping the source distinct
    total_revenue <- comdat_compare |>
      dplyr::filter(
        str_detect(Var, "Revenue"),
        !str_detect(Var, "managed|US only"),
        Time >= 1982
      ) |>
      dplyr::mutate(
        feeding.guild = str_extract(Var, "^\\w+"),
        grouping = "Total"
      )
    
    managed_revenue <- comdat_compare |>
      dplyr::filter(
        str_detect(Var, "managed species - Revenue"),
        !str_detect(Var, "US only"),
        Time >= 1982,
        str_detect(Var, paste0("JOINT|", setup$council_abbr))
      ) |>
      dplyr::mutate(
        feeding.guild = str_extract(Var, "^\\w+"),
        grouping = "Council Managed"
      )
    
    # Prepare data for the "guild" plot
    guilddat <- bind_rows(total_revenue, managed_revenue) |>
      dplyr::filter(!feeding.guild %in% c("Apex", "Other", "Revenue")) |>
      dplyr::group_by(Time, EPU, source, feeding.guild, grouping) |>
      dplyr::summarise(Value = sum(Value, na.rm = TRUE) / 1000, .groups = "drop") |>
      dplyr::mutate(feeding.guild = factor(feeding.guild, levels = setup$feeding.guilds))
    
    plot_guild_revenue <- guilddat |>
      ggplot(aes(x = Time, y = Value, color = source, linetype = grouping)) +
      geom_line(linewidth = setup$lwd) +
      facet_grid(feeding.guild ~ EPU, scales = "free_y")
    
    # Prepare data for the "total" plot
    totdat <- guilddat |>
      dplyr::group_by(Time, EPU, source, grouping) |>
      dplyr::summarise(Value = sum(Value, na.rm = TRUE) / 1000, .groups = "drop") |>
      dplyr::rename(Var = grouping)
    
    rev_ylabdat <- expression("Revenue (10"^6 * " USD)")
  
    plot_total_revenue <- totdat |>
      ggplot(aes(x = Time, y = Value, color = source, linetype = Var)) +
      geom_line(linewidth = setup$lwd) +
      geom_point(aes(shape = Var), size = setup$pcex) +
      facet_wrap(~EPU, scales = "free_y")
  
  # PLOTTING ----
 
    # Apply common aesthetics
    p_tot_rev <- plot_total_revenue +
      scale_x_continuous(breaks = seq(1980, 2020, by = 10), expand = c(0.01, 0.01)) +
      labs(y = rev_ylabdat, x = NULL) +
      ecodata::theme_ts() +
      ecodata::theme_title() +
      ecodata::theme_facet() +
      theme(legend.position = "bottom", legend.title = element_blank())
    
    p_guild_rev <- plot_guild_revenue +
      scale_x_continuous(breaks = seq(1980, 2020, by = 10), expand = c(0.01, 0.01)) +
      labs(y = rev_ylabdat, x = NULL) +
      ecodata::theme_ts() +
      ecodata::theme_title() +
      ecodata::theme_facet() +
      theme(legend.position = "bottom", legend.title = element_blank())
    
    p_tot_land <- plot_total_landings +
      scale_x_continuous(breaks = seq(1980, 2020, by = 10), expand = c(0.01, 0.01)) +
      labs(y = land_ylabdat, x = NULL) +
      ecodata::theme_ts() +
      ecodata::theme_title() +
      ecodata::theme_facet() +
      theme(legend.position = "bottom", legend.title = element_blank())
  
    p_guild_land <- plot_guild_landings +
      scale_x_continuous(breaks = seq(1980, 2020, by = 10), expand = c(0.01, 0.01)) +
      labs(y = land_ylabdat, x = NULL) +
      ecodata::theme_ts() +
      ecodata::theme_title() +
      ecodata::theme_facet() +
      theme(legend.position = "bottom", legend.title = element_blank())

    
# saving plots -----------
# ggsave(
#   filename = here::here('data-raw','total_revenue.pdf'),
#                         plot = p_tot_rev
# )
# 
# ggsave(
#   filename = here::here('data-raw','guild_revenue.pdf'),
#   plot = p_guild_rev
# )
# 
# ggsave(
#   filename = here::here('data-raw','total_landings.pdf'),
#   plot = p_tot_land
# )
# 
# ggsave(
#   filename = here::here('data-raw','guild_landings.pdf'),
#   plot = p_guild_land
# )
    