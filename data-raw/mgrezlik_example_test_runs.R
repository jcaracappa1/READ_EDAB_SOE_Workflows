# Example of how to test on the Rstudio container 
outputPathDataSets <- "/home/mgrezlik/EDAB_Dev/grezlik"
outputPath <- "/home/mgrezlik/EDAB_Dev/grezlik"
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
comland_old_path <- '/home/mgrezlik/EDAB_Dev/beet/comlandr_old.rds'
old_menh_path24 <- '/home/mgrezlik/EDAB_Dev/grezlik/menhadenEOF2024.rds'
old_menh_path <- '/home/mgrezlik/EDAB_Dev/grezlik/menhadenEOF.rds'
old_comdat_path <- '/home/mgrezlik/EDAB_Dev/grezlik/Commercial_data_pull_25.RData'


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

# comparison plots using ecodata::plot_comdat() as a template ---------------
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

    land_ylabdat <- expression("Landings (10"^3 * " metric tons)")
    
  ## DATA WRANGLING FOR REVENUE ----
    
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
  
  ## PLOTTING ----
 
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

    
## saving plots -----------
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
    
    
    
# Menhaden comparisons ----------------------------
    
# plots show difference in landings of MAB planktivores
# comparing my data to old menhaden data to see if its a menhaden issue
    
## workflow menhaden ------------------
    ## call in raw data
    ### remove header rows with meta data
    menh <- readxl::read_excel(menhaden_path, sheet = 1, skip = 6)
    
    ## Mid-Atlantic  
    mid.men <- menh |>
      dplyr::select(1, 5) |> 
      # Now rename the columns to something clean and predictable
      dplyr::rename(YEAR = 1, SPPLIVMT = 2) |>
      dplyr::mutate(MONTH = 1, NESPP3 = 221, NEGEAR = 0, TONCL2 = NA, EPU = 'MAB', UTILCD = 9,
                    MARKET_CODE = 'UN', MESHCAT = NA, SPPVALUE = 0, US = T)
    
    
    
    ## GOM
    gom.men <- menh |>
      dplyr::select(1, 6) |>
      # Rename the columns
      dplyr::rename(YEAR = 1, SPPLIVMT = 2) |>
      dplyr::mutate(NESPP3 = 221, MONTH = 1, NEGEAR = 0, TONCL2 = NA, EPU = 'GOM',
                    UTILCD = 7, MARKET_CODE = 'UN', MESHCAT = NA,
                    SPPVALUE = 0, US = T)
    
    
    ## combine mid.men and gom.men
    menhaden_max <- dplyr::bind_rows(mid.men, gom.men)    

    
## old menhaden 24----------------------------
    # using script from https://github.com/NOAA-EDAB/SOE_data/blob/main/R/create_comdat.R
    #Add Menhaden directly
    old_menh24 <- data.table::as.data.table(readRDS(old_menh_path24))
    #Get in same format as comland
    #Mid-Atlantic
    mid.men <- old_menh24[, list(year, MABcatch)]
    data.table::setnames(mid.men, c('year', 'MABcatch'), c('YEAR', 'SPPLIVMT'))
    mid.men[, MONTH := 1]
    mid.men[, NESPP3 := 221]
    mid.men[, NEGEAR := 0] #Double check
    mid.men[, TONCL2 := NA] #Double check
    mid.men[, EPU := 'MAB']
    mid.men[, UTILCD := 9]
    mid.men[, MARKET_CODE := 'UN']
    mid.men[, MESHCAT := NA]
    mid.men[, SPPVALUE := 0]
    mid.men[, US := T]
    
    #GOM
    gom.men <- old_menh24[, list(year, GOMcatch)]
    data.table::setnames(gom.men, c('year', 'GOMcatch'), c('YEAR', 'SPPLIVMT'))
    gom.men[, NESPP3 := 221]
    gom.men[, MONTH := 1]
    gom.men[, NEGEAR := 0]
    gom.men[, TONCL2 := NA]
    gom.men[, EPU := 'GOM']
    gom.men[, UTILCD := 7]
    gom.men[, MARKET_CODE := 'UN']
    gom.men[, MESHCAT := NA]
    gom.men[, SPPVALUE := 0]
    gom.men[, US := T]
      
    old_menh24 <- data.table::rbindlist(list(mid.men, gom.men), use.names = T)

## old menhaden ----------------------------
    # using script from https://github.com/NOAA-EDAB/SOE_data/blob/main/R/create_comdat.R
    #Add Menhaden directly
    old_menh <- data.table::as.data.table(readRDS(old_menh_path))
    #Get in same format as comland
    #Mid-Atlantic
    mid.men <- old_menh[, list(year, MABcatch)]
    data.table::setnames(mid.men, c('year', 'MABcatch'), c('YEAR', 'SPPLIVMT'))
    mid.men[, MONTH := 1]
    mid.men[, NESPP3 := 221]
    mid.men[, NEGEAR := 0] #Double check
    mid.men[, TONCL2 := NA] #Double check
    mid.men[, EPU := 'MAB']
    mid.men[, UTILCD := 9]
    mid.men[, MARKET_CODE := 'UN']
    mid.men[, MESHCAT := NA]
    mid.men[, SPPVALUE := 0]
    mid.men[, US := T]
    
    #GOM
    gom.men <- old_menh[, list(year, GOMcatch)]
    data.table::setnames(gom.men, c('year', 'GOMcatch'), c('YEAR', 'SPPLIVMT'))
    gom.men[, NESPP3 := 221]
    gom.men[, MONTH := 1]
    gom.men[, NEGEAR := 0]
    gom.men[, TONCL2 := NA]
    gom.men[, EPU := 'GOM']
    gom.men[, UTILCD := 7]
    gom.men[, MARKET_CODE := 'UN']
    gom.men[, MESHCAT := NA]
    gom.men[, SPPVALUE := 0]
    gom.men[, US := T]
    
    old_menh <- data.table::rbindlist(list(mid.men, gom.men), use.names = T)    
    
    
    
## sg_menh ---------------------------------------------------------
    # created menhadenEOF_SG.rds using Sarah's script as a template
    # get_menhaden.R
    #Add Menhaden directly
    sg_menh <- data.table::as.data.table(readRDS("/home/mgrezlik/EDAB_Dev/grezlik/menhadenEOF_SG.rds"))
    #Get in same format as comland
    #Mid-Atlantic
    mid.men <- sg_menh[, list(year, MABcatch)]
    data.table::setnames(mid.men, c('year', 'MABcatch'), c('YEAR', 'SPPLIVMT'))
    mid.men[, MONTH := 1]
    mid.men[, NESPP3 := 221]
    mid.men[, NEGEAR := 0] #Double check
    mid.men[, TONCL2 := NA] #Double check
    mid.men[, EPU := 'MAB']
    mid.men[, UTILCD := 9]
    mid.men[, MARKET_CODE := 'UN']
    mid.men[, MESHCAT := NA]
    mid.men[, SPPVALUE := 0]
    mid.men[, US := T]
    
    #GOM
    gom.men <- sg_menh[, list(year, GOMcatch)]
    data.table::setnames(gom.men, c('year', 'GOMcatch'), c('YEAR', 'SPPLIVMT'))
    gom.men[, NESPP3 := 221]
    gom.men[, MONTH := 1]
    gom.men[, NEGEAR := 0]
    gom.men[, TONCL2 := NA]
    gom.men[, EPU := 'GOM']
    gom.men[, UTILCD := 7]
    gom.men[, MARKET_CODE := 'UN']
    gom.men[, MESHCAT := NA]
    gom.men[, SPPVALUE := 0]
    gom.men[, US := T]
    
    sg_menh <- data.table::rbindlist(list(mid.men, gom.men), use.names = T)    
    
    
    
## combining menh sources for plotting --------------------
    
    menh_comp_max <- menhaden_max |> 
                        dplyr::mutate(source = 'excel_from_scott')

    menh_comp_old24 <- old_menh24 |> 
                        dplyr::mutate(source = 'menhadenEOF2024.rds')
    
    menh_comp_old <- old_menh |> 
      dplyr::mutate(source = 'menhadenEOF.rds')
    
    menh_comp_sg <- sg_menh |> 
      dplyr::mutate(source = 'get_menhaden.R')

    menh_comp_combined <- dplyr::bind_rows(menh_comp_old, menh_comp_sg)    

## plot time series -----------------------------
    
    plot_menh_compare <- menh_comp_combined |>
      ggplot(aes(x = YEAR, y = SPPLIVMT, color = source)) +
      geom_line() +
      facet_wrap( ~ EPU, scales = "free_y") +
      labs(y = 'Landings (10^3 metric tons)', x = NULL) +
      ggtitle('Menhaden data comparison') +
      ecodata::theme_ts() +
      ecodata::theme_title() +
      ecodata::theme_facet() +
      theme(legend.position = "bottom", legend.title = element_blank())

## saving plot -----------
    # ggsave(
    #   filename = here::here('data-raw','menhaden_data.pdf'),
    #                         plot = plot_menh_compare
    # )
    
    
    
## menhadenEOF.rds comparison ---------------------------
    
# create_comdat.R used menhadenEOF.rds as menhaden input
# Sarah shared the script and input data which creates that file
# I created menhadenEOF_SG.rds using that script
# here I compare to last year's menhadenEOF.rds to make sure I am getting the same outputs
    
    
24menh <- 
    
    
    
    
    
    
    
    
# Revenue investigation -----------------------
    
# Revenue from the workflow is not matching what is in ecodata::comdat
# I suspect this is due to the difference in refYear in the
# comlandr::get_comland_data() call
# Testing that hypothesis here
    
## last year data ------------------------
    
# last year's data is created here: https://github.com/NOAA-EDAB/SOE_data/blob/main/R/create_comdat.R
# and saved here: https://github.com/NOAA-EDAB/ecodata/blob/master/data-raw/Commercial_data_pull_25.RData
    
load(old_comdat_path)

old_comm_data <- comdat |> dplyr::mutate(pull = 'Commercial_data_pull_25.RData')  
# data goes up to 2023, so end.year must be 2023

## workflow data ------------------------

new_comm_data <- commercial_summary |> dplyr::mutate(pull = 'workflow')
# data goes up to 2025, end.year = 2025
# refYear = end.year-1 so 2024

## combine data sets to compare --------------

comp_comm_data <- dplyr::bind_rows(old_comm_data, new_comm_data)

## plot comparison --------------------

comp_revenue <- comp_comm_data |>
  dplyr::filter(Var == 'Revenue') |> 
  ggplot(aes(x = Time, y = Value, color = pull)) +
  geom_line() +
  facet_wrap( ~ Region, scales = "free_y") +
  labs(y = "Revenue (10^6 * USD)", x = NULL) +
  ggtitle('create_comland output comparison') +
  ecodata::theme_ts() +
  ecodata::theme_title() +
  ecodata::theme_facet() +
  theme(legend.position = "bottom", legend.title = element_blank())


## old_comlandr -------------------------
# Andy created an 'old comlandr pull' which uses the old method of adjusting 
# for inflation. comparing that to Commercial_data_pull_25.RData and workflow

old_commercial_summary <- create_comdat(
  comdat_path <- '/home/mgrezlik/EDAB_Dev/beet/comlandr_old.rds',
  report_year = 2025,
  end_year = 2024,
  input_path_species <- "/home/mgrezlik/EDAB_Datasets/SOE_species_list_24.rds",
  menhaden_path <- "/home/mgrezlik/EDAB_Datasets/AM Landings by Gaichas Regions 1967-2024.xlsx"
)

old_comdat <- get_comdat(
  processed_comdat = old_commercial_summary, 
  save_for_package = FALSE # Set to FALSE to see the result directly
)

# comparing comdat to old comdat

comdat_max <- comdat |> 
  dplyr::mutate(source = 'max')

comdat_old_comlandr <- old_comdat |> 
  dplyr::mutate(source = 'old_comlandr')

# comdat_compare <- dplyr::bind_rows(comdat_max, comdat_old_comlandr)

# comparing old comdat to ecodata

comdat_compare <- dplyr::bind_rows(comdat_old_comlandr, comdat_ecodata)



setup <- ecodata::plot_setup(shadedRegion = NULL, report = "MidAtlantic")

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

land_ylabdat <- expression("Landings (10"^3 * " metric tons)")

## DATA WRANGLING FOR REVENUE ----

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

## PLOTTING ----

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


## saving plots -----------
# ggsave(
#   filename = here::here('data-raw','total_revenue_old_comlandr_ecodata.pdf'),
#                         plot = p_tot_rev
# )
# 
# ggsave(
#   filename = here::here('data-raw','guild_revenue_old_comlandr_ecodata.pdf'),
#   plot = p_guild_rev
# )
# 
# ggsave(
#   filename = here::here('data-raw','total_landings_old_comlandr_ecodata.pdf'),
#   plot = p_tot_land
# )
# 
# ggsave(
#   filename = here::here('data-raw','guild_landings_old_comlandr_ecodata.pdf'),
#   plot = p_guild_land
# )
