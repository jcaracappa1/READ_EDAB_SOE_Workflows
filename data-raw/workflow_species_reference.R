#' Script to generate data-raw/species_reference_table.csv 
#' Uses a combination of existing tables:
#' - ecodata/data-raw/species_groupings_V2.csv -> SOE_Workflows/data-raw/species_reference_ecodata.csv
#' - stockstatusindicator/data-raw/2024decoder.csv -> SOE_Workflows/data-raw/species_reference_stockstatusindicator.csv
#' - spreadsheet on EDAB Google Drive -> SOE_Workflows/data-raw/BTS_stock_strata_matrix.csv

library(dplyr)

##Read in ecodata species groupings
spp.ecodata = read.csv(here::here("data-raw","species_reference_ecodata.csv"), stringsAsFactors = FALSE) |> 
  #Only want distinct COMNAME, SVSPP< ITISSPP, NESPP3, and SCINAME combinations
  dplyr::distinct(COMNAME, SVSPP, ITISSPP, NESPP3, SCINAME, .keep_all = TRUE)

##Read in stockstatusindicator groups
##Note: Not sure about this one - don't want to match on only text name. need to find source
spp.stockstatus = read.csv(here::here("data-raw","species_reference_stockstatusindicator.csv"), stringsAsFactors = FALSE)

##Read in BTS stock strata matrix and format
spp.bts = read.csv(here::here("data-raw","BTS_stock_strata_matrix.csv"), stringsAsFactors = FALSE)
#strip the leading "X" from column names in spp.bts.cn
spp.bts.cn = spp.bts.cn = gsub("^X", "", colnames(spp.bts))
#Remove and columns with leading "."
spp.bts.cn = gsub("^\\.", "", spp.bts.cn)
#rename spp.bts columsn with spp.bts.cn
colnames(spp.bts) = spp.bts.cn
#Long format spp.bts to ITISPP and stockname
spp.bts.long = spp.bts |> 
  tidyr::pivot_longer(cols = -c(STRATUM, STRATUM_AREA), names_to = "dum", values_to = "value") |> 
  #split dum to ITISPP and stockname
  tidyr::separate(dum, into = c("ITISSPP.char", "stockname"), sep = "\\.", remove = T) |> 
  dplyr::mutate(ITISSPP = as.numeric(ITISSPP.char)) |> 
  dplyr::filter(!is.na(ITISSPP)) |> 
  dplyr::select(ITISSPP, stockname, STRATUM, STRATUM_AREA, present) 

#Join spp.ecodata with spp.bts.long
spp.ref = spp.bts.long |> 
  dplyr::left_join(spp.ecodata, by = "ITISSPP", relationship = "many-to-many") |> 
  dplyr::select(-SOE_17)

write.csv(spp.ref, 
          here::here("data-raw", "species_reference_table.csv"), 
          row.names = FALSE, 
          na = "")


