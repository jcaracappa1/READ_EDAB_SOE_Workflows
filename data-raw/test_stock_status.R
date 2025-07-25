
# dir.create(here::here("data-raw/temp"))

inputPath <- "/home/atyrell/SOE_ESP_Data/READ_EDAB_SOE_Workflows/data-raw/temp/decoder.csv"
outputPath <- "/home/atyrell/SOE_ESP_Data/READ_EDAB_SOE_Workflows/data-raw/temp"

source(here::here("data-raw/workflow_stock_status.R"))

workflow_stock_status(outputPath = outputPath,
                      inputPath = inputPath)

# unlink(here::here("data-raw/temp"),
#        recursive = TRUE)
