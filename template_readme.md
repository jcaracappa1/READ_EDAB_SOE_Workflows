# Indicator Dependencies

All relevant package dependencies are listed in the DESCRIPTIONS file under `Imports`.
When all indicators have been added to the repo we can provide a list of package versions

## Survey Based Indicators

The survey data will pulled from the Oracle database via a cron job (quarterly?) using R function

```
SOEworkflows::get_survey_data(channel,outputPathDatasets)
```

* `channel` is a connection object created using `ROracle::dbConnect()`
* `outputPathDatasets` is the path to the folder where "raw" data is stored (`EDAB_Datasets`)
* Currently the data sets created are:
  - `surveyNoLengths.rds` - used in `aggregate_biomass`
  - `bigelowData.rds` - used in `survey_shannon`
  - `albatrosData.rds` - used in `survey_shannon`
  - `condition.rds` - used in `condition`

### aggregate_biomass

To run the "workflow" below, it is assumed that the "raw" survey data has been pulled using the 
`get_survey_data` function above.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_aggregate_biomass(outputPath,inputPathSurvey,inputPathSpecies)
```

* `inputPathSpecies` is the path to static data set `EDAB_Datasets/SOE_species_list_24.rds`.
* `inputPathSurvey` is the path to the dynamically created survey data `EDAB_Datasets/surveyNoLengths.rds`. 
* `outputPath` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `aggregate_biomass.rds` to match the `ecodata` package dataset

### survey_shannon

To run the "workflow" below, it is assumed that the "raw" survey data has been pulled using the 
`get_survey_data` function above.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_survey_shannon(outputPath,inputPathBigelow,inputPathAlbatross)
```

* `inputPathBigelow` is the path to static data set `EDAB_Datasets/bigelowData.rds`.
* `inputPathAlbatross` is the path to the dynamically created survey data `EDAB_Datasets/albatrossData.rds`. 
* `outputPath` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `survey_shannon.rds` to match the `ecodata` package dataset

### exp_n

To run the "workflow" below, it is assumed that the "raw" survey data has been pulled using the 
`get_survey_data` function above.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_exp_n(outputPath,inputPathBigelow,inputPathAlbatross)
```

* `inputPathBigelow` is the path to static data set `EDAB_Datasets/bigelowData.rds`.
* `inputPathAlbatross` is the path to static data set `EDAB_Datasets/albatrossData.rds`. 
* `outputPath` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `exp_n.rds` to match the `ecodata` package dataset

### species_dist

To run the "workflow" below, it is assumed that the "raw" survey data has been pulled using the 
`get_survey_data` function above.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_species_dist(outputPath,inputPathSurvey,inputPathSpecies)
```

* `inputPathSpecies` is the path to static data set `EDAB_Datasets/SOE_species_list_24.rds`.
* `inputPathSurvey` is the path to the dynamically created survey data `EDAB_Datasets/surveyNoLengths.rds`. 
* `outputPath` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `species_dist.rds` to match the `ecodata` package dataset

## Commercial Landings Based Indicators

The commercial data will pulled from the Oracle database via a cron job (quarterly?) using R function

```
SOEworkflows::get_commercial_data(channel,outputPathDatasets)
```

* `channel` is a connection object created using `ROracle::dbConnect()`
* `outputPathDatasets` is the path to the folder where "raw" data is stored (`EDAB_Datasets`)
* Currently the data sets created are:
  - `commercial_comdat.rds` - used in `comdat`
  - `commercial_bennet.rds` - used in `bennet`


### Bennet

To run the "workflow" below, it is assumed that the commercial data has been pulled using the 
`get_commercial_data` function above.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_bennet(inputPathBennet, inputPathSpecies, outputPath)
```

* `inputPathSpecies` is the path to static data set `EDAB_Datasets/SOE_species_list_24.rds`.
* `inputPathBennet` is the path to the dynamically created commercial data `EDAB_Datasets/commercial_benent.rds`. 
* `outputPath` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `bennet.rds` to match the `ecodata` package dataset


### comdat

To run the "workflow" below, it is assumed that the commercial data has been pulled using the 
`get_commercial_data` function above and that menhaden data have been pulled using the 
`create_menhaden_input.R` script in the folder `data-raw`.
*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```R
workflow_comdat(comdat_path, input_path_species, menhaden_path, outputPathDataSets)
```

* `comdat_path` is the path to the raw, comprehensive commercial landings data file, e.g., `EDAB_Datasets/commercial_comdat.rds`.
* `input_path_species` is the path to the species list used for grouping, e.g., `EDAB_Datasets/SOE_species_list_24.rds`.
* `menhaden_path` is the path to the Menhaden landings data output by create_`create_menhaden_input.R`.
* `outputPathDataSets` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `comdat.rds` to match the `ecodata` package dataset

## `stocksmart` Based Indicators

To run the "workflow" below, it is assumed that the `stocksmart` R package has been updated to include recent assessment data.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_stock_status(inputPath, outputPath)
```

* `inputPath` is the path to static data set `EDAB_Datasets/decoder.csv`.
* `outputPath` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `stock_status.rds` to match the `ecodata` package dataset


## Oceanographic Indicators

### Transition Dates (trans_dates)

To run the "workflow" below, it is assumed that the static input file from Kevin Friedland is present.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*


```
workflow_trans_dates(inputPath, outputPath)
```

* `inputPath` is the path to static data set `EDAB_Datasets/TS_SHP_adv rep MAB GOM GBK NES SCSPoly.csv`.
* `outputPath` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `trans_dates.rds` to match the `ecodata` package dataset
