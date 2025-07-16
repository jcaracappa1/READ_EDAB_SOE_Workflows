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

### comdat

To run the "workflow" below, it is assumed that the necessary raw data files 
(commercial landings, species lists, and Menhaden data) are available.

*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

The workflow is typically run by piping the output of `create_comdat` directly into `get_comdat`:

```R
create_comdat(comdat_path, report_year, end_year, input_path_species, menhaden_path) |>
  get_comdat(save_for_package = TRUE)
```

* `comdat_path` is the path to the raw, comprehensive commercial landings data file, e.g., `EDAB_Datasets/commercial_comdat.rds`.
* `report_year` is the year of the State of the Ecosystem report (e.g., 2025).
* `end_year` is the final year of data to be included in the analysis.
* `input_path_species` is the path to the species list used for grouping, e.g., `EDAB_Datasets/SOE_species_list_24.rds`.
* `menhaden_path` is the path to the Menhaden landings data Excel file provided by SEFSC.
* The final output is saved by `get_comdat` to the `data/` folder. The file name is hardcoded as `comdat.rda` to match the `ecodata` package dataset.
