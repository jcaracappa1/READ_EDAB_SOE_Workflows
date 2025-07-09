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

## Commercial Laindings Based Indicators

### comdat


