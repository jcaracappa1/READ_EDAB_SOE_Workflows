## Indicator Dependencies

All relevant package dependencies are listed in the DESCRIPTIONS file under `Imports`.
When all indicators have been added to the repo we can provide a list of package versions

### aggregate_biomass

The survey data will pulled from the Oracle database via a cron job (quarterly?) using R function

```
SOEworkflows::get_survey_data(channel,outputPathDatasets)
```

* `channel` is a connection object created using `ROracle::dbConnect()`
* `outputPathDatasets` is the path to the folder where "raw" data is stored (`EDAB_Datasets`)
* The name of the data file is hardcoded (and can be renamed!) as `surveyNoLengths.rds`

To run the "workflow" using R the survey data (above) must already be present.
*Note: the following function resides in the folder `data-raw` and is NOT part of the package*

```
workflow_aggregate_biomass(outputPath,inputPathSurvey,inputPathSpecies)
```

* `inputPathSpecies` is the path to static data set `EDAB_Datasets/SOE_species_list_24.rds`.
* `inputPathSurvey` is the path to the dynamically created survey data `EDAB_Datasets/surveyNoLengths.rds`. 
* `outputPath` is the path to folder where indicator data should be saved, `EDAB_Indicators`.
The rds file name is hardcoded as `aggregate_biomass.rds`

### comdat