<!-- badges: start -->
  [![R-CMD-check](https://github.com/NEFSC/READ_EDAB_SOE_Workflows/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NEFSC/READ_EDAB_SOE_Workflows/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

This repository is intended as a place to host any scripts or functions used within automated workflows that generate State of the Ecoystem indicators.

To contribute an indicator workflow to this repo, please follow the steps below:

-  Clone this repo
-  Switch to `dev` branch and pull
-  Build the package Ctrl + Shift + B
-  Create branch off `dev` for your indicator. Name the branch the same name as your indicator
-  Add a main wrapper workflow function in `data-raw` folder. Name it `workflow_<ecodata_dataset_name>.r`
    * This function should take only filepaths as arguments and should save the ecodata data set (see examples that already exist in the `data-raw` folder)
-  Add file in the `R` folder. Name this as `<ecodata_dataset_name>.r`. 
    * This file should contain the set of functions required to calculate the indicator. The format of the indicator should match the format as found in the `ecodata` package. You may need to look at the `ecodata/data-raw/get_<your_indicator>` function to see any additional formatting that is required
    * This/these functions should be called from the `workflow_<ecodata_dataset_name>.r` function
-  Use the `data-raw/example_test_runs.r` script to see how you might set up your functions.
    * All functions should only take file paths as arguments. All data dependencies should be read in by passing a file path to the data.
-  Check that there is a `create_ecodata_dataset_name.Rd` file in the `man` folder. If not, run `devtools::document()` to create and commit one.
-  Add any packages used to create the indicator in the `DESCRIPTION` file.
-  Add instructions in the `tempate_readme.md` file (in the root of this repo)
-  Final testing should be done inside your Rstudio container. (You'll need to repeat the first few steps; clone, pull, build)
-  To test your workflow use the `example_test_runs.r` as a template.
-  To test the R package, please build the package and then run `devtools::check()`, either from the command line or click the `check` button under the build tab in Rstudio
   - If any errors arise (you can ignore `Notes` and `warnings`),  please try to fix them prior to creating a pull request
-  When checks pass, create a pull request into the `dev` branch and assign two people as reviewers



This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
