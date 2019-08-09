# Mortality-Adj-Bio-Age-Around-the-world
Implementation of Moshe A. Milvesky's paper "Calibrating Gompertz in Reverse: Mortality Adjusted Biological Age around the World"

# Folder Structure

## Scripts
- [00_method.R](scripts/00_method.R): the primary methods to compute the models in the paper
- [00_data](scripts/00_data): contains files that construct the data-set from the data extracted from the mortality database
    - [00_merge_data.R](scripts/00_data/merge_data.R): where the data from Human Mortality Databases is merged into a single table
    - [00_country_codes.R](scripts/00_data/country_codes.R): where a mapping between country codes and country names is generated based on the data-files
- [01_analysis](scripts/01_analysis): all scripts that run analysis are located here.
    - [_makeham_zero.R](scripts/01_makeham_zero.R): attempting to fit the model with zero-makeham
    - [2011_model.R](scripts/01_analysis/2011_model.R): where the paper is implemented (note that each stage omits its own data-frame)
    - [historic_models.R](scripts/01_analysis/historic_models.R): where the stage1 model is implemented for all data from 1900 to 2011
- [02_display](scripts/02_display): any generation of custom latex or images can be found here

## Data
- [00_raw](data/): this folder is not uploaded to here, ineased I made the zip file with the contents of the data avaiblable here. This is due to size-limiatation.
- [01_processed](data/01_processed): this folder contains data extracted from the scripts prefixed with "00".
- [02_models](data/02_models): the raw-results that each regression step generated are stored here, as well as the computation for B-Age.
- [02_display](data/02_display): this is where the tables present in the paper are stored in csv format
