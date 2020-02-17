# Mortality-Adj-Bio-Age-Around-the-world
Implementation of "Calibrating Gompertz in Reverse: What is Your Longevity-Risk-adjusted Global Age", By: Moshe A. Milevsky

# Prerequisite

- Install R and R studio
- Install the [packrat](https://rstudio.github.io/packrat/) library (`install.packages("packrat")`)
- Open Rstudio using the `Mosh-Book.Rproj` file
- Run `packrat::init()`

# Folder Structure

## Scripts
- [00_method.R](scripts/00_method.R): the primary methods to compute the models in the paper
- [00_data](scripts/00_data): contains files that construct the data-set from the data extracted from the mortality database
    - [construct_dataset.R](scripts/00_data/construct_dataset.R): where the data from Human Mortality Databases is merged into a single table
    - [00_country_codes.R](scripts/00_data/country_codes.R): where a mapping between country codes and country names is generated based on the data-files
- [01_analysis](scripts/01_analysis): all scripts that run analysis are located here.
    - [_makeham_zero.R](scripts/01_makeham_zero.R): attempting to fit the model with zero-makeham
    - [2011_model.R](scripts/01_analysis/2011_model.R): where the paper is implemented (note that each stage omits its own data-frame)
    - [historic_models.R](scripts/01_analysis/historic_models.R): where the stage1 model is implemented for all data from 1900 to 2011
   - [historic_prep_display_data.R](scripts/01_analysis/historic_prep_display_data.R): where the historical data is preped for the display scripts in 02_display
- [02_display](scripts/02_display): any generation of custom latex or images can be found here
    - [00_methods.R](scripts/02_display/00_methods.R): generic display methods
    - [2011_plots.R](scripts/02_display/2011_plots.R): 2011 plots
    - [2011_tables_in_latex.R](scripts/02_display/2011_tables_in_latex.R): tables 1/2/3 in latex form
    - [historic_plots.R](scripts/02_display/historic_plots.R): historical plots
    - [historic_tex_tables.R](scripts/02_display/historic_tex_tables.R): historical latex tables
- [04_errors](scripts/04_errors): error analysis plots

## Data
- [00_raw](data/00_raw): this folder is not uploaded to here, instead I made the upload process is replicated in this script [construct_dataset.R](scripts/00_data/construct_dataset.R). 
- [01_processed](data/01_processed): this folder contains data extracted from the scripts prefixed with "00".
- [02_models](data/02_models): the raw-results that each regression step generated are stored here, as well as the computation for B-Age.
- [02_display](data/02_display): this is where the tables present in the paper are stored in csv format
