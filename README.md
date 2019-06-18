# Mortality-Adj-Bio-Age-Around-the-world
Implementation of Moshe A. Milvesky's paper "Calibrating Gompertz in Reverse: Mortality Adjusted Biological Age around the World"

# Folder Structure

## Scripts
- [00_merge_data.R](scripts/00_merge_data.R): where the data from Human Mortality Databases is merged into a single table
- [00_country_codes.R](scripts/00_country_codes.R): where a mapping between country codes and country names is generated based on the data-files
- [01_basic_model.R](scripts/01_models.R): where the paper is implemented (note that each stage omits its own data-frame)
- [02_paper_tables.R](scripts/02_paper_tables.R): where the tables that exist in the paper are generated
- [02_plots.Rmd](scripts/02_plots.Rmd): where the plots that exist in the paper are generated


## Data
- [00_raw](data/): this folder is not uploaded to here, ineased I made the zip file with the contents of the data avaiblable here. This is due to size-limiatation.
- [01_processed](data/01_processed): this folder contains data extracted from the scripts prefixed with "00".
- [02_models](data/02_models): the raw-results that each regression step generated are stored here, as well as the computation for B-Age.
- [02_paper_tables](data/02_paper_tables): this is where the tables present in the paper are stored in csv format
