# Mortality-Adj-Bio-Age-Around-the-world
Implementation of Moshe A. Milvesky's paper "Calibrating Gompertz in Reverse: Mortality Adjusted Biological Age around the World"

# Folder Structure

## Scripts
- [00_merge_data.R](scripts/00_merge_data.R): where the data from Human Mortality Databases is merged into a single table
- [00_country_codes.R](scripts/00_country_codes.R): where a mapping between country codes and country names is generated based on the data-files
- [01_basic_model.R](scripts/01_basic_model.R): where Stage 1 and 2 of the paper are implemented

## Data
- [00_raw](data/): this folder is not uploaded to here, ineased I made the zip file with the contents of the data avaiblable here. This is due to size-limiatation.
- [01_processed](data/01_processed): this folder contains data extracted from the scripts prefixed with "00".
- [02_paper_tables](data/02_paper_tables): this is where the tables present in the paper are stored in csv format
