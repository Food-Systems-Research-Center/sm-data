> [!NOTE]
> This project is in development. The framework, metrics, and analyses are all preliminary.

_last updated 2025-07-04_

# Sustainability Metrics Data

Author: Chris Donovan

Contact: [christopher.donovan@uvm.edu](mailto:christopher.donovan@uvm.edu)

Created On October 15th, 2024

## Table of Contents

- [Sustainability Metrics Data](#sustainability-metrics-data)
  - [Table of Contents](#table-of-contents)
  - [About](#about)
  - [Setting Up the Project](#setting-up-the-project)
    - [Clone the Repository](#clone-the-repository)
    - [Open Project](#open-project)
    - [API Keys](#api-keys)
    - [Python Environment](#python-environment)
    - [Load SMdata Package](#load-smdata-package)
  - [Navigation](#navigation)
  - [Running the Project](#running-the-project)
  - [Data and Licensing](#data-and-licensing)

## About

The Sustainability Metrics project is a collaborative effort to measure food system sustainability in New England led by the University of Vermont (UVM) UVM Food Systems Research Center (FSRC). We work with five teams of investigators conducting primary research in vital aspects of local and regional food systems. To support this work, we are also exploring secondary data sources.

This repository houses an R project which collects, cleans, and compiles secondary data from a variety of sources in service of the Sustainability Metrics project. Clean exports of data are sent to [SMdocs](https://www.github.com/Food-Systems-Research-Institute/SMdocs) and [SMexplorer](https://www.github.com/Food-Systems-Research-Institute/SMexplorer).

## Setting Up the Project

### Clone the Repository

```bash
git clone https://github.com/Food-Systems-Research-Institute/SMdata.git
```

### Open Project

Open the `.Rproj` file with RStudio and restore packages from the `renv` lockfile:

```r
if (!require('renv')) install.packages('renv')
library('renv')
renv::restore()
```

### API Keys

Note that the project requires around 45GB of tabular and spatial data to run. This should be whittled down at some point to only the files that we really use, but we haven't gotten there yet. While the project expects the data to be housed in the `1_raw` directory, little of it is pushed to the GitHub repository for size constraints. Instead, you can download it from [OneDrive](https://uvmoffice-my.sharepoint.com/:u:/g/personal/swalshda_uvm_edu/ETMgUnpyIFdImfhaBt_hFA8BCZ3I8Fotb11s14FpVskEMQ?e=gUCA3s). Once downloaded, put the contents of the zip file into `1_raw` and the project should work.

There are also various API calls in the project, some of which require an API key to run, including USDA NASS, the US Census, and the Bureau of Economic Analysis. To be able to run these calls, request the appropriate API keys ([NASS](https://quickstats.nass.usda.gov/api), [Census](https://api.census.gov/data/key_signup.html), [BEA](https://apps.bea.gov/API/signup/)), and add them to the `.Renviron` file as `x_API_KEY` so that they will be called in the appropriate scripts.

### Python Environment

There are some Python functions used in spatial analysis. To reproduce the virtual environment, use:

```bash
python -m venv .venv
pip install -r requirements.txt
```

If you are running Python functions through reticulate, there is one more step to make sure it is accessing the same environment. Set the `.Renviron` file to specify the path to the environment with:

```bash
echo "RETICULATE_PYTHON=.venv/Scripts/python.exe" >> .Renviron
```

These paths may change slightly depending on what system you are on. Functional paths for Windows are shown.

### Load SMdata Package

The last step in setting up the project is to load the `SMdata` package in R, using `devtools::load_all()`, `ctrl + shirt + l`, or running the `setup.R` script found at the top of the `table_of_contents.R`. 

The SMdata package can also be downloaded from GitHub:

```r
if (!require(devtools)) install.packages('devtools')
devtools::install_github('Food-Systems-Research-Institute/SMdata')
```

This provides easy access to the secondary data metrics, metadata, other useful objects like FIPS keys, as well as a set of utility functions for working with the data.

## Navigation

After opening the `SMdata.Rproj` file, head to `table_of_contents.R`, which outlines the workflow of the project. To navigate to a script, you can either use `CTRL/CMD + LEFT CLICK` on a file path or put the cursor into the file path and hit `F2`. Once in a script, bring up the navigation pane with `CTRL/CMD + SHIFT + O`, or jump to a section with `ALT + SHIFT + J`. Note that each script loads the relevant objects at the start and saves them at the end. So, you can jump into any script in the project without running all previous scripts. However, within each script, code must be run sequentially, starting with the housekeeping section. 

The file structure is organized as follows:

-   `1_raw`: Downloads of bulk files, datasets, and spatial layers before any processing has taken place. Note that these files are large, and currently not pushed to this repo. See below regarding access.
-   `2_clean`: Cleaned, wrangled, compiled datasets and spatial layers ready for exploratory analysis. (But really all the datasets to be exported are in `6_outputs`. This is a bit jenky.)
-   `3_functions`: Functions that are used throughout the project. These are sourced in the housekeeping section of each script as needed.
-   `4_scripts`: R scripts for API calls, wrangling, metadata creation, and exporting to `SMdocs` and `SMexplorer` repos.
-   `5_objects`: Intermediate objects worth saving. Results from API calls, before processing, are saved here under `api_outs`. 
-   `6_outputs`: Zipped `.csv` files of metrics and metadata as well as `.rds` files to be used in other projects.

## Running the Project

Once `SMdata` is set up and loaded, you can either navigate the project by running scripts in the `table_of_contents.R` file, or by using a series of internal functions:

- `sm_call_apis()` will run all the API calls that collect data for the project. Note that there are plenty of bulk CSVs involved as well, so not everything will be updated automatically. 
- `sm_wrangle()` will run all the cleaning and wrangling scripts for each dataset.
- `sm_export()` will aggregate and export data in various formats.
- `sm_update()` will update the datasets made available by the `SMdata` package (to be run after `sm_export()`). 

Documentation can be found for both internal and exported functions (`?sm_wrangle()`). `sm_` function calls roughly follow the sections of the `table_of_contents.R` script. 

The important outputs are found in `6_outputs`, including:

- `metrics_and_metadata.zip`: One `.csv` file for each
- `sm_data.rds`: A list that contains metrics, metadata, and useful intermediate objects like keys to FIPS codes
- `sm_spatial.rds`: A list that contains polygons for counties, states, and the entire Northeast US. Counties from both 2021 and 2024 are included to accommodate the change in Connecticut from counties to governance regions circa 2022.

## Data and Licensing

<div style="display: flex; align-items: center;">
  <a rel="license" href="https://www.gnu.org/licenses/gpl-3.0.en.html#license-text">
    <img alt="GPLv3 License" style="border-width:0; margin-right: 10px;" src="https://www.gnu.org/graphics/gplv3-or-later-sm.png" />
  </a>
  <span>
    The code in this project is licensed under the 
    <a rel="license" href="https://www.gnu.org/licenses/gpl-3.0.en.html#license-text">GNU General Public License v3</a>.
  </span>
</div>
<br>

<div style="display: flex; align-items: center;">
  <a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/">
    <img alt="Creative Commons Licence" style="border-width:0; margin-right: 10px;" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" />
  </a>
  <span> 
    Any modeled, derived, or otherwise processed datasets created in this project are avaiable under the 
    <a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.
  </span>
</div>

Otherwise, data are available under licenses from the original sources. (Should add a list of those sources and licenses here at some point). 
