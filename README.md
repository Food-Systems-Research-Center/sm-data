_last updated 2025-05-26_

> [!NOTE] This project is in development. The framework, metrics, and analyses are all preliminary.

# Sustainability Metrics Data

Author: Chris Donovan

Contact: [christopher.donovan@uvm.edu](mailto:christopher.donovan@uvm.edu)

Created On October 15th, 2024

## Table of Contents

- [Sustainability Metrics Data](#sustainability-metrics-data)
  - [Table of Contents](#table-of-contents)
  - [About](#about)
  - [Setting Up the Project](#setting-up-the-project)
  - [Navigation](#navigation)
  - [Running the Project](#running-the-project)
  - [Data and Licensing](#data-and-licensing)

## About


The Sustainability Metrics project is a collaborative effort to measure food system sustainability in New England led by the University of Vermont (UVM) UVM Food Systems Research Center (FSRC). We work with five teams of investigators conducting primary research in vital aspects of local and regional food systems. To support this work, we are also exploring secondary data sources.

This repository houses an R project which collects, cleans, and compiles secondary data from a variety of sources in service of the Sustainability Metrics project. Clean exports of data are sent to [sm-docs](https://www.github.com/food-systems-research-center/sm-docs) and [sm-explorer](https://www.github.com/food-systems-research-center/sm-explorer).

## Setting Up the Project

1. Clone the Git repository:

```bash
git clone https://github.com/Food-Systems-Research-Center/sm_data.git
```

2. Open the `.Rproj` file with RStudio and restore packages from the `renv` lockfile:

```r
if (!require('renv')) install.packages('renv')
library('renv')
renv::restore()
```

## Navigation

After opening the `sm-data.Rproj` file, head to `table_of_contents.R`, which outlines the workflow of the project. To navigate to a script, you can either use `CTRL/CMD + LEFT CLICK` on a file path or put the cursor into the file path and hit `F2`. Once in a script, bring up the navigation pane with `CTRL/CMD + SHIFT + O`, or jump to a section with `ALT + SHIFT + J`. Note that each script loads the relevant objects at the start and saves them at the end. So, you can jump into any script in the project without running all previous scripts. However, within each script, code must be run sequentially, starting with the housekeeping section. 

The file structure is organized as follows:

-   `1_raw`: Downloads of bulk files, datasets, and spatial layers before any processing has taken place. Note that these files are large, and currently not pushed to this repo. See below regarding access.
-   `2_clean`: Cleaned, wrangled, compiled datasets and spatial layers ready for exploratory analysis.
-   `3_functions`: Functions that are used throughout the project. These are sourced in the housekeeping section of each script as needed.
-   `4_scripts`: R scripts for API calls, wrangling, metadata creation, and exporting to `sm-docs` and `sm-explorer` repos.
-   `5_objects`: Intermediate objects worth saving. Results from API calls, before processing, are saved here under `api_outs`. 
-   `6_outputs`: CSV or figure outputs.

## Running the Project

Note that the project requires around 45GB of tabular and spatial data to run. This should be whittled down at some point to only the files that we really use, but we haven't gotten there yet. While the project expects the data to be housed in the `1_raw` directory, little of it is pushed to the GitHub repository for size constraints. Instead, you can download it from [OneDrive](https://uvmoffice-my.sharepoint.com/:u:/g/personal/swalshda_uvm_edu/ETMgUnpyIFdImfhaBt_hFA8BCZ3I8Fotb11s14FpVskEMQ?e=gUCA3s). Once downloaded, put the contents of the zip file into `1_raw` and the project should work.

There are also various API calls in the project, some of which require an API key to run, primarily from USDA NASS. To run these calls, [request an API key from NASS](https://quickstats.nass.usda.gov/api) and include it in the `.Renviron` file so that it will be called in the appropriate script.

One last note is that the project uses several functions from the projecter package, which is loaded in the `.Rprofile` file but not explicitly called in any scripts. If it does not automatically download on opening the project, download it with:

```r
if (!require('remotes')) install.packages('remotes')
remotes::install_github('ChrisDonovan307/projecter')
library('projecter')
```

One actual last note, there are some Python functions used in the geoprocessing. To reproduce the virtual environment, use:

```bash
python -m venv .venv
pip install -r requirements.txt
```

If you are running Python functions through reticulate, there is one more step to make sure it is accessing the same environment. Set the `.Renviron` file to specify the path to the environment with:

```bash
echo "RETICULATE_PYTHON=.venv/Scripts/python.exe" >> .Renviron
```

Note that these paths may change slightly depending on what system you are on. Functional paths for Windows are shown.

After that, you can source individual scripts, or run the entire project by sourcing the `table_of_contents.R` file. The final outputs of the project are in the `6_outputs/` directory in `.rds` format as lists. `sm_data.rds` contains all sustainability metrics data, including state level data on a national scale and county level data within New England, tree-formatted datasets for graphing the Sustainability Metrics Framework, as well as metadata and several utility datasets for navigating FIPS codes. `sm_spatial.rds` contains the spatial file outputs, including polygons for states and counties (both in 2021 and 2024, due to changes in Connecticut county layouts), as well as various rasters used in other parts of the analysis in the `sm-docs` repo. 

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
