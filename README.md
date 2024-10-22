# Sustainability Metrics Data

## About

> [!NOTE] This project is early in development. The framework, metrics, and analyses are all preliminary.

The Sustainability Metrics project is a collaborative effort to measure food system sustainability in New England led by the University of Vermont (UVM) UVM Food Systems Research Center (FSRC). We work with five teams of investigators conducting primary research in vital aspects of local and regional food systems. To support this work, we are also exploring secondary data sources.

This repository houses an R project which collects, cleans, and compiles secondary data from a variety of sources in service of the Sustainability Metrics project. Clean exports of data are sent to [sm-docs](https://www.github.com/food-systems-research-center/sm-docs) and [sm-explorer](https://www.github.com/food-systems-research-center/sm-explorer).

## Navigation

After opening the `sm-data.Rproj` file, head to `table_of_contents.R`, which outlines the workflow of the project. To navigate to a script, you can either use `CTRL/CMD + LEFT CLICK` on a file path or put the cursor into the file path and hit `F2`. Once in a script, bring up the navigation pane with `CTRL/CMD + SHIFT + O`, or jump to a section with `ALT + SHIFT + J`. Note that each script loads the relevant objects at the start and saves them at the end. So, you can jump into any script in the project without running all previous scripts. However, within each script, code must be run sequentially, starting with the housekeeping section. 

The file structure is organized as follows:

-   `1_raw`: Downloads of bulk files, datasets, and spatial layers before any processing has taken place. Note that these files are large, and currently not pushed to this repo.
-   `2_clean`: Cleaned, wrangled, compiled datasets and spatial layers ready for exploratory analysis.
-   `3_functions`: Functions that are used throughout the project. These are sourced in the housekeeping section of each script as needed.
-   `4_scripts`: R scripts.
-   `5_objects`: Intermediate objects worth saving.
-   `6_outputs`: CSV or figure outputs.
