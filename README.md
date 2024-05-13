# signal-detection-tool
A tool for detection of signals in infectious disease surveillance data. 

## What is a signal?
A signal is an anomaly in a infectious disease surveillance time-series.

## Process
The tool development will follow an open source development process.

The participants of the United4Surveillance project will be the maintainers of the repository and can contribute directly, third parties can contribute via pull requests.

The maintainers will meet regularly and discuss pull request, issues and add new issues according to the collected requirements.

Maintainers and contributers can work indepently on issues.

For more information on how to contribute, please see our [contributing guidelines](https://github.com/United4Surveillance/signal-detection-tool/blob/main/.github/CONTRIBUTING.md).

## Installation

You can install the development version of SignalDetectionTool like so:

Installation directly from github:

1.  To install a package from a private repository setup a *github access token* by visiting [https://github.com/settings/tokens](https://github.com/settings/tokens) and following the       'Generate new token' process.
2.  To make the token available in RStudio edit the .Renviron (you can do this by running `usethis::edit_r_environ()`) and add
    GITHUBTOKEN=“newly created tokenstring”.
3.  Restart RStudio.
4.  Install the package using
    `devtools::install_github(repo = "https://github.com/United4Surveillance/signal-detection-tool", ref = "<branch you want to install from>", auth_token = Sys.getenv("GITHUBTOKEN"), build_vignettes = TRUE)`

Installation from a local repository:

1. Open the R-project in Rstudio.
2. Run `devtools::install(build_vignettes = TRUE)`. 

## Input data
The tool accepts data in the predefined format described in the description.md in /data/input. 

### Internal data connection
If you want to establish an internal data connection rather than manual file input, you have to define a function called 'readDB' that returns a data.frame in the format described above. We suggest to implement this function in an R script called 'internal.R' in the '/R' folder, because this file will not be uploaded to the github repository. Make sure not to upload any sensitive information to the github repository. Furthermore, you have to set the parameter 'readDB' to TRUE in the 'golem-config.yml' in the '/inst' folder.

## Output
The tool produces signals based on the given input data. Signals are listed in a human readable output file, the format is described in description.md in data/output/ and in an interactive report together with the surveillance data.

## Development roadmap
The Joint Action project roadmap states that the tool development should be finished by end of March 2024, while piloting the software should start in January 2024.
We view this latter deadline as . Our goal is to get as much feedback from the piloting countries as early as possible.
Therefore, we adopt an agile development approach with the aim to ship improved versions as often and as early as possible. 

**Our goal is to have a working minimal viable product (MVP) at the end of October 2023, at the latest.**

Here is possible iterative development roadmap:

#### V 0.1
* General:
	* Provide example data (real or simulated)
* Simple R script:
	* Load data from path
	* Run FarringtonFlex from Surveillance Package
    * Generate plot of results

#### V 0.2
* R script:
	* Expand input data to regionality
	* Run FarringtonFlex per region
	* Generate plot of results
* Simple Evaluation:
	* Load truth data
    * Calculate Sensitivity & Specificity
    * Print results

#### V 0.3
* Simple user interface:
	* Create shiny app
	* "Run" button script functions
	* Show plot
	* Show evaluation results
* Evaluation:
  * Add timeliness metric

#### V 0.4
* Simple user interface:
	* Input data from user prompt (file explorer)
	* Display regional signals
* Simple output report:
	* Create report containing plots
	* Turn into PDF

#### V 0.5
* General:
	* Add Config file (no code)
		* Name of country
		* Diseases of interest
	* Get shapefiles for NUTS3 regions
	* Visualize regional data using shape files
* User interface:
	* Generate report from button
	* Use/Display information from config file 
    * Add drop down menu to  switch between diseases
