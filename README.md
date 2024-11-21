
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SignalDetectionTool

<!-- badges: start -->
<!-- badges: end -->

## Overview

A shiny app for automated detection of signals in infectious disease
surveillance data. A signal is an anomaly in an infectious disease
surveillance time series.  
This app allows the user to investigate generated signals and
corresponding surveillance time series. It includes upload of csv or
Excel files, data checks, possibility to set parameters for signal
detection, visualisation of results and generation of reports. The tool
is interactive and flexible so that it can be used in different contexts
and customised to the user’s needs.

## Installation

You can install the most recent release of the SignalDetectionTool from
[GitHub](https://github.com/United4Surveillance/signal-detection-tool)
with:

``` r
# install.packages("devtools")
devtools::install_github("United4Surveillance/signal-detection-tool")
```

<details>
<summary>
<strong>Installing latest SignalDetectionTool version</strong>
</summary>
<ul>
<li>
You can download the latest release of the SignalDetectionTool through
the Releases page
</li>
<li>
To navigate to the Release page, scroll up and look on the right-hand
side of the page under a section labelled Releases. Click on ‘Releases’
</li>
<li>
You can find the latest tool release at the top of the page
</li>
<li>
There are three files you can download:
</li>
<ul>
<li>
If you just want to run the tool and do not want or need to see the code
behind, you can follow the installation using .tar.gz
</li>
<li>
This is a binary. You can use this if you do not have Rtools installed
and you are not interested in modifying the code of the app.
</li>
<li>
This is just a zipped folder of the repository. It can be unzipped and
installed over the ‘Build’ tab in Rstudio. This can be used if you want
to modify code of the app for your usage. You can also just clone the
repository instead.
</li>
</ul>
</ul>
<strong>Installation using .tar.gz</strong>
<ul>
<li>
Download the provided .tar.gz file and save it on your computer
</li>
<li>
Install the package <code>remotes</code> by putting
<span style="color:blue;">install.package(“remotes”)</span> in the
console
</li>
<li>
Install the package dependencies for the SignalDetectionTool using the
full path where you stored the .tar.gz file and replace the x.x.x with
the current version of the binary. For example:
<span style="color:blue;">remotes::install_deps(“C:/Users/YourUsername/Downloads/SignalDetectionTool_x.x.x.tar.gz”)</span>
</li>
<li>
Execute this command in the console
</li>
<li>
This should also install any dependencies you need to run the tool. Wait
until the installation finishes successfully
</li>
<li>
Write the following command into the console to install the tool:
<span style="color:blue;">install.packages(“path_to_the_tool/SignalDetectionTool_x.x.x.tar.gz”,
repos = NULL, type=“source”)</span>
</li>
<li>
Continue to read ‘Running the shiny application’
</li>
</ul>
<strong>Installation using SignalDetectionTool_x.x.x_binary.zip
file</strong>
<ul>
<li>
Download the provided zip file and save it on your computer and open
RStudio
</li>
<li>
Install the package remotes by putting
<span style="color:blue;">install.package(“remotes”)</span> in the
console
</li>
<li>
Install the package dependencies for the SignalDetectionTool using the
full path where you stored the .zip file and replace the x.x.x with the
current version of the binary.For example:
<span style="color:blue;">remotes::install_deps(“C:/Users/YourUsername/Downloads/SignalDetectionTool_x.x.x.binary.zip”)</span>
</li>
<li>
Execute this command in the console
</li>
<li>
This command will then ask you if you want to update your installed
packages. Please type 1 and press enter to install the updates. For
advanced users: you can also install only the minimal required versions
written inside the DESCRIPTION file by using
<span style="color:blue;">remotes::install_version()</span>
</li>
<li>
Now the SignalDetectionTool needs to be installed. For this execute this
command
<span style="color:blue;">install.packages(“your_path_to_the_zip/SignalDetectionTool_x.x.x_binary.zip”)</span>
in the console, replacing again your_path_to_the_zip by your system path
and changing x.x.x to the version specification.
</li>
<li>
Continue to read ‘Running the shiny application’
</li>
</ul>
<summary>
<strong>Installation from source after unzipping the Source Code
(zip):</strong>
</summary>
<ul>
<li>
Download the provided zip file and save it on your computer
</li>
<li>
Go into the unzipped signal-detection-tool folder and double-click on
the <code>SignalDetectionTool.Rproj</code> file to open it
</li>
<li>
You can either use the RStudio suggestion to install packages by
clicking on the ‘Install’ button or copy the following line of code into
the console: <span style="color:blue;">devtools::install_deps()</span>
</li>
<li>
This command will then ask you if you want to update your installed
packages. Please type 1 and press enter to install the updates
</li>
<li>
For advanced users: you can also install only the minimal required
versions written inside the <code>DESCRIPTION</code> file by using
<span style="color:blue;">remotes::install_version()</span>
</li>
<li>
In case the command
<span style="color:blue;">devtools::install_deps()</span> gives you an
error that there is no package called ‘devtools’, you need to install it
using <span style="color:blue;">install.packages(“devtools”)</span> and
then run the command again
</li>
<li>
Now the SignalDetectionTool needs to be installed. For this, go to the
top right window in RStudio and click on the tab ‘Build’. Next, click on
the ‘Install’ button. You will see that the installation process has
started
</li>
<li>
Continue to read ‘Running the shiny application’
</li>
</ul>

<strong>Running the shiny application</strong><br> To run the shiny app,
first load the package and then run the app using the following
commands:<br>
<span style="color:blue;">library(SignalDetectionTool)</span><br>
<span style="color:blue;">run_app()</span>

</details>

## Usage

The shiny application is structured into a **Help**, **Data**, **Input
parameters**, **Signals** and **Report** tab.  
You can try the shiny application yourself using test data provided with
the package. The test data is found in dev/data/input/input.csv.

### Help

The Help tab provides more information to the user how to use the app
and details about settings in the individual tabs and signal detection
algorithms.

### Data

In the Data tab the user can upload the surveillance data for which the
signal detection should be performed. The input format is a line list of
infectious disease cases and the tool accepts data in the predefined
format described in dev/data/input/description.md. A data format check
is automatically run once the data file has been uploaded. You can also
view the required input format using

``` r
library(SignalDetectionTool)
View(input_metadata)
```

### Input parameters

In the Input parameters tab you can configure the signal detection you
want to perform. You can specify filters, stratification variables, time
periods and the signal detection algorithm which should be used. For
some algorithms, a pandemic correction can be applied, incorporating an
interrupted time series analysis into the outbreak detection methods.
![](man/figures/README-input_tab.PNG)

### Signals

The Signals tab can look like this when using the test data and
stratification by age group, county and sex.
![](man/figures/README-signals_tab.PNG)
![](man/figures/README-timeseries.PNG)
![](man/figures/README-signal_detection_table.PNG)

### Report

In the Report tab HTML and Word reports can be generated showing the
results of the Signals Tab.

## Development Process and Contribution

The tool development is an open source development process. The
participants of Work Package 2.2 of the [United4Surveillance
project](https://united4surveillance.eu/) will be the maintainers of the
repository and can contribute directly, third parties can contribute via
pull requests. The maintainers are meeting regularly to discuss pull
requests, issues and to add new issues according to the collected
requirements. Maintainers and contributors can work independently on
issues. For more information on how to contribute, please see our
[contributing
guidelines](https://github.com/United4Surveillance/signal-detection-tool/blob/main/.github/CONTRIBUTING.md).

## Roadmap

The initial version of the tool (0.1.0) was deployed in April 2024 to
eleven European public health institutes for piloting. The official
piloting phase ended in November 2024. The tool can and is used after
November 2024 and will still be developed further.<br> Some key features
we want to develop are:

- possibility to aggregate the time series on different levels
  i.e. daily, biweekly, monthly,…
- possibility to combine stratifications
- retrieve linelist with cases corresponding to signals generated
