---
output: 
  html_document
---

## Introduction
This tab is meant to help, guide and assist you in any issues or questions that might arise while using the SignalDetectionTool app. It will cover the background and purpose of the tool, guide you through the different options and functionalities provided, as well as a contact point for you to report any bugs found while using the tool.

### United4Surveillance 
The SignalDetectionTool is developed within task under the United4Surveillance (U4S) project, Workpackage 2, Task 2 - Outbreak and Signal Detection. The goal of U4S is to ensure better detection of early warning signs and to strengthen infectious disease surveillance systems at the national level. Through early detection of infectious disease outbreaks, the further spread of diseases can potentially be contained, thereby reducing harmful consequences. Automated signal detection using tools can thereby help to identify specific events of concern and manage and foster automated data analysis. 

### Why use the SignalDetectionTool? 
Use this Shiny app as what it is - a tool. A tool to help bring another perspective to your disease surveillance, while also making it easy to generate reports and figures.

## Input data
This app requires specific input data formatting, which is described in detail in MS Teams.

## Workflow of the tool
The SignalDetectionTool app is made up of several tabs, which all holds different, key properties for using the app correctly. 
The workflow of the tool is to move from left to right, in a natural order. However, it may prove necessary to jump between the `Input` and `Signals` tabs, to achieve the right signal detection for your needs.
Additionally, the `Input`, `Signals` and `Report` tabs will be empty untill an input dataset is given.

### Data tab
The data tab is where you provide the tool with the proper input data, to perform signal detection upon. 

Clicking on the `Browse` button, you access a File Manager System, where you can navigate to your data set, saved as either an Excel (.xlsx) or comma-seperated file (.csv). 
Once you have selected a file, the file is read and checked by the tool. You will then receive feedback about your data in two sections: 

* In the first section, only those columns in your data where the column name has been written correctly are checked. For example, the column “country_id” is not checked when the column name deviates from it and is, for example, “country_id(NUTS)” in your dataset. This section checks whether all mandatory columns are present in your dataset and whether your columns have the right type and correct values. For example, it is checked whether the dates are in the correct YYYY-MM-DD format.

* In the second section, you receive feedback about all columns in your dataset that differ from the predefined names and are therefore not recognised by the DataCheckTool. From the example given above in i) the column “country_id(NUTS)” would appear in this section. As you want to use the column “country_id(NUTS)” later for signal detection, please change the column names to the required ones. Columns shown in this section are not checked for correct type and values.

In general, this feedback mechanism can, and should, be used in an iterative process. This means, that you get feedback about your data, you correct your data accordingly, and then upload it again. When all error are fixed you can stop, otherwise you can continue correcting and uploading.

In the data tab, you are also presented with a table showing the data provided. If the input data did not pass the data format check, then the raw data provided is shown, otherwise a table of pre-processed data, ready for signal detection is presented and you can move on the next tab, `Input`.

### Input tab 
The input tab is where you can interact, manipulate and shape the input data set, to investigate for signals proper. There are multiple different features to change, which will be covered in this section.

#### Pathogen
Using this tool, it is only possible to perform signal detection upon one kind of pathogen at a time. Thus if data about multiple pathogens exist in data, one will be selected. 

#### Filters
You can choose to investigate a subset of your data according to the filters you select. When filtering upon one or more variables, whether it be choosing a specific timeperiod through date_report and / or selecting only the age_groups 00-29, then that is the subset of data used to train the outbreak detection algorithms and is shown on the timeseries visualisation.

#### Strata
You can choose to stratify by up to three variables. Choosing a variable for stratification means that signals will be examined for each level of chosen strata(s). For example, if the variable age_group is one of chosen stratas, then each age_group level, e.g. "00-04", "05-09" etc., will be examined for signals. This strata examination will be shown both in the result table found at the bottom of the `Signals` tab, and additionally, a strata-specific visualisation or table is shown on the top of the `Signals` tab. Choosing a strata will not affect the timeseries visualisation in the `Signals` tab.

#### Signal detection period
Here you can set the number of weeks you want to generate signals for. The signals are generated for the most recent weeks, which is also dependent on if any filtering is done.

#### Signal detection algorithm
Depending on the number of weeks you want to generate alarms for, and the filters you set, the choice of algorithms is automatically updated to those which are possible to apply for your setting, with respects for the individual needs of training data of the different algorithms.  

In the following we call the training period the time period which is used to fit the parameters of the algorithms. This time period is given by your selection of date_report (Filters section) and the Signal detection period. If you select the time period 01-01-2020 until 31-12-2021 in the Filters section and select six weeks for the signal detection period the training period will be from 01-01-2020 until 22-11-2021 (six weeks before 31-12-2021). If you do not set any filter for date_report in the Filters section then the full time period of your dataset minus the Signal Detection Period will be used to fit the parameters.

Which signal detection algorithm should you choose? It depends on multiple criteria, and they all have their individual weaknesses and strengths.  

**CUSUM**
- does not model time trend
- does not model seasonality
- cusum can compute the baseline theoretically on only one time point in the past but for a good baseline more historic data should be used

CUSUM (cumulative sum) is an algorithm which detects shifts in the mean of a time series. It accumulates deviations of the observed case counts from the mean case count and once this accumulation has reached a value above a defined tolerance an alarm is triggered. After an alarm was triggered the cumulative sum is set to zero and the algorithm restarts. As CUSUM accumulates the deviations over time it is more sensitive to small and moderate shifts in the case count. The mean case count is computed on all case counts of the training period. CUSUM does not account for seasonality as it just computes a mean over all case counts.  
CUSUM was originally developed by E. S. Page and typically used in industry for monitoring change detection. Rossi et al. (1999) serves as reference in the public health context. 

**Farrington Flexible**
- models time trend
- models seasonality
- needs more historic data (78 time points) to compute a baseline

The Farrington Flexible uses a generalised linear model (GLM) to model the observed case count. In addition to a time trend terms for seasonality are also included in this model. For the computation of the baseline past very high case counts (potential outbreaks) are downweighted so that they don't shift the baseline upwards. Furthermore the past 26 time points before the current time point are excluded from the baseline to avoid reducing sensitivity when an outbreak has recently started before the current time point. The algorithm needs at least 78 time points (1.5 years when time points are weeks) of data in the past to calibrate the baseline.  
Farrington Flexible (Noufaily et al. 2012) is an improved version of the classic Farrington algorithm (Farrington et al. 1996). In this tool the implementation from the R package surveillance is used which is illustrated in Salmon et al. (2016).

**EARS**
- does not model time trend
- does not model seasonality
- needs only 7 time points of historic data 

The EARS (Early Aberration Reporting System) algorithm uses a window-based approach for outbreak detection. For the EARS C1 algorithm implemented in this tool mean and standard deviation are computed from the cases observed in the last 7 time points before the current time point. These parameters are used to compute a threshold based on the quantiles of the normal distribution. When the observed case count is above the computed threshold an alarm is raised. Because of the short time interval considered, EARS does not take into account seasonality and time trends. The baseline is computed using only the last 7 time points.  
EARS was developed by CDC and published in Hutwagner et al. (2003). 

### Signals tab
The `Signals` tab is split into 2 / 3 "rows", depending on whether any stratas were chosen in the `Input` tab. The first row belongs to the visualisations of the respective stratas chosen. 
The second row to a timeseries / epicurve, depicting the signal detection in the number of weeks specified in the `Input` tab.
The third and final row display a signal detection table, which provides information on the alarms found in both the entirety of the (filtered) dataset as well as per stratification parameter.

### Report tab
The `Report` tab is the final destination in the SignalDetectionTool Shiny app. Here, it is possible to produce and download a report, using the different parameter selections, filtering choices and stratifications chosen in the `Input` tab. Thus, you can customise exactly how you want your report. 
The report can be produced to three different formats: HTML, DOCX or PDF. All come with the U4S logo and expressions, as also found throughout the Shiny app. 

## Bug reporting
If you encounter any technical issues or bugs when using the SignalDetectionTool, please report them on [Github](https://github.com/United4Surveillance/signal-detection-evaluation-tool/issues/new/choose) by making an issue. 
Thank you in advance!


