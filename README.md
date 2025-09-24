# Traffic Forecasting Project

This project was developed as part of the *Forecasting and Predictive Analytics* course of the Master's degree in Business Analytics at the University of Bologna.  
It focuses on applying time series forecasting methods in **R** to a real-world dataset. We selected a dataset consisting of hourly observations recorded on Interstate 94 Westbound at the Minnesota Department of Transportation (MnDOT) Automatic Traffic Recorder (ATR) station 301, located roughly midway between Minneapolis and St. Paul.  

After data preparation, the dataset covers the period from January 1, 2016, to September 30, 2018, with a total of 24,096 hourly observations and no missing values.  
The full dataset is available here: [UCI Repository – Metro Interstate Traffic Volume](https://archive.ics.uci.edu/dataset/492/metro+interstate+traffic+volume).  

The goal was to produce accurate hourly forecasts of the expected traffic volume over a 72-hour horizon.  

## Project Overview
The project followed the complete forecasting workflow:  
1. Data acquisition and preparation: identification of a suitable dataset, cleaning, and preprocessing.  
2. Exploratory data analysis: descriptive statistics and visualization to understand the main features of the data.  
3. Model selection and estimation: testing and comparing different forecasting models to identify the most appropriate one.  
4. Evaluation: assessing model performance and interpreting the results.  

## Repository Contents
The repository contains three main files:  
- **`TheOutliers_Data.csv`** → the dataset used in the project (already cleaned and ready to use).  
- **`TheOutliers_Code.R`** → the R script containing the full code for data preparation, analysis, and forecasting.  
- **`TheOutliers_Report.pdf`** → the final report, describing the methodology adopted and discussing the results.  

## Code
All steps of the project are supported by R scripts written by my group and me.  
The code is fully reproducible and makes use of standard R packages for data handling, visualization, and forecasting.  

## How to Run in R
1. Open the `.R` files in RStudio.  
2. Install the required packages listed at the top of the script.  
3. Run the code to reproduce the analysis and results.  
