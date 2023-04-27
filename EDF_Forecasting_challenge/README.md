# Load Forecasting During the COVID Period
## Day ahead forecasting of electricity load from April 2020 to January 2021

The coronavirus disease 2019 (COVID-19) pandemic has urged many governments in the world to enforce a strict lockdown where all nonessential businesses are closed and citizens are ordered to stay at home. One of the consequences of this policy is a significant change in electricity consumption patterns. Since load forecasting models rely on calendar or meteorological information and are trained on historical data, they fail to capture the significant break caused by the lockdown and have exhibited poor performances since the beginning of the pandemic. The proposed challenge aims at improving forecasts during this period by proposing new methods but also new open data.

Data are taken from http://www.rte-france.com/fr/eco2mix/eco2mix Licence Ouverte v2.0 (Etalab) and https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker#data


This project was undertaken with Ángel REYERO LOBO, and won the challenge proposed as part of the Predictive Modelling course given by the Pr. Yanig GOUDE (Senior Researcher, EDF R&D).

The functions have been encoded as an R library named 'ProjectLoadForecasting'.
The trained models have been saved separately.


The Kaggle competition is accessible at the following address:
https://www.kaggle.com/competitions/load-forecasting-during-the-covid-period/overview


Dataset Description:
The data correspond to french electricity load from 20212 to january 2021 at a daily resolution (mean per day) in MW. We provide a train set from january 2012 to august 2019 and a test set from september 2019 to january 2021. The goal of the challenge is to forecast the target "Load".

File descriptions:
train.csv - the training set from january 2012 to april 2020
test.csv - the test set from april 2020 to january 2021.
sampleSubmission.csv - a sample submission file in the correct format

Data fields:
Date Date , format YYYY-mm-dd,
Load - the electricity load in MW
Load.1 - lag one day of electricity load
Load.7 - lag 7 days of electricity load
Temp - mean temperarture. over 39 stations of France in celsius degrees
Temp_s95 - smooth temperature with 0.95 smoothing parameter
Temp_s99 - smooth temperature with 0.99 smoothing parameter
Temp_s95_min - daily min of the smoothed temperature with 0.95 smoothing parameter
toy - time. of year from 0 to 1 each year
WeekDays - day of the week
BH - bank holidays
DLS - daylight savings
Summer_break - summer holidays period
Winter_break - winter holidays period
GovernmentResponseIndex - Index of restriction measure due to covid 19, see https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker#data


