# Recidivism Prediction

This repository documents the project Minseon Lee and Aaron Dunmore completed in the fall of 2019. We developed risk assessment instruments (RAIs) predicting two-year recidivism in Broward County, FL, using the data released by [ProPublica](https://github.com/propublica/compas-analysis). We evaluated the predictive performance of our models and compared it to the performance of the RAI used by the courts in Broward County previously. In addition, we investigated whether our RAIs are equally predictive across race/ethnicity, age, and sex groups. 

This project was completed as part of [Data Mining](https://www.andrew.cmu.edu/user/achoulde/95791/index.html) taught by [Prof. Alexandra Chouldechova](https://www.andrew.cmu.edu/user/achoulde/).

### Instructions

This repository contains four folders:

Folders | Description
-- | ------
`Notebooks` | Final report. You can execute it without having to run any other code
`Data` | Processed compas.db and .csv files used by our models 
`Cache` | Trained models as R objects. It will save time when you re-excute our notebook
`Data_Processing` | .R and .sql files used to generate processed compas.db and .csv files in `Data` 

You can also rebuild our project from scratch:

1. Delete the contents of `Data`. Place [compas.db](https://github.com/propublica/compas-analysis/blob/master/compas.db) in `Data`.
2. Open [compas.db](https://github.com/propublica/compas-analysis/blob/master/compas.db) in a sqlite manager, and execute `Data_Processing`/data_processing.sql. We used [DB Browser for SQLite](https://sqlitebrowser.org/). This will create a number of variables used by our models in the database.
3. Execute `Data_Processing`/data_export.R. Your working directory should be set to the 
   top level of this folder. This will form several .csv files used by our analysis.
4. Execute `Notebooks`/final_report.Rmd. Your working directory should be set to `Notebooks`.
   
If you would like to train all models as new, delete the contents of `Cache`. 












