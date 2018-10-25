# Hydrologic-Model-to-Predict-Streamflow-for-Delaware-River
This is a model developed while taking Dr. Chuck Kroll's ERE 645 Hydrologic Modeling course during the Spring of 2016. All this work was done independently based on instructor assignment and received an A.

The goal of the model is to predict streamflow for the Delaware River in Hale Eddy, NY using a simple precipitation-runoff modeling approach.

There are three separate files associated with this model:
  - Chien_HW7.R Main script used to run the model. Requires an input of nwis atmoshpheric data in csv form.
  - Chien_HW7fxns.R Functions called in the main script above. These functions perform various processes to filter, clean and combine the data using a simple P - ET = Q water balance.
  - Chien_HW7Calibrate.R a script to calibrate the n input parameters of the model using an n-dimensional calibration matrix.
