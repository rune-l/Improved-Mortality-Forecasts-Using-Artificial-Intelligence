# Improved Mortality Forecasts Using Artificial Intelligence

Master thesis comparing stochastic factor mortality forecasting models with multi population factor models, machine learning models and deep learning models. This thesis finds that mortality forecasts can be improved by using information on multiple populations. Furthermore, using embeddings and representational learning using deep neural networks then mortality forecasting performance can be further improved. 

This thesis tests performance for 3174 different time-series and find that the best deep learning model can be found in the superior set of models 81.06% of the time, while the Lee-Carter model is only found in the superior set of models 54.20% of the time. The model confidence set approach is used to find the superior set of models for each time-series. 



The package with all the coded functions can be found in the folder mthesis ([link to package](https://github.com/rune-l/Improved-Mortality-Forecasts-Using-Artificial-Intelligence/tree/master/mthesis))



Mortality data used in this thesis can be found at:
Human Mortality Database.  University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de

The package requires a data.frame that is named df_mx_all where all mortality rates are stored for all the countries (guide to creating df_mx_all can be found [here](https://github.com/rune-l/Improved-Mortality-Forecasts-Using-Artificial-Intelligence/blob/master/Create%20data/Guide.md)). It must be saved in the data folder (create a folder named data [here](https://github.com/rune-l/Improved-Mortality-Forecasts-Using-Artificial-Intelligence/tree/master/mthesis) and put the data.frame df_mx_all in the folder) and the package must be recompiled with this data.frame for all functions to work.

All the results and theory used is explained and presented in the full master thesis:
[here](https://github.com/rune-l/Improved-Mortality-Forecasts-Using-Artificial-Intelligence/blob/master/Master_thesis.pdf)
