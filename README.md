# E-CESM: Extension of counterfactual effect size model of Quillien & Lucas 2022

For submission for conferences 2023 there is just the model implementation here ('ecesm_model_preds.R'), which takes the 64 simulated worlds (2^6 factors) and makes predictions using our version of the CESM.

Also see the model fit script 'CESM_fits.R' for our function to bring in Other causes. However, we removed the actual data steps because we could not ensure anonymity of the behavioural data. So this script will not run as is; it is just for info about modelling.

# Comparison of model to human data

See below for each of the 64 grid world scenarios, with human participant responses of each category (Preference, Character, Knowledge, Other) as coloured bars, and model predictions as red dots.

![big_plot](big_plot.pdf)
