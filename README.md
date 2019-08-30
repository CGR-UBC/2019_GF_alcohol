# 2019_GF_alcohol

Analysis scripts for the regression models in publication: [insert link]

Two sets of models: choice and bet. All R files require LogModel.R LogPost.R and LogPrep.R to run.

For each model, four versions of the mode were run: 

1. Frequentist regression with linear predictors (if didn't violate normaility assumption)
2. Frequentist regression with binary predictors for Run Length and Streak Length
3. Bayesian regression with linear predictors (if didn't violate normaility assumption)
4. Bayesian regression with binary predictors for Run Length and Streak Length

All files read in the same data file containing all participants data. This is a .csv file with one row per trial, with the following columns:

* Pairing (this is participant ID)
* Run.Length (integers)
* Win.Length (integers)
* Loss.Length (integers)
* Choice.same.as.last.outcome (binary, 0 = choice different)
* Bigbet.median.split (binary, 0 = small bet)
* Group (binary, placebo = 0)