# Simulation Studies: Exploring Theory 

This repository contains folders and code related to statistical simulations that study an interesting theory.

The folder BIC Study contains code files that perform simulations to verify whether the probability of BIC selecting the true model approaches 1 as the sample size increases. Note that the code is computationally intensive.

The folder qq plot construction contains code to draw Q-Q plot for certain standard distributions. 

The folder LASSO and Ridge is an exercise aimed at determining how often LASSO correctly identifies non-significant regressors and sets them to zero. Additionally, it explores whether there is any relationship between sample size and the accuracy of identifying true non-significant regressors. As the sample size increases, it is observed that, in some simulations, additional regressors are also set to zero. 

The sampling Techniques folder contains methods for generating samples from non-standard functions/distributions. Currently, the Acceptance-Rejection method is included. Importance Sampling and other methods will be added soon. Gibbs sampling method added. 

The Time Series folder contains exercises related to time series analysis. The initial commit includes an exercise demonstrating a step-by-step classical additive decomposition of time series data. As the course progresses, more exercises will be added.  

The Bagging folder contains code related to applying the bagging method to Linear Regression and Linear Discriminant Analysis techniques. It also includes code demonstrating the use of Random Forest with the Linear Regression technique.

The Missing Data Treatment folder contains code for creating missing data from the original dataset. After generating the missing data, various missing value imputation techniques are applied to fill the NA values. Finally, visualization is performed to assess the impact of the missing value imputation.
