# pvalue

This code creates a simulation where two groups (group 1, group 2) of random observations are generated, a 1000 times over, with specified means of 0 and 1 respectively and having the same standard deviation. Each group consists of 16 observations where the observations are normally distributed. 

The t-distribution of the difference in the means of the 1000-simulated tests shows that the difference of mean is close to 1. The distribution of the 1000 p-values generated where the number of p-values that are less than or equal to 0.05 is 776 out of a 1000 tests or 78%. A look at the power calculation for this t-test is 78%, which is also in keeping with our simulated experiment. In other words, 78% of the simulated t-tests give the correct result. 

