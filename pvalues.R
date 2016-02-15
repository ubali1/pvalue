# load neccesary libraries
library(ggplot2)
library(MASS) #for the mvrnorm function to follow

# set constants
mu1 <- 0 # mean for first group
mu2 <- 1 # mean for second group
sd1 <- 1 # standard deviation for first group
sd2 <- 1 # standard deviation for second group
n <- 16 # number of observations per group
numberOfSimulations <- 1000 # number of simulations to run

# set the seed to create reproducability
set.seed(1024453)

# run the test resulting in n x numberOfSimulations matrix for the first group
normdist1 <- matrix(data=rnorm(n * numberOfSimulations, mean = mu1, sd = sd1), nrow=numberOfSimulations)
normdist1_means <- data.frame(means=apply(normdist1, 1, mean))

g <- ggplot(normdist1_means, aes((x = normdist1_means$means))) 
g + geom_histogram(aes(y=..density..), binwidth = 0.1, color = "black", fill = "white") + geom_density(alpha=.2, fill="#FF6666") + geom_vline(aes(xintercept=mu1), color="red", linetype="dashed", size=1) + stat_function(fun = dnorm, args = list(mean = mu1, sd = sd1), color = "blue")

# end of first group simulation

# run the test resulting in n x numberOfSimulations matrix for the second group
normdist2 <- matrix(data=rnorm(n * numberOfSimulations, mean = mu2, sd = sd2), nrow=numberOfSimulations)
normdist2_means <- data.frame(means=apply(normdist2, 1, mean))

m <- ggplot(normdist2_means, aes((x = normdist2_means$means))) 
m + geom_histogram(aes(y=..density..), binwidth = 0.1, color = "black", fill = "white") + geom_density(alpha=.2, fill="#FF6666") + geom_vline(aes(xintercept=mu2), color="red", linetype="dashed", size=1) + stat_function(fun = dnorm, args = list(mean = mu2, sd = sd2), color = "blue")

# end of second group simulation

# Add a new column with the name group and entries as 'group1' and 'group2' respectively
normdist1_means$group <- 'group1'
normdist2_means$group <- 'group2'

# combine the two distributions of the means using 'rbind' into a single column with their respective identifiers in the second
# column labeled group. 

normdist <- rbind(normdist1_means, normdist2_means)

# plot normdist data using frequency distributions of the means and 
ggplot(normdist, aes(means, fill = group)) + geom_density(alpha = 0.2)

# the following section of the code is adapted from Colquhoun et. al. 

cor=0.   #correlation = 0
# Establish covariance matrix for multivariate normal distrubtion simulation
var1 = sd1^2
var2 = sd2^2
sigma = matrix(c(var1,cor,cor,var2),2,2)

# Initialize variables
mean <- c(mu1, mu2)
mean_group1 <- vector(length = numberOfSimulations)
mean_group2 <- vector(length = numberOfSimulations)
mean_difference <- vector(length = numberOfSimulations)
pvalue <- vector(length = numberOfSimulations)
lowCI <- vector(length = numberOfSimulations)
highCI <- vector(length = numberOfSimulations)
sig_Pvalue = 0

# define p values for counting the number of p values between 0 and 0.05
#set min and max P values for "significance"
minPvalue = 0.0
maxPvalue = 0.05

# set random number generator seed so sequence repeats
set.seed(1024453)

for (r in c(1:numberOfSimulations))
{
        #simulate 2D data using multivariate normal distribution
        simdata = mvrnorm(n = n, mean, sigma)
        
        #label column headers to the generated values
        colnames(simdata) = c('group1', 'group2') 
        
        summary(simdata)
        simA = simdata[1:n,1] # assigns column 1 of the 16 observations to a new variable simA
        simA
        simB = simdata[1:n,2] # assigns column 2 of the 16 observations to a new variable simB
        simB
        
        # conduct two-sided t-test with alpha = 5% and with paired = FALSE and assume equal variance between the two groups
        
        tresult <- t.test(simB,simA,alternative="two.sided", paired = FALSE, var.equal = TRUE,
                        conf.level = 0.95)
        # calculate difference between the means of the two groups
        mean_difference[r] = tresult$estimate[1] - tresult$estimate[2]
        mean_group1[r] <- tresult$estimate[1]
        mean_group2[r] <- tresult$estimate[2]
        p <- tresult$p.value
        pvalue[r] <- p 
        lowCI[r] <- tresult$conf.int[1]
        highCI[r] <- tresult$conf.int[2]
        if (p > minPvalue & p <= maxPvalue) sig_Pvalue = sig_Pvalue + 1
}

# plot frequency histogram of pvalues using ggplot and overlaid with a density plot in red
#convert pvalue from vector to data.frame as ggplot doesn't take vector input
pvalue <- as.data.frame(pvalue)
g <- ggplot(pvalue, aes((x = pvalue))) 
g + geom_histogram(binwidth = 0.04, main = "Distribution of P values") + geom_density(alpha = 0.2, aes(y=..density..), color = "red")

# plot frequency histogram of mean difference values using ggplot and a vertical line to intersect at x =1 
#convert mean difference from vector to data.frame as ggplot doesn't take vector input
mean_difference <- as.data.frame(mean_difference)
g <- ggplot(mean_difference, aes((x = mean_difference))) 
g + geom_histogram(binwidth = 0.2, main = "Distribution of mean difference") + geom_density(alpha = 0.2, aes(x = mean_difference), color = "red") + geom_vline(aes(xintercept=1), color="red", linetype="dashed", size=1)


# plot frequency histogram of distribution of means in group 1 and group 2 using ggplot 
#convert means from vector to data.frame as ggplot doesn't take vector input
mean_group1 <- as.data.frame(mean_group1)
names(mean_group1[1]) <- 'mean'

mean_group2 <- as.data.frame(mean_group2)
mean_group1$group <- 'group1'
mean_group2$group <- 'group2'
colnames(mean_group1) <- c('mean', 'group')
colnames(mean_group2) <- c('mean', 'group')
meandist <- rbind(mean_group1, mean_group2)
ggplot(meandist, aes(mean, fill = group)) + geom_density(alpha = 0.2)

# calculate the power of the t test
test_power <- power.t.test(n = n, sd =  sd1, delta = 1, sig.level = maxPvalue, type = "two.sample", power=NULL)
test_power
test_power$power
test_power$sig.level

# print the number of significant p-values in this simulation
sig_Pvalue



        