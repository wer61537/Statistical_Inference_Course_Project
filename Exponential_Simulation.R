## Simulating the Exponential Distribution

#remove all objects just to be safe
rm(list = ls(all = TRUE))

#load libraries
library(ggplot2)
library(knitr)
library('gridExtra')

#for color schemes
library('RColorBrewer')

#set color scheme for graphing
BuGn <- brewer.pal(5, 'BuGn')[3:5]
reds <- brewer.pal(5, 'Reds')[3:5]


#======================================================================
#set parms for rexp
#======================================================================
# lambda is the rate parameter of the exponential distribution, 
# N is the number of simulations done and 
# n is the number of samples created for each sim. 

set.seed(100)
lambda <- 0.2; 
N <- 1000; 
n <- 40
#======================================================================
#set parms for rexp
#======================================================================

#======================================================================
#calculate the distribution of simulated means
#======================================================================

#======================================================================
#calculate the distribution of simulated means
#======================================================================
#use sapply to run 1 to N sims and apply to calculate the mean for each sim
dist_means<-apply(sapply(1:N, function(i) {rexp(n, lambda)}), 2, mean)

title1 <- expression(
  paste(
    'Figure 1: Sampling Distribution of Simulated Mean (',
    bar(x), 
    ') compared to Theoretical Mean (',
    frac(1, lambda),
    ')')
)

p1<-ggplot(mapping=aes(x=dist_means)) +
  geom_histogram(bins=25, col=BuGn[2], fill=BuGn[1]) +
  geom_vline(xintercept=1 / lambda, col=BuGn[3], size=2) +
  labs(title=title1, x='Simulated Mean', y='Count')

p1

#======================================================================
#calculate the distribution of simulated means
#======================================================================


#======================================================================
#convergence of the simulated mean to the theoretical mean
#======================================================================
#theo mean is 5.0
#accumulate the means and divide each additional term by it's index
cumul_means <- cumsum(dist_means) / seq_along(dist_means)
#calculate the difference between the mean and the theoretical mean (1/lambda)
abs_diff_mean <- abs(cumul_means - (1 / lambda))

#plot the convergence of the simulated mean to the theoretical mean

title2 <- expression(paste('Figure 2: Convergence of simulated mean (',
                           bar(x), 
                           ') to the theoretical mean (', 
                           frac(1, lambda),
                           ')')
)

p2 <- ggplot(mapping=aes(x=seq_along(cumul_means), y=cumul_means)) +
  geom_hline(yintercept=1 / lambda, col=reds[3]) +
  geom_line(col=BuGn[3]) +
  labs(title=title2, x=expression(paste(italic(ith),' Simulation')), y='Simulated Mean')
p2

title3 <- expression(paste('Figure 3: Convergence of absolute difference between simulated mean and theoretical mean, ', 
                           abs(bar(x) - frac(1, lambda))))

p3 <- ggplot(mapping=aes(x=seq_along(abs_diff_mean), y=abs_diff_mean)) +
  geom_hline(yintercept=0, col=reds[3]) +
  geom_line(col=BuGn[3]) +
  labs(title=title3, x=expression(paste(italic(ith),' Simulation')), y='Mean')
p3


#======================================================================
#convergence of the simulated mean to the theoretical mean
#======================================================================


#======================================================================
#convergence of the simulated variance to the theoretical Variance
#======================================================================
cumul_vars <- sapply(seq_along(dist_means),
                     function(x) {
                       var(dist_means[1:x])
                     })

# theo variance is 1 / (lambda^2 * n)  or 0.625
title4 <- expression(paste('Figure 4: Convergence of simulation variance (', s^2, ') to theoretical variance (', frac(1, (lambda^2 * n)),')'))
p4 <- ggplot(mapping=aes(x=seq_along(cumul_vars), y=cumul_vars)) +
  geom_hline(yintercept=(1 / (lambda^2 * n)), col=reds[3]) +
  geom_line(na.rm=T, col=BuGn[3]) +
  labs(title=title4, x=expression(paste(italic(ith),' Simulation')), y='Variance')
p4

abs_diff_variance <- abs(cumul_vars - (1 / (lambda^2 * n)))

title5 <- expression(paste('Figure 5: Convergence to 0 of absolute difference between simulation and theoretical variance (', abs(s^2 - frac(1, (lambda^2 * n))), ')'))
p5 <- ggplot(mapping=aes(x=seq_along(abs_diff_variance), y=abs_diff_variance)) +
  geom_hline(yintercept=0, col=reds[3]) +
  geom_line(na.rm=T, col=BuGn[3]) +
  labs(title=title5, x=expression(paste(italic(ith),' Simulation')), y='Variance')
p5


#======================================================================
#convergence of the simulated variance to the theoretical Variance
#======================================================================

#======================================================================
#Determination of normality
#======================================================================  
title6 <- expression(paste('Figure 6: Simulated Mean and Normal Distributions ',
                           italic(N) %~% (list(1 / lambda, 1 / (lambda * sqrt(n))))))
p6<-ggplot(mapping=aes(x=dist_means)) +
  geom_histogram(aes(y=..density..), bins=25, col=BuGn[2], fill=BuGn[1]) +
  stat_function(fun=dnorm,
                args=list(mean=1 / lambda, sd=1 / (lambda * sqrt(n))),
                col=BuGn[3], size=2) +
  labs(title=title6, x='Value', y='Density')+
  geom_vline(xintercept=1 / lambda, col=BuGn[3], size=2)
p6

title7<-  'Figure 7: Q-Q plot of the simulation distribution'

p7<-ggplot(mapping=aes(sample=dist_means)) +
  geom_abline(intercept=1 / lambda, slope=1 / (lambda * sqrt(n)),
              col=reds[3]) +
  stat_qq(col=BuGn[2], fill=BuGn[1], alpha=0.1) +
  labs(title=title7,x='Theoretical quantiles', y='Sample quantiles')
p7
#======================================================================
#Determination of normality
#======================================================================

## ------------------------------------------------------------------------
#coverage is the (empirical) probability that the population mean is 
# included in the conﬁdence interval. Mean is 5, use 3-7.  
#use 95% confidence interval
seq_lambdas<- seq(3, 7, by=0.01)


#calculate coverage
coverage <- sapply(seq_lambdas, function(lamb) {
  mu_hats <- rowMeans(matrix(rexp(n*N, rate=0.2),N, n))
  lower_limit <- mu_hats - qnorm(0.975) * sqrt(1/lambda**2/n)
  upper_limit <- mu_hats + qnorm(0.975) * sqrt(1/lambda**2/n)
  mean(lower_limit < lamb & upper_limit > lamb)
}
)
title8<-  'Figure 8: Coverage (probability that population mean is in confidence interval)'

#coverage
p8<-qplot(seq_lambdas, coverage) + geom_hline(yintercept=0.95,col=reds[3])+
  labs(title=title8,x='Lambda', y='Probability (Coverage)')
p8

