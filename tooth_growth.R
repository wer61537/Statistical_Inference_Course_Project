library(plyr)

#get the dataset that is part of R
data(ToothGrowth)

summary(ToothGrowth)
str(ToothGrowth)

#take a look at the unique values for each of the variables
unique(ToothGrowth$len)
unique(ToothGrowth$supp)
unique(ToothGrowth$dose)

#The dose strength has only 3 available values, while the supplement type 
#is a factor with 2 values. So in the dataset existing only 6 possible combinations 
#with these two variables, and I explore them.

#Summary of guinea pigs given 0.5mg of Vitamin C
summary(ToothGrowth[which(ToothGrowth$dose==0.5 & ToothGrowth$supp=='VC'),]$len)

#Summary of guinea pigs given 1.0mg of Vitamin C
summary(ToothGrowth[which(ToothGrowth$dose==1.0 & ToothGrowth$supp=='VC'),]$len)

#Summary of guinea pigs given 2.0mg of Vitamin C
summary(ToothGrowth[which(ToothGrowth$dose==2.0 & ToothGrowth$supp=='VC'),]$len)

#Summary of guinea pigs given 0.5mg of Orange Juice
summary(ToothGrowth[which(ToothGrowth$dose==0.5 & ToothGrowth$supp=='OJ'),]$len)

#Summary of guinea pigs given 1.0mg of Orange Juice
summary(ToothGrowth[which(ToothGrowth$dose==1.0 & ToothGrowth$supp=='OJ'),]$len)

#Summary of guinea pigs given 2.0mg of Orange Juice
summary(ToothGrowth[which(ToothGrowth$dose==2.0 & ToothGrowth$supp=='OJ'),]$len)


ddply(ToothGrowth, dose ~ supp,function(x) 
  c(mean=mean(x$len), sd=sd(x$len),
    conf.int=t.test(x$len)$conf.int))

#The results of the exploratory analysis depicted above are resumed in the following boxplot.

library(ggplot2)
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  ggtitle("ToothGrowth Length vs Dose by Supplement") + 
  geom_boxplot(aes(fill=factor(dose))) + geom_jitter() + facet_grid(.~supp)

ggplot(ToothGrowth, aes(x=supp, y=len)) + 
  ggtitle("ToothGrowth Length vs Supplement by Dose") + 
  geom_boxplot(aes(fill=factor(supp))) + geom_jitter() + facet_grid(.~dose)


ddply(ToothGrowth, dose ~ supp,function(x) 
  c(mean=mean(x$len), sd=sd(x$len),
    conf.int=t.test(x$len)$conf.int))

#Use confidence intervals and hypothesis tests to compare tooth growth by supp and dose
#Starting from the results of the exploratory analysis I decide to analyze the tooth length with respect to the combination of supp anda dose. I formulate 3 null hypothesis:
  
#H0: mu(VC, 0.5) = mu(OJ, 0.5)
#H0: mu(VC, 1.0) = mu(OJ, 1.0)
#H0: mu(VC, 2.0) = mu(OJ, 2.0)
#Each of these hypothesis will be tested against the respective alternate hypothesis (~=). To verify the hypothesis I use a t test.

#H0: mu(VC, 0.5) = mu(OJ, 0.5)

t.test(ToothGrowth[which(ToothGrowth$dose==0.5 & ToothGrowth$supp=='VC'),]$len, 
       ToothGrowth[which(ToothGrowth$dose==0.5 & ToothGrowth$supp=='OJ'),]$len)
## 
##  Welch Two Sample t-test
## 
## data:  ToothGrowth[which(ToothGrowth$dose == 0.5 & ToothGrowth$supp ==  and ToothGrowth[which(ToothGrowth$dose == 0.5 & ToothGrowth$supp ==     "VC"), ]$len and     "OJ"), ]$len
## t = -3.17, df = 14.97, p-value = 0.006359
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -8.781 -1.719
## sample estimates:
## mean of x mean of y 
##      7.98     13.23
#The 95% confidence interval doesn’t contain 0 so reject the null hypothesis.

#H0: mu(VC, 1.0) = mu(OJ, 1.0)

t.test(ToothGrowth[which(ToothGrowth$dose==1.0 & ToothGrowth$supp=='VC'),]$len, 
       ToothGrowth[which(ToothGrowth$dose==1.0 & ToothGrowth$supp=='OJ'),]$len)
## 
##  Welch Two Sample t-test
## 
## data:  ToothGrowth[which(ToothGrowth$dose == 1 & ToothGrowth$supp ==  and ToothGrowth[which(ToothGrowth$dose == 1 & ToothGrowth$supp ==     "VC"), ]$len and     "OJ"), ]$len
## t = -4.033, df = 15.36, p-value = 0.001038
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -9.058 -2.802
## sample estimates:
## mean of x mean of y 
##     16.77     22.70
#The 95% confidence interval doesn’t contain 0 so reject the null hypothesis.

#H0: mu(VC, 2.0) = mu(OJ, 2.0)

t.test(ToothGrowth[which(ToothGrowth$dose==2.0 & ToothGrowth$supp=='VC'),]$len, 
       ToothGrowth[which(ToothGrowth$dose==2.0 & ToothGrowth$supp=='OJ'),]$len)
## 
##  Welch Two Sample t-test
## 
## data:  ToothGrowth[which(ToothGrowth$dose == 2 & ToothGrowth$supp ==  and ToothGrowth[which(ToothGrowth$dose == 2 & ToothGrowth$supp ==     "VC"), ]$len and     "OJ"), ]$len
## t = 0.0461, df = 14.04, p-value = 0.9639
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -3.638  3.798
## sample estimates:
## mean of x mean of y 
##     26.14     26.06
#The 95% confidence interval contains 0 so can not reject the null hypothesis.

#State your conclusions and the assumptions needed for your conclusions
#In the analysis I performed 3 hypothesis for different combinations of supplement and dose:
  
#H0: mu(VC, 0.5) = mu(OJ, 0.5)
#H0: mu(VC, 1.0) = mu(OJ, 1.0)
#H0: mu(VC, 2.0) = mu(OJ, 2.0)
#In the first two cases the hypothesis are been rejected, confirming that orange juice has a greater impact on tooth growth with 0.5mg and 1.0mg doses. In the third case the hypothesis is been accepted, showing that with a 2.0mg dose there isn’t differences between orange juice and vitamin C.


#Confidence Intervals and Hypothesis Testing
#Supplement as a Factor
#Analyzing the data for correlation between the delivery method and change in tooth growth:
  
  t.test(len ~ supp, paired = F, var.equal = F, data = ToothGrowth)
## 
##  Welch Two Sample t-test
## 
## data:  len by supp
## t = 1.9153, df = 55.309, p-value = 0.06063
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.1710156  7.5710156
## sample estimates:
## mean in group OJ mean in group VC 
##         20.66333         16.96333
#A confidence interval of [-0.171, 7.571] does not allow us to reject the null hypothesis (that there is no correlation between delivery method and tooth length).

#Dosage as a Factor
#Analyzing the data for correlation between the dose level and change in tooth growth:
  
dose1 <- subset(ToothGrowth, dose %in% c(0.5, 1.0))
dose2 <- subset(ToothGrowth, dose %in% c(0.5, 2.0))
dose3 <- subset(ToothGrowth, dose %in% c(1.0, 2.0))
t.test(len ~ dose, paired = F, var.equal = F, data = dose1)
## 
##  Welch Two Sample t-test
## 
## data:  len by dose
## t = -6.4766, df = 37.986, p-value = 1.268e-07
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -11.983781  -6.276219
## sample estimates:
## mean in group 0.5   mean in group 1 
##            10.605            19.735
t.test(len ~ dose, paired = F, var.equal = F, data = dose2)
## 
##  Welch Two Sample t-test
## 
## data:  len by dose
## t = -11.799, df = 36.883, p-value = 4.398e-14
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -18.15617 -12.83383
## sample estimates:
## mean in group 0.5   mean in group 2 
##            10.605            26.100
t.test(len ~ dose, paired = F, var.equal = F, data = dose3)
## 
##  Welch Two Sample t-test
## 
## data:  len by dose
## t = -4.9005, df = 37.101, p-value = 1.906e-05
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -8.996481 -3.733519
## sample estimates:
## mean in group 1 mean in group 2 
##          19.735          26.100

#The confidence intervals ([-11.98, -6.276] for doses 0.5 and 1.0, [-18.16, -12.83] for doses 0.5 and 2.0, and [-8.996, -3.734] for doses 1.0 and 2.0) allow for the rejection of the null hypothesis and a confirmation that there is a significant correlation between tooth length and dose levels.

#Suplement as a Factor within Dose Levels
#Analyzing the data for correlation between dose level and change in tooth growth within each dose level:
  
Tooth.dose5 <- subset(ToothGrowth, dose == 0.5)
Tooth.dose1 <- subset(ToothGrowth, dose == 1.0)
Tooth.dose2 <- subset(ToothGrowth, dose == 2.0)
t.test(len ~ supp, paired = F, var.equal = F, data = Tooth.dose5)
## 
##  Welch Two Sample t-test
## 
## data:  len by supp
## t = 3.1697, df = 14.969, p-value = 0.006359
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  1.719057 8.780943
## sample estimates:
## mean in group OJ mean in group VC 
##            13.23             7.98
t.test(len ~ supp, paired = F, var.equal = F, data = Tooth.dose1)
## 
##  Welch Two Sample t-test
## 
## data:  len by supp
## t = 4.0328, df = 15.358, p-value = 0.001038
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  2.802148 9.057852
## sample estimates:
## mean in group OJ mean in group VC 
##            22.70            16.77
t.test(len ~ supp, paired = F, var.equal = F, data = Tooth.dose2)
## 
##  Welch Two Sample t-test
## 
## data:  len by supp
## t = -0.0461, df = 14.04, p-value = 0.9639
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -3.79807  3.63807
## sample estimates:
## mean in group OJ mean in group VC 
##            26.06            26.14
#The confidence intervals for dose levels 0.5mg and 1.0mg([1.72, 8,78] within 0.5mg, [2.80, 9.06] within 1.0mg) allow for the rejection of the null hypothesis and a confirmation that there is a significant correlation between tooth length and dose levels. However, the confidence interval for dose level 2.0[-3.80, 3.64] is not enough to reject the null hypothesis.

#Conclusions and Assumptions
#To make conclusions with the data in this dataset, we must assume that the poplulations are independent, that the variances between populations are different, a random population was used, the population was comprised of similar guinea pigs, measurement error was accounted for with significant digits, and double blind research methods were used. For the populations to be independent, 60 guinea pigs would have to be used so each combination of dose level and delivery method were not affected by the other methods. To ensure double blind research methods are followed, the researchers taking the measurements must have been unaware of which guinea pigs were given which dose level or delivery method. The guinea pigs must also be unaware that they are being given a specific treatment.

#If all the preceding assumptions are true, we may infer that there is a significant difference between tooth length and dose levels across both delivery methods. A higher dose level consistently led to longer teeth. Initially it appeared that the delivery method had no significant impact on tooth length, but when controlling for dose level we discovered that there was a significant difference at 0.5mg and 1.0mg, but not at 2.0mg. Based on this evidence, it appears that orange juice is a better delivery method with a larger impact on tooth length for a given dose of Vitamin C, but above a maximum dose level there is no further improvement.

