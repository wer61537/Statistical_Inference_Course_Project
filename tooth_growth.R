library(plyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(knitr, warn.conflicts = FALSE)
library(xtable, warn.conflicts = FALSE)
library('RColorBrewer')

#set color scheme for graphing
blues <- brewer.pal(5, 'Blues')[3:5]
reds <- brewer.pal(5, 'Reds')[3:5]

#get the dataset that is part of R
data(ToothGrowth)

#get a look at the dataset
summary(ToothGrowth)
str(ToothGrowth)

#take a look at the unique values for each of the variables
unique(ToothGrowth$len)
unique(ToothGrowth$supp)
unique(ToothGrowth$dose)

#to get a sense of the interactions and spread of data, 
#interactions are calculated and the plotted with 
#a box plot
toothInt <- ddply(ToothGrowth,.(dose,supp),summarise, val = mean(len))

#figure 1
ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
  ggtitle("ToothGrowth Length vs Supplement by Dose with Interaction") + 
  geom_boxplot (aes(fill=factor(supp))) + geom_jitter() + facet_grid(.~dose) + 
  geom_point(data = toothInt, aes(y = val)) +
  geom_line(data = toothInt, aes(y = val, group = dose)) + 
  theme_bw()

#figure 2
ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  ggtitle("ToothGrowth Length vs Dose by Supplement with Interaction") + 
  geom_boxplot (aes(fill=factor(dose))) + geom_jitter() + facet_grid(.~supp) + 
  geom_point(data = toothInt, aes(y = val)) +
  geom_line(data = toothInt, aes(y = val, group = supp)) + 
  theme_bw()

#Summary of guinea pigs given 0.5, 1.0 and 2.0 mg doses of Vitamin C and Orange Juice
t<-rbind(
  summary(ToothGrowth[which(ToothGrowth$dose==0.5 & ToothGrowth$supp=='VC'),]$len),
  summary(ToothGrowth[which(ToothGrowth$dose==1.0 & ToothGrowth$supp=='VC'),]$len),
  summary(ToothGrowth[which(ToothGrowth$dose==2.0 & ToothGrowth$supp=='VC'),]$len),
  
  summary(ToothGrowth[which(ToothGrowth$dose==0.5 & ToothGrowth$supp=='OJ'),]$len),
  summary(ToothGrowth[which(ToothGrowth$dose==1.0 & ToothGrowth$supp=='OJ'),]$len),
  summary(ToothGrowth[which(ToothGrowth$dose==2.0 & ToothGrowth$supp=='OJ'),]$len)
  
)
#set the row and column names
dimnames(t) <- list(
  c("0.5 mg Vitamin C", "1.0 mg Vitamin C", "2.0 mg Vitamin C",
    "1.0 mg Orange Juice","1.5 mg Orange Juice","2.0 mg Orange Juice"),
  c("Min.",    "1st Qu.", "Median",  "Mean",    "3rd Qu.", "Max."))

#show the table
t
#genterate the html to create the table for Markdown
print(xtable(t), type = "html")
anova.out <- aov(len ~ supp * dose, data=ToothGrowth)
summary(anova.out)
#create html for the report
print(xtable(summary(anova.out)), type = "html")


#not so sure this is correct
ddply(ToothGrowth, dose ~ supp,function(x) 
  c("t-statistic"=t.test(x$len)$statistic,"p-value" =t.test(x$len)$p.value, mean=mean(x$len), sd=sd(x$len),
    conf.int=t.test(x$len)$conf.int))

#get VC and OJ variance
var(ToothGrowth[which(ToothGrowth$supp=='VC'),]$len)
var(ToothGrowth[which(ToothGrowth$supp=='OJ'),]$len)

#Use confidence intervals and hypothesis tests to compare tooth growth by supp and dose

#Caluculate the 95% confidence interval (c.i) each dose and each supplement
ci<-as.data.frame(
  list(
    round(t.test(ToothGrowth$len[ToothGrowth$dose==0.5])$conf.int,2),
    round(t.test(ToothGrowth$len[ToothGrowth$dose==1.0])$conf.int,2),
    round(t.test(ToothGrowth$len[ToothGrowth$dose==2.0])$conf.int,2),
    round(t.test(ToothGrowth$len[ToothGrowth$supp=='OJ'])$conf.int,2),
    round(t.test(ToothGrowth$len[ToothGrowth$supp=='VC'])$conf.int,2)
  )
)
#relabel
row.names(ci)=c('Lower Limit','Upper Limit')
names(ci)=c('0.5 mg/day','1.0 mg/day','2.0 mg/day','OJ','VC')
#generate html
print(xtable(ci), type = "html")

#p-values for dose and supplement compare
pvalues<-as.data.frame(
  list(
    t.test(ToothGrowth$len[ToothGrowth$dose==0.5],ToothGrowth$len[ToothGrowth$dose==1])$p.value,
    t.test(ToothGrowth$len[ToothGrowth$dose==0.5],ToothGrowth$len[ToothGrowth$dose==2])$p.value,
    t.test(ToothGrowth$len[ToothGrowth$dose==1.0],ToothGrowth$len[ToothGrowth$dose==2])$p.value,
    t.test(ToothGrowth$len[ToothGrowth$supp=="VC"],ToothGrowth$len[ToothGrowth$supp=="OJ"])$p.value
  )
)
#relabel
row.names(pvalues)<-c('p-value')
names(pvalues)<-c('0.5 mg/day vs 1.0 mg/day','0.5 mg/day vs 2.0 mg/day','1.0 mg/day vs 2.0 mg/day','VC vs OJ')
#generate html
print(xtable(pvalues), type = "html")

#ci's for each combination of dose and supplement level
ci_combos<-as.data.frame(
  list(
    round(t.test(ToothGrowth$len[ToothGrowth$supp=="OJ" & ToothGrowth$dose==0.5])$conf.int,2),
    round(t.test(ToothGrowth$len[ToothGrowth$supp=="OJ" & ToothGrowth$dose==1])$conf.int,2),
    round(t.test(ToothGrowth$len[ToothGrowth$supp=="OJ" & ToothGrowth$dose==2])$conf.int,2),
    
    round(t.test(ToothGrowth$len[ToothGrowth$supp=="VC" & ToothGrowth$dose==0.5])$conf.int,2),
    round(t.test(ToothGrowth$len[ToothGrowth$supp=="VC" & ToothGrowth$dose==1])$conf.int,2),
    round(t.test(ToothGrowth$len[ToothGrowth$supp=="VC" & ToothGrowth$dose==2])$conf.int,2)
  )
)
#relabel
row.names(ci_combos)<-c('Lower Limit','Upper Limit')
names(ci_combos)=c('OJ @0.5 mg/day','OJ @ 1 mg/day','OJ @2 mg/day','VC @0.5 mg/day','VC @1mg/day','VC @2 mg/day')
#generate html
print(xtable(ci_combos), type = "html")
ci_combos

#p-values for combos
pvalues_combos<-as.data.frame(
  list(
    t.test(ToothGrowth$len[ToothGrowth$supp=="OJ" & ToothGrowth$dose==0.5],ToothGrowth$len[ToothGrowth$supp=="VC" & ToothGrowth$dose==0.5])$p.value,
    t.test(ToothGrowth$len[ToothGrowth$supp=="OJ" & ToothGrowth$dose==1],ToothGrowth$len[ToothGrowth$supp=="VC" & ToothGrowth$dose==1])$p.value,
    t.test(ToothGrowth$len[ToothGrowth$supp=="OJ" & ToothGrowth$dose==2],ToothGrowth$len[ToothGrowth$supp=="VC" & ToothGrowth$dose==2])$p.value
  )
)
#relabel
row.names(pvalues_combos)<-c('p-value')
names(pvalues_combos)<-c('OJ vs VC @0.5 mg/day','OJ vs VC @1.0 mg/day','OJ vs VC @2.0 mg/day')
#generate html
print(xtable(pvalues_combos), type = "html")
#xtable reformats digits so get them from raw data
pvalues_combos

