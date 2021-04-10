#Setting environment

library(datasets)
library(dplyr)
library(rmarkdown)
library(reshape2)
library(cowplot)
library(knitr)
library(ggplot2)
#Getting the data

##Get data
data("ToothGrowth")

#Show the sample data
head(ToothGrowth)



#Show the summary

summary(ToothGrowth)

#Compare tooth growth by supp and dose

##Visionalise

plotting2<- ggplot(ToothGrowth,aes(x=factor(dose),y=len,fill=factor(dose))) + 
  geom_boxplot(notch=F)
plotting2<- plotting2 + facet_grid(.~supp)
plotting2<- plotting2 + scale_x_discrete("Dosage_OJ_or_VC")  
plotting2<- plotting2 + scale_y_continuous("Tooth Length")
plotting2<- plotting2 + scale_fill_discrete(name="Dose")
plotting2<- plotting2 + ggtitle("Histogram of Compare tooth growth by supp and dose")


##Hypothesis testing

###Supp
type1 = ToothGrowth$len[ToothGrowth$supp == 'OJ']
type2 = ToothGrowth$len[ToothGrowth$supp == 'VC']

t.test(type1, type2, alternative = "greater", paired = FALSE, var.equal = FALSE, conf.level = 0.95)

###Dose
level1 = ToothGrowth$len[ToothGrowth$dose == 0.5]
level2 = ToothGrowth$len[ToothGrowth$dose == 1]
level3 = ToothGrowth$len[ToothGrowth$dose == 2]

t.test(level1, level2, alternative = "less", paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(level2, level3, alternative = "less", paired = FALSE, var.equal = FALSE, conf.level = 0.95)

