
##The envirnoment setting

###For Part 1

require(ggplot2)

###For Part 2

data("ToothGrowth")

## Data Setting for Step 1

set.seed(1)
lam <- 0.2
n<- 40
sim<- 1000

###Analysis for exponentials

sim_expo <- replicate(sim, rexp(n, lam))

mean_expo <- apply(sim_expo, 2, mean)

#### Q1 & Q2

Theo_Mean <- 1/lam
Actu_Mean <- mean(mean_expo)
Theo_SD <- ((1/lam)*(1/sqrt(n)))
Actu_SD <- sd(mean_expo)
Theo_Var <- Theo_SD^2
Actu_Var <- var(mean_expo)

Theo_Mean
Actu_Mean
Theo_SD
Actu_SD
Theo_Var
Actu_Var

#### Q3
##### Visalisation

dfmean_expo<-data.frame(mean_expo)
plotting<-ggplot(dfmean_expo, aes(x=mean_expo))
plotting<-plotting + geom_histogram(binwidth = lam, fill="blue", color="black", aes(y = ..density..))
plotting<-plotting + labs(x="Mean", y="Density")
plotting<-plotting + geom_vline(xintercept=Actu_Mean, size=0.5, color= "pink")
plotting<-plotting + stat_function(fun=dnorm, args=list(mean=Actu_Mean, sd=Actu_SD),color="pink",size=1)
plotting<-plotting + geom_vline(xintercept=Theo_Mean, size=0.5, color="green")
plotting<-plotting + stat_function(fun=dnorm, args=list(mean=Theo_Mean, sd=Theo_SD),color="green",size=1)

plotting


