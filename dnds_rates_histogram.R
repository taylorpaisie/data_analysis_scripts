setwd("/Users/tpaisie/Dropbox (UFL)/cholera/september_2018/env_paper/selection/")


library(ggplot2)
library(tidyr)
library(dplyr)

#ds <- read.table(file = "dS_rates.txt", sep = '\t', header = TRUE)
#dn <- read.table(file = 'dN_rates.txt', sep = '\t', header = TRUE)

dnds <- read.table(file = "dNdS_rates_101018.txt", sep = '\t', header = TRUE)
#dnds <- dnds[1:3,]

attach(dnds)
dS <- ggplot(dnds, aes(x=X, y=Clinical.mean.dS)) + geom_col(fill = "mediumpurple4") + 
  geom_errorbar(aes(ymin = Clinical.mean.dS - Clincal.SD.dS, ymax = Clinical.mean.dS + Clincal.SD.dS)) + 
  ggtitle("Clincal dS Rates") + xlab("Weighted Average") + ylab("Mean dS Rates") + ylim(0, 0.02)
plot(dS)

envds <- ggplot(dnds, aes(x=X, y=Env.mean.dS)) + geom_col(fill = "springgreen3") + 
  geom_errorbar(aes(ymin = Env.mean.dS - Env.SD.dS, ymax = Env.mean.dS + Env.SD.dS)) + 
  ggtitle("Environmental dS Rates") + xlab("Weighted Average") + ylab("Mean dS Rates") + ylim(0, 0.02)
plot(envds)

dN <- ggplot(dnds, aes(x=X, y=Clinical.mean.dN)) + geom_col(fill = "mediumpurple4") + 
  geom_errorbar(aes(ymin = Clinical.mean.dN - Clinical..SD.dN, ymax = Clinical.mean.dN + Clinical..SD.dN)) + 
  ggtitle("Clincal dN Rates") + xlab("Weighted Average") + ylab("Mean dN Rates") + ylim(0, 0.02)
plot(dN)

envdn <- ggplot(dnds, aes(x=X, y=Env.mean.dN)) + geom_col(fill = "springgreen3") + 
  geom_errorbar(aes(ymin = Env.mean.dN - Env.SD.dN, ymax = Env.mean.dN + Env.SD.dN)) + 
  ggtitle("Environmental dN Rates") + xlab("Weighted Average") + ylab("Mean dN Rates") + ylim(0, 0.02)
plot(envdn)

multiplot(dS, envds, dN, envdn, layout = matrix(c(1,2,3,4), byrow=TRUE, nrow = 2))

          