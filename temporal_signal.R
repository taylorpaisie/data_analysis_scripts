setwd("/Users/taylorpaisie/Dropbox (UFL)/cholera/september_2018/env_paper/before_env/temporal_signal/")
setwd("/Users/tpaisie/Dropbox (UFL)/cholera/september_2018/env_paper/clinical_only/snps/clin_2015_2017/")

library(reshape2)
library(scales)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(plyr)
library(stats)

#clinical only samples
temp <- read.table(file = "clin_only_temp_signal_data_101018.txt", sep = '\t', header = TRUE)
head(temp)
attach(temp)

slope <- lm(formula = distance ~ date, data = temp)$coefficients[2]

intercept <- lm(formula = distance ~ date, data = temp)$coefficients[1]

ggplot(temp, aes(x=date, y=distance)) + theme_light() + geom_point() +
  geom_abline(aes(slope = slope, intercept=intercept, colour = "red")) +
  xlab("Time") + ylab("Root-to-tip Divergence") +
  scale_x_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016, 2017, 2018)) +
  scale_y_continuous(position = "left") +
  theme(axis.title.y = element_text(size=25)) +
  theme(axis.title.x = element_text(size=25)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=10))

#clin only (2010-2013 samples)
temp <- read.table(file = "clin_only_2013_temp_signal_101018.txt", sep = '\t', header = TRUE)
head(temp)
attach(temp)

slope <- lm(formula = distance ~ date, data = temp)$coefficients[2]

intercept <- lm(formula = distance ~ date, data = temp)$coefficients[1]

ggplot(temp, aes(x=date, y=distance)) + theme_light() + geom_point() +
  geom_abline(aes(slope = slope, intercept=intercept, colour = "red")) +
  xlab("Time") + ylab("Root-to-tip Divergence") +
  scale_x_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016, 2017, 2018)) +
  scale_y_continuous(position = "left") +
  theme(axis.title.y = element_text(size=25)) +
  theme(axis.title.x = element_text(size=25)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=10))



#clin only (2013-2015 samples)
temp <- read.table(file = "clin_only_2013_2015_temp_signal_101018.txt", sep = '\t', header = TRUE)
head(temp)
attach(temp)

slope <- lm(formula = distance ~ date, data = temp)$coefficients[2]

intercept <- lm(formula = distance ~ date, data = temp)$coefficients[1]

ggplot(temp, aes(x=date, y=distance)) + theme_light() + geom_point() +
  geom_abline(aes(slope = slope, intercept=intercept, colour = "red")) +
  xlab("Time") + ylab("Root-to-tip Divergence") +
  scale_x_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016, 2017, 2018)) +
  scale_y_continuous(position = "left") +
  theme(axis.title.y = element_text(size=25)) +
  theme(axis.title.x = element_text(size=25)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=10))




#clin only (2015-2017 samples)
temp <- read.table(file = "clin_only_2013_2015_temp_signal_101018.txt", sep = '\t', header = TRUE)
head(temp)
attach(temp)

slope <- lm(formula = distance ~ date, data = temp)$coefficients[2]

intercept <- lm(formula = distance ~ date, data = temp)$coefficients[1]

ggplot(temp, aes(x=date, y=distance)) + theme_light() + geom_point() +
  geom_abline(aes(slope = slope, intercept=intercept, colour = "red")) +
  xlab("Time") + ylab("Root-to-tip Divergence") +
  scale_x_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016, 2017, 2018)) +
  scale_y_continuous(position = "left") +
  theme(axis.title.y = element_text(size=25)) +
  theme(axis.title.x = element_text(size=25)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=10))





# env only
# temp <- read.table(file = "env_only_temp_signal_091218.txt", sep = '\t', header = TRUE)
# head(temp)
# attach(temp)
# 
# slope <- lm(formula = distance ~ date, data = temp)$coefficients[2]
# 
# intercept <- lm(formula = distance ~ date, data = temp)$coefficients[1]
# 
# ggplot(temp, aes(x=date, y=distance)) + theme_light() + geom_point() + 
#   geom_abline(aes(slope = slope, intercept=intercept, colour = "red")) + 
#   xlab("Time") + ylab("Root-to-tip Divergence") + 
#   scale_x_continuous(breaks = c(2008,2009,2010,2011,2012,2013,2014,2015,2016)) + 
#   scale_y_continuous(position = "left") + 
#   theme(axis.title.y = element_text(size=25)) +
#   theme(axis.title.x = element_text(size=25)) +
#   theme(axis.text.y = element_text(size=15)) +
#   theme(axis.text.x = element_text(size=10)) 


# clinical and env trees
temp1 <- read.table("clin_only_rep1.txt", sep = "\t", header = TRUE)
head(temp1)
attach(temp1)

slope <- lm(formula = distance ~ date, data = temp1)$coefficients[2]
intercept <- lm(formula = distance ~ date, data = temp1)$coefficients[1]

p1 <- ggplot(temp1, aes(x=date, y=distance)) + theme_light() + geom_point() + 
  geom_abline(aes(slope = slope, intercept=intercept, colour = "red")) + 
  xlab("Time") + ylab("Root-to-tip Divergence") + 
  scale_x_continuous(breaks = c(2008,2009,2010,2011,2012,2013,2014,2015,2016)) + 
  scale_y_continuous(position = "left") + 
  # theme(axis.title.y = element_text(size=25)) +
  # theme(axis.title.x = element_text(size=25)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=10))


temp2 <- read.table("clin_only_rep2.txt", sep = "\t", header = TRUE)
head(temp2)
attach(temp2)

slope <- lm(formula = distance ~ date, data = temp2)$coefficients[2]
intercept <- lm(formula = distance ~ date, data = temp2)$coefficients[1]

p2 <- ggplot(temp2, aes(x=date, y=distance)) + theme_light() + geom_point() + 
  geom_abline(aes(slope = slope, intercept=intercept, colour = "red")) + 
  xlab("Time") + ylab("Root-to-tip Divergence") + 
  scale_x_continuous(breaks = c(2008,2009,2010,2011,2012,2013,2014,2015,2016)) + 
  scale_y_continuous(position = "left") + 
  # theme(axis.title.y = element_text(size=25)) +
  # theme(axis.title.x = element_text(size=25)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=10))



temp3 <- read.table("clin_only_rep3.txt", sep = "\t", header = TRUE)
head(temp3)
attach(temp3)

slope <- lm(formula = distance ~ date, data = temp3)$coefficients[2]
intercept <- lm(formula = distance ~ date, data = temp3)$coefficients[1]

p3 <- ggplot(temp3, aes(x=date, y=distance)) + theme_light() + geom_point() + 
  geom_abline(aes(slope = slope, intercept=intercept, colour = "red")) + 
  xlab("Time") + ylab("Root-to-tip Divergence") + 
  scale_x_continuous(breaks = c(2008,2009,2010,2011,2012,2013,2014,2015,2016)) + 
  scale_y_continuous(position = "left") + 
  # theme(axis.title.y = element_text(size=25)) +
  # theme(axis.title.x = element_text(size=25)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=10))


temp4 <- read.table("clin_only_rep4.txt", sep = "\t", header = TRUE)
head(temp4)
attach(temp4)

slope <- lm(formula = distance ~ date, data = temp4)$coefficients[2]
intercept <- lm(formula = distance ~ date, data = temp4)$coefficients[1]

p4 <- ggplot(temp4, aes(x=date, y=distance)) + theme_light() + geom_point() + 
  geom_abline(aes(slope = slope, intercept=intercept, colour = "red")) + 
  xlab("Time") + ylab("Root-to-tip Divergence") + 
  scale_x_continuous(breaks = c(2008,2009,2010,2011,2012,2013,2014,2015,2016)) + 
  scale_y_continuous(position = "left") + 
  # theme(axis.title.y = element_text(size=25)) +
  # theme(axis.title.x = element_text(size=25)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=10))


temp5 <- read.table("clin_only_rep5.txt", sep = "\t", header = TRUE)
head(temp5)
attach(temp5)

slope <- lm(formula = distance ~ date, data = temp5)$coefficients[2]
intercept <- lm(formula = distance ~ date, data = temp5)$coefficients[1]

p5 <- ggplot(temp5, aes(x=date, y=distance)) + theme_light() + geom_point() + 
  geom_abline(aes(slope = slope, intercept=intercept, colour = "red")) + 
  xlab("Time") + ylab("Root-to-tip Divergence") + 
  scale_x_continuous(breaks = c(2008,2009,2010,2011,2012,2013,2014,2015,2016)) + 
  scale_y_continuous(position = "left") + 
  # theme(axis.title.y = element_text(size=25)) +
  theme(axis.title.x = element_text(size=25)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=10))


temp6 <- read.table("clin_only_rep6.txt", sep = "\t", header = TRUE)
head(temp6)
attach(temp6)

slope <- lm(formula = distance ~ date, data = temp6)$coefficients[2]
intercept <- lm(formula = distance ~ date, data = temp6)$coefficients[1]

p6 <- ggplot(temp6, aes(x=date, y=distance)) + theme_light() + geom_point() + 
  geom_abline(aes(slope = slope, intercept=intercept, colour = "red")) + 
  xlab("Time") + ylab("Root-to-tip Divergence") + 
  scale_x_continuous(breaks = c(2008,2009,2010,2011,2012,2013,2014,2015,2016)) + 
  scale_y_continuous(position = "left") + 
  # theme(axis.title.y = element_text(size=25)) +
  # theme(axis.title.x = element_text(size=25)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=10))



temp7 <- read.table("clin_only_rep7.txt", sep = "\t", header = TRUE)
head(temp7)
attach(temp7)

slope <- lm(formula = distance ~ date, data = temp7)$coefficients[2]
intercept <- lm(formula = distance ~ date, data = temp7)$coefficients[1]

p7 <- ggplot(temp7, aes(x=date, y=distance)) + theme_light() + geom_point() + 
  geom_abline(aes(slope = slope, intercept=intercept, colour = "red")) + 
  xlab("Time") + ylab("Root-to-tip Divergence") + 
  scale_x_continuous(breaks = c(2008,2009,2010,2011,2012,2013,2014,2015,2016)) + 
  scale_y_continuous(position = "left") + 
  # theme(axis.title.y = element_text(size=25)) +
  # theme(axis.title.x = element_text(size=25)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=10))


temp8 <- read.table("clin_only_rep8.txt", sep = "\t", header = TRUE)
head(temp8)
attach(temp8)

slope <- lm(formula = distance ~ date, data = temp8)$coefficients[2]
intercept <- lm(formula = distance ~ date, data = temp8)$coefficients[1]

p8 <- ggplot(temp8, aes(x=date, y=distance)) + theme_light() + geom_point() + 
  geom_abline(aes(slope = slope, intercept=intercept, colour = "red")) + 
  xlab("Time") + ylab("Root-to-tip Divergence") + 
  scale_x_continuous(breaks = c(2008,2009,2010,2011,2012,2013,2014,2015,2016)) + 
  scale_y_continuous(position = "left") + 
  # theme(axis.title.y = element_text(size=25)) +
  # theme(axis.title.x = element_text(size=25)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=10))


temp9 <- read.table("clin_only_rep9.txt", sep = "\t", header = TRUE)
head(temp9)
attach(temp9)

slope <- lm(formula = distance ~ date, data = temp9)$coefficients[2]
intercept <- lm(formula = distance ~ date, data = temp9)$coefficients[1]

p9 <- ggplot(temp9, aes(x=date, y=distance)) + theme_light() + geom_point() + 
  geom_abline(aes(slope = slope, intercept=intercept, colour = "red")) + 
  xlab("Time") + ylab("Root-to-tip Divergence") + 
  scale_x_continuous(breaks = c(2008,2009,2010,2011,2012,2013,2014,2015,2016)) + 
  scale_y_continuous(position = "left") + 
  # theme(axis.title.y = element_text(size=25)) +
  # theme(axis.title.x = element_text(size=25)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=10))


temp10 <- read.table("clin_only_rep10.txt", sep = "\t", header = TRUE)
head(temp10)
attach(temp10)

slope <- lm(formula = distance ~ date, data = temp10)$coefficients[2]
intercept <- lm(formula = distance ~ date, data = temp10)$coefficients[1]

p10 <- ggplot(temp10, aes(x=date, y=distance)) + theme_light() + geom_point() + 
  geom_abline(aes(slope = slope, intercept=intercept, colour = "red")) + 
  xlab("Time") + ylab("Root-to-tip Divergence") + 
  scale_x_continuous(breaks = c(2008,2009,2010,2011,2012,2013,2014,2015,2016)) + 
  scale_y_continuous(position = "left") + 
  #theme(axis.title.y = element_text(size=25)) +
  theme(axis.title.x = element_text(size=25)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=10))

multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, cols = 2)
