setwd("/Users/taylorpaisie/Dropbox (UFL)/cholera/september_2018/env_paper/date_randomization_tests/clin_and_env/rep10/log_files/")

library(TipDatingBeast)
library(utils)
library(stats)
library(mclust)
library(DescTools)
library(TeachingDemos)

#env_only
RandomDates(name = "env_only_snps_sc_const_50", reps = 10)
PlotDRT(name = "env_only_snps_sc_const_50.", reps = 10, burnin = 0.1)

#clin and env
RandomDates(name = "vc_rep1_new_CM", reps = 10)

PlotDRT(name = "vc_rep1_new_CM", reps = 10, burnin = 0.1)
PlotDRT(name = "vc_rep2_new_CM", reps = 10, burnin = 0.1)
PlotDRT(name = "vc_rep3_new_CM", reps = 10, burnin = 0.1)
PlotDRT(name = "vc_rep4_new_CM", reps = 10, burnin = 0.1)
PlotDRT(name = "vc_rep5_new_CM", reps = 10, burnin = 0.1)
PlotDRT(name = "vc_rep6_new_CM", reps = 10, burnin = 0.1)
PlotDRT(name = "vc_rep7_new_CM", reps = 10, burnin = 0.1)
PlotDRT(name = "vc_rep8_new_CM", reps = 10, burnin = 0.1)
PlotDRT(name = "vc_rep9_new_CM", reps = 10, burnin = 0.1)
PlotDRT(name = "vc_rep10_new_CM", reps = 10, burnin = 0.1)

#clin and env

