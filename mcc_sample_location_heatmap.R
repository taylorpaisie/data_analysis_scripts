setwd("/Users/taylorpaisie/Dropbox (UFL)/cholera/september_2018/env_paper/random_sampling/rep_mcc_trees/")

library(ggtree)
library(ape)
library(ggplot2)
library(colorspace)
library(Biostrings)
library(phytools)


#clinical-environmental tree
mcc1 <- read.beast(file = "vc_rep1_MCC.tree")
mcc2 <- read.beast(file = "vc_rep2_MCC.tree")
mcc3 <- read.beast(file = "vc_rep3_MCC.tree")
mcc4 <- read.beast(file = "vc_rep4_MCC.tree")
mcc5 <- read.beast(file = "vc_rep5_MCC.tree")
mcc6 <- read.beast(file = "vc_rep6_MCC.tree")
mcc7 <- read.beast(file = "vc_rep7_MCC.tree")
mcc8 <- read.beast(file = "vc_rep8_MCC.tree")
mcc9 <- read.beast(file = "vc_rep9_MCC.tree")
mcc10 <- read.beast(file = "vc_rep10_MCC.tree")

loc1 <- read.csv2(file = "vc_rep1_locations.csv", sep = ",", header = TRUE, row.names = 1)
loc2 <- read.csv2(file = "vc_rep2_locations.csv", sep = ",", header = TRUE, row.names = 1)
loc3 <- read.csv2(file = "vc_rep3_locations.csv", sep = ",", header = TRUE, row.names = 1)
loc4 <- read.csv2(file = "vc_rep4_locations.csv", sep = ",", header = TRUE, row.names = 1)
loc5 <- read.csv2(file = "vc_rep5_locations.csv", sep = ",", header = TRUE, row.names = 1)
loc6 <- read.csv2(file = "vc_rep6_locations.csv", sep = ",", header = TRUE, row.names = 1)
loc7 <- read.csv2(file = "vc_rep7_locations.csv", sep = ",", header = TRUE, row.names = 1)
loc8 <- read.csv2(file = "vc_rep8_locations.csv", sep = ",", header = TRUE, row.names = 1)
loc9 <- read.csv2(file = "vc_rep9_locations.csv", sep = ",", header = TRUE, row.names = 1)
loc10 <- read.csv2(file = "vc_rep10_locations.csv", sep = ",", header = TRUE, row.names = 1)
# loc1 <- read.table("vc_rep1_locations.txt", sep = "\t", header = TRUE)
# #dd1 <- data.frame(loc1, check.rows = TRUE, check.names = TRUE)
# #dd1 <- data.frame(row.names=seq(1,dd1))
# row.names(dd1) <- NULL
# head(dd1)
# attach(loc1)
# 
# data.frame(row.names=seq(1,data_length))

p1 <- ggtree(mcc1, mrsd = "2015-12-05", size = 0.5, ladderize = TRUE)
p2 <- ggtree(mcc2, mrsd = "2015-12-05", size = 0.5, ladderize = TRUE)
p3 <- ggtree(mcc3, mrsd = "2015-12-05", size = 0.5, ladderize = TRUE)
p4 <- ggtree(mcc4, mrsd = "2015-12-05", size = 0.5, ladderize = TRUE)
p5 <- ggtree(mcc5, mrsd = "2015-12-05", size = 0.5, ladderize = TRUE)
p6 <- ggtree(mcc6, mrsd = "2015-12-05", size = 0.5, ladderize = TRUE)
p7 <- ggtree(mcc7, mrsd = "2015-12-05", size = 0.5, ladderize = TRUE)
p8 <- ggtree(mcc8, mrsd = "2015-12-05", size = 0.5, ladderize = TRUE)
p9 <- ggtree(mcc9, mrsd = "2015-12-05", size = 0.5, ladderize = TRUE)
p10 <- ggtree(mcc10, mrsd = "2015-12-05", size = 0.5, ladderize = TRUE)

plot(p8)

  #geom_point2(aes(label = posterior, subset = posterior >= 0.9), size = 2.5, shape = 18) + 
  #scale_x_continuous(breaks = seq(2010, 2016))
# p <- p %<+% dd1 + geom_tippoint(aes(color=as.factor(Zone), size = 2.5)) + 
#   scale_color_manual(values = c("1" = "#ffcc66","2" = "#ff6666", "3" = "#ff8c00", "4" = "#66ccff", "5" = "#ff00ff", "6" = "#355F91", "7" = "#707070"))

# 
# p  %>% gheatmap(loc1, offset=1, width=0.6) + 
#   scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6", "7"), 
#                      values = c("1" = "#ffcc66","2" = "#ff6666", "3" = "#ff8c00", "4" = "#66ccff", "5" = "#ff00ff", "6" = "#355F91", "7" = "#707070"))
# plot(p)
# 
# #+ scale_y_continuous(expand=c(0, 0.3)))
# 
# ph <- gheatmap(p, loc1, width = 1) + 
#   scale_fill_manual(values = c("1" = "#ffcc66","2" = "#ff6666", "3" = "#ff8c00", "4" = "#66ccff", "5" = "#ff00ff", "6" = "#355F91", "7" = "#707070"))
# plot(ph)


p1 <- (p1 + scale_y_continuous(expand=c(0, 0.3))) %>% gheatmap(loc1, offset=0, width = 0.05, colnames = FALSE) + 
    scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6", "7"),
                       values = c("1" = "#ffcc66","2" = "#ff6666", "3" = "#ff8c00", "4" = "#66ccff", "5" = "#ff00ff", "6" = "#355F91", "7" = "#707070")) +
    theme_tree2(legend.position = "right")
plot(p1)

p2 <- (p2 + scale_y_continuous(expand=c(0, 0.3))) %>% gheatmap(loc2, offset=0, width = 0.05, colnames = FALSE) + 
  scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6", "7"),
                     values = c("1" = "#ffcc66","2" = "#ff6666", "3" = "#ff8c00", "4" = "#66ccff", "5" = "#ff00ff", "6" = "#355F91", "7" = "#707070")) +
  theme_tree2(legend.position = "right")
plot(p2)

p3 <- (p3 + scale_y_continuous(expand=c(0, 0.3))) %>% gheatmap(loc3, offset=0, width = 0.05, colnames = FALSE) + 
  scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6", "7"),
                     values = c("1" = "#ffcc66","2" = "#ff6666", "3" = "#ff8c00", "4" = "#66ccff", "5" = "#ff00ff", "6" = "#355F91", "7" = "#707070")) +
  theme_tree2(legend.position = "right")
plot(p3)

p4 <- (p4 + scale_y_continuous(expand=c(0, 0.3))) %>% gheatmap(loc4, offset=0, width = 0.05, colnames = FALSE) + 
  scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6", "7"),
                     values = c("1" = "#ffcc66","2" = "#ff6666", "3" = "#ff8c00", "4" = "#66ccff", "5" = "#ff00ff", "6" = "#355F91", "7" = "#707070")) +
  theme_tree2(legend.position = "right")
plot(p4)

p5 <- (p5 + scale_y_continuous(expand=c(0, 0.3))) %>% gheatmap(loc5, offset=0, width = 0.05, colnames = FALSE) + 
  scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6", "7"),
                     values = c("1" = "#ffcc66","2" = "#ff6666", "3" = "#ff8c00", "4" = "#66ccff", "5" = "#ff00ff", "6" = "#355F91", "7" = "#707070")) +
  theme_tree2(legend.position = "right")
plot(p5)

p6 <- (p6 + scale_y_continuous(expand=c(0, 0.3))) %>% gheatmap(loc6, offset=0, width = 0.05, colnames = FALSE) + 
  scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6", "7"),
                     values = c("1" = "#ffcc66","2" = "#ff6666", "3" = "#ff8c00", "4" = "#66ccff", "5" = "#ff00ff", "6" = "#355F91", "7" = "#707070")) +
  theme_tree2(legend.position = "right")
plot(p6)

p7 <- (p7 + scale_y_continuous(expand=c(0, 0.3))) %>% gheatmap(loc7, offset=0, width = 0.05, colnames = FALSE) + 
  scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6", "7"),
                     values = c("1" = "#ffcc66","2" = "#ff6666", "3" = "#ff8c00", "4" = "#66ccff", "5" = "#ff00ff", "6" = "#355F91", "7" = "#707070")) +
  theme_tree2(legend.position = "right")
plot(p7)

p8 <- (p8 + scale_y_continuous(expand=c(0, 0.3))) %>% gheatmap(loc8, offset=0, width = 0.05, colnames = FALSE) + 
  scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6", "7"),
                     values = c("1" = "#ffcc66","2" = "#ff6666", "3" = "#ff8c00", "4" = "#66ccff", "5" = "#ff00ff", "6" = "#355F91", "7" = "#707070")) +
  theme_tree2(legend.position = "right") + geom_tiplab(size=1.5, color="black")
plot(p8)

p9 <- (p9 + scale_y_continuous(expand=c(0, 0.3))) %>% gheatmap(loc9, offset=0, width = 0.05, colnames = FALSE) + 
  scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6", "7"),
                     values = c("1" = "#ffcc66","2" = "#ff6666", "3" = "#ff8c00", "4" = "#66ccff", "5" = "#ff00ff", "6" = "#355F91", "7" = "#707070")) +
  theme_tree2(legend.position = "right")
plot(p9)

p10 <- (p10 + scale_y_continuous(expand=c(0, 0.3))) %>% gheatmap(loc10, offset=0, width = 0.05, colnames = FALSE) + 
  scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6", "7"),
                     values = c("1" = "#ffcc66","2" = "#ff6666", "3" = "#ff8c00", "4" = "#66ccff", "5" = "#ff00ff", "6" = "#355F91", "7" = "#707070")) +
  theme_tree2(legend.position = "right")
plot(p10)


multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol = 2)
