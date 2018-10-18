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

# env_loc <- read.csv2(file = "env_sample_locations_081718.csv", sep = ",", header = TRUE)
# loc <- read.csv2(file = "haiti_cholera_locations_072018.csv", sep = ",", header = TRUE)

# dd <- data.frame(loc1, check.rows = TRUE, check.names = TRUE)
# row.names(dd) <- NULL

# loc1 <- read.csv2(file = "vc_rep1_locations.csv", sep = ",", header = TRUE)
# dd1 <- data.frame(loc1, check.rows = TRUE, check.names = TRUE)
# row.names(dd1) <- NULL

p1 <- ggtree(mcc1, aes(color = Location), mrsd = "2015-12-05", size = 0.5) + scale_color_manual(values=c("mediumpurple4", "springgreen3")) +
  geom_point2(aes(label = posterior, subset = posterior >= 0.9), size = 2.5) + 
  scale_x_continuous(breaks = seq(2010, 2017)) + theme_tree2() + geom_tiplab(size=1.5, color="black")
#scale_color_manual(values = c("Ouest" = "#ffcc66", "Centre" = "black","Sud" = "#ff6666", "Sud-Est" = "#ff00ff", "Artibonite" = "#66ccff"))
plot(p1)

p2 <- ggtree(mcc2, aes(color = Location), mrsd = "2015-12-05", size = 0.5) + scale_color_manual(values=c("mediumpurple4", "springgreen3")) +
  geom_point2(aes(label = posterior, subset = posterior >= 0.9), size = 2.5) + 
  scale_x_continuous(breaks = seq(2010, 2017)) + theme_tree2() + geom_tiplab(size=1.5, color="black")
plot(p2)

p3 <- ggtree(mcc3, aes(color = Location), mrsd = "2015-12-03", size = 0.5) + scale_color_manual(values=c("mediumpurple4", "springgreen3")) +
  geom_point2(aes(label = posterior, subset = posterior >= 0.9), size = 2.5) + 
  scale_x_continuous(breaks = seq(2010, 2017)) + theme_tree2() + geom_tiplab(size=1.5, color="black")
plot(p3)

p4 <- ggtree(mcc4, aes(color = Location), mrsd = "2015-12-05", size = 0.5) + scale_color_manual(values=c("mediumpurple4", "springgreen3")) +
  geom_point2(aes(label = posterior, subset = posterior >= 0.9), size = 2.5) + 
  scale_x_continuous(breaks = seq(2010, 2017)) + theme_tree2() + geom_tiplab(size=1.5, color="black")
plot(p4)

p5 <- ggtree(mcc5, aes(color = Location), mrsd = "2015-12-05", size = 0.5) + scale_color_manual(values=c("mediumpurple4", "springgreen3")) +
  geom_point2(aes(label = posterior, subset = posterior >= 0.9), size = 2.5) + 
  scale_x_continuous(breaks = seq(2010, 2017)) + theme_tree2() + geom_tiplab(size=1.5, color="black")
plot(p5)

p6 <- ggtree(mcc6, aes(color = Location), mrsd = "2015-12-05", size = 0.5) + scale_color_manual(values=c("mediumpurple4", "springgreen3")) +
  geom_point2(aes(label = posterior, subset = posterior >= 0.9), size = 2.5) + 
  scale_x_continuous(breaks = seq(2010, 2017)) + theme_tree2() + geom_tiplab(size=1.5, color="black")
plot(p6)

p7 <- ggtree(mcc7, aes(color = location), mrsd = "2015-12-05", size = 0.5) + scale_color_manual(values=c("mediumpurple4", "springgreen3")) +
  geom_point2(aes(label = posterior, subset = posterior >= 0.9), size = 2.5) + 
  scale_x_continuous(breaks = seq(2010, 2017)) + theme_tree2() + geom_tiplab(size=1.5, color="black")
plot(p7)

p8 <- ggtree(mcc8, aes(color = Location), mrsd = "2015-11-12", size = 0.5) + scale_color_manual(values=c("mediumpurple4", "springgreen3")) +
  geom_point2(aes(label = posterior, subset = posterior >= 0.9), size = 2.5) + 
  scale_x_continuous(breaks = seq(2010, 2017)) + theme_tree2() + geom_tiplab(size=1.5, color="black")
plot(p8)

p9 <- ggtree(mcc9, aes(color = Location), mrsd = "2015-12-05", size = 0.5) + scale_color_manual(values=c("mediumpurple4", "springgreen3")) +
  geom_point2(aes(label = posterior, subset = posterior >= 0.9), size = 2.5) + 
  scale_x_continuous(breaks = seq(2010, 2017)) + theme_tree2() + geom_tiplab(size=1.5, color="black")
plot(p9)

p10 <- ggtree(mcc10, aes(color = Location), mrsd = "2015-12-05", size = 0.5) + scale_color_manual(values=c("mediumpurple4", "springgreen3")) +
  geom_point2(aes(label = posterior, subset = posterior >= 0.9), size = 2.5) + 
  scale_x_continuous(breaks = seq(2010, 2017)) + theme_tree2() + geom_tiplab(size=1.5, color="black")
plot(p10)

# plot all trees on one pdf
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol = 2)

attach(dd1)
p1 <- ggtree(mcc1, mrsd = "2015-12-05", size = 1, ladderize = TRUE) + #scale_color_manual(values=c("mediumpurple4", "springgreen3")) + 
  geom_point2(aes(label = posterior, subset = posterior >= 0.9), size = 2.5) + 
  scale_x_continuous(breaks = seq(2010, 2016)) + theme_tree2()

p1 <- p1 %<+% dd1 + geom_tippoint(aes(color=Location), size = 2)

(p1 + scale_y_continuous(expand=c(0, 0.3))) %>% gheatmap(loc1, offset=0, width = 0.05, colnames=FALSE) + 
  scale_fill_manual(breaks = c("Ouest", "Centre", "Sud", "Sud-Est", "Artibonite"), 
                    values = c("Ouest" = "#ffcc66", "Centre" = "black","Sud" = "#ff6666", "Sud-Est" = "#ff00ff", "Artibonite" = "#66ccff"))
plot(p1)




## 3.6 Show the heatmap-like box based on the external meta-data file
p <- ggtree(mcc1, mrsd = "2015-12-05", size = 1, ladderize = TRUE) + geom_point2(aes(label = posterior, subset = posterior >= 0.9), size = 2.5, shape = 18) + 
  scale_x_continuous(breaks = seq(2010, 2016)) #+ geom_tiplab(size=3, color="black")

# p <- p %<+% loc1 + geom_tippoint(aes(color=Location), size = 2.5) + 
#   scale_color_manual(values = c("Ouest" = "#ffcc66", "Centre" = "black","Sud" = "#ff6666", "Sud-Est" = "#ff00ff", "Artibonite" = "#66ccff"))
# pp <- (p1 + scale_y_continuous(expand=c(0, 0.3))) %>% gheatmap(loc1, offset=0, width = 0.05, colnames=FALSE) + 
#   scale_color_manual(breaks = c("Ouest", "Centre", "Sud", "Sud-Est", "Artibonite"), 
#                      values = c("Ouest" = "#ffcc66", "Centre" = "black","Sud" = "#ff6666", "Sud-Est" = "#ff00ff", "Artibonite" = "#66ccff")) + 
#   theme_tree2(legend.position = "right")


plot(p1)

ph <- gheatmap(p1, dd1) + scale_fill_manual(values = c("Ouest" = "#ffcc66", "Centre" = "black","Sud" = "#ff6666", "Sud-Est" = "#ff00ff", "Artibonite" = "#66ccff"))
plot(ph)
