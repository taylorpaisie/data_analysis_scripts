setwd("/Users/taylorpaisie/Dropbox (UFL)/cholera/september_2018/env_paper/all_samples/")
setwd("/Users/tpaisie/Dropbox (UFL)/cholera/september_2018/env_paper/all_samples/")

library(ape)
library(Biostrings)
library(ggplot2)
library(ggtree)
library(phangorn)
library(treeio)
library(RColorBrewer)

source <- read.csv2(file = "clin_env_source_062118.txt", header = TRUE, sep = "\t")
ml_tree <- read.newick("all_vc_final_snps_062018.fa.tree")


#trying to highlight clades
q <- ggtree(ml_tree, ladderize = TRUE, layout = "rectangular") #+ geom_hilight(node = 280, fill = "darkgray") + 
  #geom_hilight(node = 299, fill = "darkgray") + geom_hilight(node = 301, fill = "darkgray")
d <- q$data
d <- d[!d$isTip,]
d$label <- as.numeric(d$label)
d <- d[d$label > 90,]
p <- q + geom_nodepoint(data=d, aes(label=label), size = 3.5, shape = 18) + geom_treescale(offset = 4)

dd <- data.frame(source, check.rows = TRUE, check.names = TRUE)
row.names(dd) <- NULL

# cb_palette <- c("Northern Africa" = "navajowhite", "Angola" = "khaki3", "South America" = "goldenrod1", "Middle East" = "darkgrey", 
#                 "Asia" = "red1", "Western Africa" = "seagreen", "Burundi" = "purple4", "Cameroon" = "slategray4", "CAR" = "palevioletred3", 
#                 "Chad" = "tan2", "Eastern Africa" = "springgreen", "DRC" = "blue", "North America" = "#66cc99", 
#                 "Europe" = "#b15928", "Malawi" = "darkorchid", "Rwanda" = "sienna4", "Sao Tome" = "#56B4E9", 
#                 "Uganda" = "deeppink", "Zambia" = "plum")


p <- p %<+% dd + geom_tippoint(aes(color=Source), size = 4) + 
  scale_color_manual(values = c("Environmental" = "springgreen3", "Clinical" = "mediumpurple4")) + #scale_y_reverse() +
  theme_tree()
plot(p)

#print(p, newpage = TRUE, vp=grid::viewport(angle=-30))

#+ 
  #theme_tree(legend.position = "right")

#+ geom_text2(aes(subset=!isTip, label=node), hjust=-.3)

#plot(p)

cpos = get_clade_position(p, node=271)
p2 = with(cpos, p+xlim(xmin, xmax)+ylim(ymin, ymax))
plot(p2)
