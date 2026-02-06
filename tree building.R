install.packages("ape")
install.packages("ggtree")
install.packages("BiocManager")
library(ape)
library(ggtree)
BiocManager::install("ggtree")
a

getwd()
setwd("/Users/aritra_evolves/R-scripts/ENTO606")

tree <- read.tree("trees_From_step5.new")
tree
tree5 <- tree[[5]]

par(mar = c(2, 2, 1, 1))
plot(tree5, , cex = 0.6, label.offset = 1)
add.scale.bar(x= 40, y = 1, cex = 0.6)

cons_tree <- read.tree("consensus.nex")
par(mar = c(2, 2, 1, 1))
plot(cons_tree, , cex = 0.6, label.offset = 1)


