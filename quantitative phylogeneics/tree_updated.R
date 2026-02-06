library(ape)
library(ggtree)

cons_3 <- read.nexus("consensus.nex", translate = T)
par(mar = c(2, 2, 1, 1))
plot(cons_3, , cex = 0.6, label.offset = 1)
nodelabels(cons_3$node.label,
           cex = 0.4,
           frame = "n",
           adj = c(0.7, -0.5))
add.scale.bar(x= 60, y = 1, cex = 0.6)
cons_3$tip.label
readLines("TipLabelAnnotations.txt")

annot <- read.delim("TipLabelAnnotations.txt",
                    header = TRUE,
                    stringsAsFactors = FALSE)
annot
str(annot)

label_map <- setNames(annot$Full_taxon_name,
                      annot$Short_taxon_ID)

cons_3$tip.label <- label_map[cons_3$tip.label]
cons_3$tip.label

cons_3$node.label



