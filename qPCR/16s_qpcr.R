#reordering the label names (arranging the treatments in proper order)
AB5EM_hg$Treatment <- factor(AB5EM_hg$Treatment,
                           levels = c("MD", "MDtrp", "WKB8", "WKB10", "HBC2"))

AB5EM_mg$Treatment <- factor(AB5EM_mg$Treatment,
                             levels = c("MD", "MDtrp", "WKB8", "WKB10", "HBC2"))

#plotting the data: hind gut
## y intercept should be calculated in a similar way like the samples with the most diluted standards.
hg_qPCR <- ggplot(data = AB5EM_hg, aes(x = Treatment, y = qPCR, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.5)) +
  geom_point(aes(fill = Treatment),
             shape = 21,
             size = 2,
             stroke = 0.5,
             color = "black",
             position = position_jitterdodge()) +
  geom_hline(yintercept = 9083,
             linetype = "dashed",
             color = "maroon",
             linewidth = 0.6) +
  theme_bw() +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(trans = "log10") +
  annotation_logticks(base = 10, side = "l") +
  labs(x = "Groups",
       y = "16S rRNA amplicon reads - qPCR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+
  ggtitle("DNA-hindguts")

hg_qPCR


#plotting the data: midgut
mg_qPCR <- ggplot(data = AB5EM_mg, aes(x = Treatment, y = qPCR, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.5)) +
  geom_point(aes(fill = Treatment),
             shape = 21,
             size = 2,
             stroke = 0.5,
             color = "black",
             position = position_jitterdodge()) +
  geom_hline(yintercept = 53240,
             linetype = "dashed",
             color = "maroon",
             linewidth = 0.6) +
  theme_bw() +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(trans = "log10") +
  annotation_logticks(base = 10, side = "l") +
  labs(x = "Groups",
       y = "16S rRNA amplicon reads - qPCR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+
  ggtitle("DNA-midguts")

mg_qPCR




