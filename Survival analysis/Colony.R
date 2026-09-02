
kruskal.test(Log_CFU ~ Treatment, data = ab10)
kruskal.test(Log_CFU ~ Diet, data = wkb8)
kruskal.test(Log_CFU ~ Diet, data = wkb10)

library(ggplot2)
colony <- ggplot(ab10, aes(Treatment, Log_CFU)) +
  geom_boxplot() + geom_jitter(aes(color = Diet), width = 0.15, size = 2, alpha = 0.7) + scale_color_manual(values = c("Pollen" = "#1B9E77",
                                                                                                                                     "No_pollen" = "#D95F02")) +
  scale_fill_manual(values = c("Pollen" = "#1B9E77",
                               "No_pollen" = "#D95F02")) + theme_bw()

