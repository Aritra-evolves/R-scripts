library(survival)
attach(AB4_h1)
S1 <- Surv(AB4_h1$Start, AB4_h1$Stop, AB4_h1$Death)
summary(S1)

S2 <- Surv(AB3th$Start, AB3th$Stop, AB3th$Death)
summary(S2)

S3 <- Surv(AB3ac$Start, AB3ac$Stop, AB3ac$Death)
summary(S3)
# Fit a mixed effects Cox model
library(coxme)
model_coxme <- coxme(S1 ~ Trt1 + (1 | Cage/Bee), data = AB4_h1)
summary(model_coxme)

model_coxme_th <- coxme(S2 ~ Trt1 + (1 | Cage/Bee), data = AB3th)
summary(model_coxme_th)

model_coxme_ac <- coxme(S3 ~ Trt1 + (1 | Cage/Bee), data = AB3ac)
summary(model_coxme_ac)
# ANOVA
library(car)
Anova(model_coxme)
Anova(model_coxme_th)
Anova(model_coxme_ac)
# Estimated marginal means
library(emmeans)
pairs(emmeans(model_coxme, ~ Trt1))

pairs(emmeans(model_coxme_th, ~ Trt1))

pairs(emmeans(model_coxme_ac, ~ Trt1))
##Plot survival curves##
library(survminer)
fit_h1 <- survfit(S1 ~ Treatment, data = AB4_h1)
AB3th$Treatment <- factor(AB3th$Treatment, levels = c("Thiacloprid", "Control"))
fit_th <- survfit(S2 ~ Treatment, data = AB3th)
fit_ac <- survfit(S3 ~ Treatment, data = AB3ac)
summary(fit_h1)
summary(fit_ac)
summary(fit_th)
ggsurvplot(fit_h1, data = AB4_h1, 
           ylab = ("Survival Probability"), 
           xlab = ("Days"), 
           palette = "Dark2",
           conf.int = T,
           facet.by = c("Regime"),
           ggtheme = theme_bw(),
           legend.title = "Treatments")

ggsurvplot(fit_ac, data = AB3ac,
                 xlab = ("Days"), 
                 palette = c("Dark2"),
                facet.by = c("Regime"),
                ggtheme = theme_bw(),
                conf.int = T,
                 legend.title = "Treatments")

AB3th$Treatment <- factor(AB3th$Treatment, levels = c("Thiacloprid", "Control"))
ggsurvplot(fit_th, data = AB3th,
           xlab = ("Days"), 
           palette = c("#D95F02","#1B9E77" ),
           alpha = 1,
           facet.by = c("Regime"),
           ggtheme = theme_bw(),
           conf.int = T,
           legend.title = "Treatments")



colors <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
            "#66A61E", "#E6AB02", "#A6761D", "#666666")

barplot(rep(1, length(colors)),
        col = colors,
        border = NA,
        names.arg = colors,
        las = 2,
        main = "Dark2 Palette Colors")