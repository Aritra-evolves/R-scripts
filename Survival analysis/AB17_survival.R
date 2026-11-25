library(survival)
attach(AB9)
S1 <- Surv(mortality_ab17$Start, mortality_ab17$Stop, mortality_ab17$Death)
summary(S1)
library(coxme)
model_coxme1 <- coxme(S1 ~ Treatment + (1 | Cage/Bee), data = mortality_ab17)
summary(model_coxme1)
model_coxme2 <- coxme(S1 ~ Treatment + (1 | Box/Bee), data = mortality_ab17)
summary(model_coxme2)
model_coxme3 <- coxme(S1 ~ Treatment + (1 | Box/Cage/Bee), data = mortality_ab17)
summary(model_coxme3)
library(car)
Anova(model_coxme1)
Anova(model_coxme2)
Anova(model_coxme3)

library(emmeans)
cage <- pairs(emmeans(model_coxme1, ~ Treatment))
box <- pairs(emmeans(model_coxme2, ~ Treatment))
box_cage <- pairs(emmeans(model_coxme3, ~ Treatment))

library(openxlsx)
write.xlsx(cage, "Cage.xlsx")
write.xlsx(box, "Box.xlsx")
write.xlsx(box_cage, "Box_Cage.xlsx")
library(survminer)
fit_1 <- survfit(S1 ~ Treatment, data = mortality_ab17)
summary(fit_1)
q <- ggsurvplot(fit_1, data = mortality_ab17, 
           ylab = ("Survival Probability"), 
           xlab = ("Days"), 
           palette = "Dark2",
           conf.int = F,
           ggtheme = theme_bw(),
           alpha = 0.5,
           legend.title = "Treatments")

fit_2 <- survfit(S1 ~ Box, data = mortality_ab17)
summary(fit_2)
p <- ggsurvplot(fit_2, data = mortality_ab17, 
                ylab = ("Survival Probability"), 
                xlab = ("Days"), 
                palette = "Dark2",
                conf.int = F,
                facet.by = c("Treatment"),
                ggtheme = theme_bw(),
                alpha = 0.5,
                legend.title = "Boxes")


