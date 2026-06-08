library(survival)
attach(AB8)
S1 <- Surv(AB8$Start, AB8$Stop, AB8$Death)
summary(S1)

library(coxme)
model_coxme1 <- coxme(S1 ~ Treatment + (1 | Cage/Individual), data = AB8)
summary(model_coxme1)
model_coxme2 <- coxme(S1 ~ Treatment + (1 | Box/Individual), data = AB8)
summary(model_coxme2)
model_coxme3 <- coxme(S1 ~ Treatment + (1 | Cage/Box/Individual), data = AB8)
summary(model_coxme3)

library(car)
Anova(model_coxme1)
Anova(model_coxme2)
Anova(model_coxme3)

library(emmeans)
cage <- pairs(emmeans(model_coxme1, ~ Treatment))
box <- pairs(emmeans(model_coxme2, ~ Treatment))
cage_box <- pairs(emmeans(model_coxme3, ~ Treatment))

library(broom)
library(openxlsx)
cage_8 <- tidy(cage)
box_8 <- tidy(box)
cb_8 <- tidy(cage_box)

write.xlsx(cage_8, "Cage.xlsx")
write.xlsx(box_8, "Box.xlsx")
write.xlsx(cb_8, "Cage+Box.xlsx")

library(survminer)
fit_1 <- survfit(S1 ~ Treatment, data = AB8)
summary(fit_1)
ggsurvplot(fit_1, data = AB8, 
           ylab = ("Survival Probability"), 
           xlab = ("Days"), 
           palette = "Dark2",
           conf.int = F,
           ggtheme = theme_bw(),
           legend.title = "Treatments")
