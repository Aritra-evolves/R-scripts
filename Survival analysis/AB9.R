library(survival)
attach(AB9)
S1 <- Surv(AB9$Start, AB9$Stop, AB9$Death)
summary(S1)
library(coxme)
model_coxme1 <- coxme(S1 ~ Treatment + (1 | Cage/Bee), data = AB9)
summary(model_coxme1)
model_coxme2 <- coxme(S1 ~ Treatment + (1 | Box/Bee), data = AB9)
summary(model_coxme2)
model_coxme3 <- coxme(S1 ~ Treatment + (1 | Diet/Bee), data = AB9)
summary(model_coxme3)
model_coxme4 <- coxme(S1 ~ Treatment + (1 | Box/Cage/Bee), data = AB9)
summary(model_coxme4)
model_coxme5 <- coxme(S1 ~ Treatment + (1 | Box/Cage/Diet/Bee), data = AB9)
summary(model_coxme5)
library(car)
Anova(model_coxme1)
Anova(model_coxme2)
Anova(model_coxme3)
Anova(model_coxme4)
Anova(model_coxme5)
library(emmeans)
cage <- pairs(emmeans(model_coxme1, ~ Treatment))
box <- pairs(emmeans(model_coxme2, ~ Treatment))
diet9 <- pairs(emmeans(model_coxme3, ~ Treatment))
box_cage <- pairs(emmeans(model_coxme4, ~ Treatment))
box_cage_diet <- pairs(emmeans(model_coxme5, ~ Treatment))
library(openxlsx)
write.xlsx(cage, "Cage.xlsx")
write.xlsx(box, "Box.xlsx")
write.xlsx(diet9, "Diet.xlsx")
write.xlsx(box_cage, "Box_Cage.xlsx")
write.xlsx(box_cage_diet, "Box_Cage_Diet.xlsx")
library(survminer)
fit_1 <- survfit(S1 ~ Treatment, data = AB9)
summary(fit_1)
ggsurvplot(fit_1, data = AB9, 
           ylab = ("Survival Probability"), 
           xlab = ("Days"), 
           palette = "Dark2",
           conf.int = F,
           facet.by = c("Diet"),
           ggtheme = theme_bw(),
           legend.title = "Treatments",
           legend.lab = c("MD", "MD-Trp", "WKB8", "WKB10", "HBC2"))

)

fit_2 <- survfit(S1 ~ Diet, data = AB9)
summary(fit_2)
p <- ggsurvplot(fit_2, data = AB9, 
           ylab = ("Survival Probability"), 
           xlab = ("Days"), 
           palette = "Dark2",
           conf.int = F,
           facet.by = c("Treatment"),
           ggtheme = theme_bw(),
           legend.title = "Diet")

p$data$Treatment <- factor(
  p$data$Treatment,
  levels = c("MD", "MD-Trp", "WKB8", "WKB10", "HBC2")
)

print(p)