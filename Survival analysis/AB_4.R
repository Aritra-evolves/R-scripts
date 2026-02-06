library(survival)
attach(AB4_h1)
S1 <- Surv(AB4_h1$Start, AB4_h1$Stop, AB4_h1$Death)
summary(S1)

S9 <- Surv(AB4_h9$Start, AB4_h9$Stop, AB4_h9$Death)
summary(S9)

S2 <- Surv(AB4_h2$Start, AB4_h2$Stop, AB4_h2$Death)
summary(S2)

S3 <- Surv(AB4_h3$Start, AB4_h3$Stop, AB4_h3$Death)
summary(S3)
# Fit a mixed effects Cox model
library(coxme)
model_coxme_h1 <- coxme(S1 ~ Trt1 + (1 | Cage/Bee), data = AB4_h1)
summary(model_coxme_h1)

model_coxme_h9 <- coxme(S9 ~ Trt1 + (1 | Cage/Bee), data = AB4_h9)
summary(model_coxme_h9)

model_coxme_h2 <- coxme(S2 ~ Trt1 + (1 | Cage/Bee), data = AB4_h2)
summary(model_coxme_h2)

model_coxme_h3 <- coxme(S3 ~ Trt1 + (1 | Cage/Bee), data = AB4_h3)
summary(model_coxme_h3)
# ANOVA
library(car)
Anova(model_coxme_h1)
Anova(model_coxme_h9)
Anova(model_coxme_h2)
Anova(model_coxme_h3)
# Estimated marginal means
library(emmeans)
pairs(emmeans(model_coxme_h1, ~ Trt1))
pairs(emmeans(model_coxme_h9, ~ Trt1))
pairs(emmeans(model_coxme_h2, ~ Trt1))
pairs(emmeans(model_coxme_h3, ~ Trt1))
# Plot survival curves
library(survminer)
fit_h1 <- survfit(S1 ~ Treatment, data = AB4_h1)
summary(fit_h1)
ggsurvplot(fit_h1, data = AB4_h1, 
           ylab = ("Survival Probability"), 
           xlab = ("Days"), 
           palette = "Dark2",
           conf.int = T,
           facet.by = c("Regime"),
           ggtheme = theme_bw(),
           legend.title = "Treatments")

fit_h9 <- survfit(S9 ~ Treatment, data = AB4_h9)
summary(fit_h9)
ggsurvplot(fit_h9, data = AB4_h9, 
           ylab = ("Survival Probability"), 
           xlab = ("Days"), 
           palette = "Dark2",
           conf.int = T,
           facet.by = c("Regime"),
           ggtheme = theme_bw(),
           legend.title = "Treatments")

fit_h2 <- survfit(S2 ~ Treatment, data = AB4_h2)
summary(fit_h2)
ggsurvplot(fit_h2, data = AB4_h2, 
           ylab = ("Survival Probability"), 
           xlab = ("Days"), 
           palette = "Dark2",
           conf.int = T,
           facet.by = c("Regime"),
           ggtheme = theme_bw(),
           legend.title = "Treatments")

fit_h3 <- survfit(S3 ~ Treatment, data = AB4_h3)
summary(fit_h3)
ggsurvplot(fit_h3, data = AB4_h3, 
           ylab = ("Survival Probability"), 
           xlab = ("Days"), 
           palette = "Dark2",
           conf.int = T,
           facet.by = c("Regime"),
           ggtheme = theme_bw(),
           legend.title = "Treatments")

library(ggplot2)
library(tidyverse)
df_long <- AB4 %>%
  pivot_longer(
    cols = c(`Eclosion rate (%)`, `Longevity (days)`),       # <-- replace with your actual two y-columns
    names_to = "variable",
    values_to = "value"
  )

ggplot(df_long, aes(x = Colony, y = value, fill = variable)) +  # x_var = your x column
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Grouped Bar Plot",
    x = "Category",
    y = "Value",
    fill = "Variable"
  ) +
  theme_bw()
ggplot(AB4, aes(x = Colony, y = `Eclosion rate (%)`)) +  # x_var = your x column
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Grouped Bar Plot",
    x = "Colony",
    y = "Eclosion rate (%)") +
  theme_bw()

ggplot(AB4, aes(x = Colony, y = `Longevity (days)`)) +  # x_var = your x column
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Grouped Bar Plot",
    x = "Colony",
    y = "Longevity (days)") +
  theme_bw()
