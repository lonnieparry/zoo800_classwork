######WEEK 14 HOMEWORK######
    ###Lonnie Parry###

###OBJECTIVE 1###

###Just copying this lower bit to reorient myself to homework 11 and to show part A

#this simulated data represents the relationship between critical thermal maximum values (maximum temperature a fish can tolerate before losing the ability to maintain an upright posture when temperature is increased gradually) of both male and female herring and fish length. In some species, it is reported that females have higher CTmax values overall; however, this may depend on factors such as age and reproductive status.

#Question: Does the relationship between body length and critical thermal maximum differ between male and female herring?

ctmax<-read.csv("ctmax_simulated.csv")
str(ctmax)
summary(ctmax)
ctmax$sex <- factor(ctmax$sex)
#model with interaction

###PART B###
model <- lm(ctmax ~ mass * sex, data = ctmax)
summary(model)
#model without interaction
reduced_mod <- lm(ctmax ~ mass + sex, data = ctmax)
summary(reduced_mod)

#comparing the models
anova(reduced_mod, model, test = "LRT")

AIC(reduced_mod, model)

library(ggplot2)

ggplot(ctmax, aes(x = mass, y = ctmax, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic() +
  labs(title = "CTmax vs Mass in Male and Female Herring",
       x = "Mass (g)",
       y = "Critical Thermal Maximum (°C)")

###PART C###
# LLs
ll_full    <- logLik(model)
ll_reduced <- logLik(reduced_mod)

# Negative log-likelihoods (NLL)
nll_full <- -as.numeric(ll_full)
nll_reduced<- -as.numeric(ll_reduced)

ll_full
ll_reduced
nll_full
nll_reduced

###PART D###
library(lmtest)
lrtest(reduced_mod, model)

#the LRT came back with a really large p value of .9965, so we can infer that an interaction does not improve the model. Ecologically this means that male and female herring do not differ in the slope of the relationship between CTmax and body mass. 

###OBJECTIVE 2###

###PART A###

#For my 5 models I will have...
#Full model (mass*sex)
m_full   <- lm(ctmax ~ mass * sex, data = ctmax)
#main effects (mass+sex)
m_main   <- lm(ctmax ~ mass + sex, data = ctmax)
#single variable 1 (mass)
m_mass   <- lm(ctmax ~ mass, data = ctmax)
#single variable 2 (sex)
m_sex    <- lm(ctmax ~ sex, data = ctmax)
#intercept only (ctmax-1)
m_null   <- lm(ctmax ~ 1, data = ctmax)

#creating the AIC table
model_list <- list(
  full        = m_full,
  main_effects = m_main,
  mass_only   = m_mass,
  sex_only    = m_sex,
  intercept   = m_null)

aic_table <- data.frame(
  Model = names(model_list),
  AIC   = sapply(model_list, AIC))
aic_table <- aic_table[order(aic_table$AIC), ]
View(aic_table)

###PART B###

#AIC identified the main-effects model as the best-supported by the data. This suggests that both body mass and sex influence CTmax, but the interaction is unnecessary. Ecologically, males and females differ in their average CTmax values (intercept differences), but the effect of body mass on CTmax is about the same for both sexes.
