##########ZOO800 HMWK 12##########
    ######LONNIE PARRY######


#####OBJECTIVE 1 PART A#####

# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

bassdata <- read.csv("bsb_tagging_data.csv")

# Clean dates
bassdata <- bassdata %>%
  mutate(
    Date_at_capture   = mdy(Date_at_capture),
    Date_at_recapture = mdy(Date_at_recapture)
  )

# Filter females captured AND recaptured after July
post_spawning <- bassdata %>%
  filter(
    Sex_at_capture == "F",
    month(Date_at_recapture) >= 8
  )

# Count which ones changed sex 
post_spawning <- post_spawning %>%
  mutate(
    changed_sex = ifelse(Sex_at_recapture %in% c("M", "intersex"), 1, 0)
  )

# Counts
successes <- sum(post_spawning$changed_sex == 1, na.rm = TRUE)
failures  <- sum(post_spawning$changed_sex == 0, na.rm = TRUE)

successes; failures


# successes = number of females that changed sex ("M" or "intersex")
# failures  = number of females that remained "F"

alpha <- successes + 1
beta  <- failures + 1

p <- seq(0, 1, length.out = 500)   
pdf_vals <- dbeta(p, alpha, beta)

#Lets plot this bad boy
plot(
  p, pdf_vals,
  type = "l",
  lwd = 2,
  xlab = "Proportion That Individual Changed Sex",
  ylab = "Probability Density",
  main = "Beta Distribution for Sex Change Probability"
)


#####OBJECTIVE 1 PART B- 95% CI#####

alpha <- successes + 1
beta  <- failures + 1

CI_lower <- qbeta(0.025, alpha, beta)
CI_upper <- qbeta(0.975, alpha, beta)

c(CI_lower, CI_upper)

#We are 95% confident that the true sex-change probability lies between 3.76% and 26.5%


#####OBJECTIVE 2 PART A#####

#we can you the glm() function here bc the outcome is binary, so a logistic regression is appropriate

#picking out females at capture after July

glmmodel <- glm(changed_sex ~ Length_at_capture,
             data = post_spawning,
             family = binomial)
summary(glmmodel)

#Coefficients:  Estimate   Std. Error z value  Pr(>|z|)  
#(Intercept)    -28.50486   13.20595  -2.158   0.0309 *
#Length_at_capture 0.08532    0.04188   2.037   0.0416 

#Because p<0.05 for Length_at_capture, we can conclude that there is a statistically significant relationship between length at capture and the probability of sex change having occured in female bass captured after spawning

#####OBJECTIVE 2 PART B#####

summary(glmmodel)

#Coefficients:  Estimate   Std. Error z value  Pr(>|z|)  
#(Intercept)    -28.50486   13.20595  -2.158   0.0309 *
#Length_at_capture 0.08532    0.04188   2.037   0.0416 

#For every 1 mm increase in length, the log odds of sex change increase by 0.0853.

#####OBJECTIVE 2 PART C#####

# Create a sequence of lengths to predict over
partc <- data.frame(
  Length_at_capture = seq(min(post_spawning$Length_at_capture, na.rm = TRUE),
                          max(post_spawning$Length_at_capture, na.rm = TRUE),
                          length.out = 200)
)

# Get predicted probabilities + 95% CI
pred <- predict(glmmodel, newdata = partc, type = "link", se.fit = TRUE)

partc$fit  <- plogis(pred$fit)                             # predicted prob
partc$lwr  <- plogis(pred$fit - 1.96 * pred$se.fit)        # lower CI
partc$upr  <- plogis(pred$fit + 1.96 * pred$se.fit)        # upper CI

# Plot
ggplot() +
  # points for actual data
  geom_jitter(data = post_spawning,
              aes(x = Length_at_capture, y = changed_sex),
              height = 0.05, width = 0, alpha = 0.6) +
  # predicted probability curve
  geom_line(data = partc,
            aes(x = Length_at_capture, y = fit),
            color = "purple", size = 1.2) +
  # confidence ribbon
  geom_ribbon(data = partc,
              aes(x = Length_at_capture, ymin = lwr, ymax = upr),
              fill = "skyblue", alpha = 0.2) +
  labs(
    x = "Length at Capture (mm)",
    y = "Probability of Sex Change",
    title = "Predicted Probability of Sex Change vs. Length in Female Black Sea Bass Post Spawning"
  ) +
  theme_bw(base_size = 14)

###FIGURE CAPTION###
#Relationship between body length and the probability of sex change for female black sea bass recaptured after spawning season. Points show individual observations. The blue line represents the predicted probability from a logistic regression model, with the shaded region showing the 95% confidence interval

