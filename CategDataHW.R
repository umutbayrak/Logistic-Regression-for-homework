migraine <- read.table("migraine.txt")
summary(migraine)
str(migraine)


# Question 2 - a.

# First compare the 100 and placebo at 5% significance level

placebo_and_hundred <- migraine[migraine$trt != 50, ]

# For gender and bassev variables are factorised.

fGender_hundred <- factor(placebo_and_hundred$gender)
fBassev_hundred <- factor(placebo_and_hundred$bassev)
fTreatment_hundred <- factor(placebo_and_hundred$trt)

# Logistic regression model for Placebo

model_pla_hun <- glm(relief ~ age + fGender_hundred + fBassev_hundred + fTreatment_hundred, family=binomial, 
                    data = placebo_and_hundred)

summary(model_pla_hun, correlation = TRUE)

# To assess the importance of the main effects of fGender and fBassev within this model 
# we can use the anova function

anova(model_pla_hun, test = "Chisq")


################################################################################
# When the placebo is taken as a reference, 
# STOP-HEADACHE 100 is significant with p-value of < 2e-16 at 5% significant level
# Age is also significant with p-value of 2.36e-06 at 5% significant level
# Correlation is really weak between predictors so no multicollinearity. 
# Coef. of treatment 100 is 2.282792, coef. age is -0.023531
# Anova table shows that Gender and Bassev factors are not that significant as main effects
################################################################################

# Placebo vs. STOP-HEADACHE 50 at 5% significance level

placebo_and_fifty <- migraine[migraine$trt != 100, ]

fGender_fifty <- factor(placebo_and_fifty$gender)
fBassev_fifty <- factor(placebo_and_fifty$bassev)
fTreatment_fifty <- factor(placebo_and_fifty$trt)

model_pla_fif <- glm(relief ~ age + fGender_fifty + fBassev_fifty + fTreatment_fifty, 
                     family=binomial, 
                     data = placebo_and_fifty)

summary(model_pla_fif, correlation = TRUE)

anova(model_pla_fif, test = "Chisq")

################################################################################
# When the placebo is taken as a reference, 
# STOP-HEADACHE 50 is significant with p-value of < 2e-16 at 5% significant level
# Age is also significant with p-value of 2.73e-05 at 5% significant level
# Correlation is really weak between predictors so no multicollinearity. 
# Coef. of treatment 50 is 1.695247, coef. age is -0.021844
# Anova table shows that Gender and Bassev factors are not that significant as main effects
################################################################################

# STOP-HEADACHE 100 vs. STOP-HEADACHE 50 at 5% significance level

hundred_and_fifty <- migraine[migraine$trt != 0, ]

fGender_treat <- factor(hundred_and_fifty$gender)
fBassev_treat <- factor(hundred_and_fifty$bassev)
fTreatment_treat <- factor(hundred_and_fifty$trt)

model_treatment <- glm(relief ~ age + fGender_treat + fBassev_treat + 
                       fTreatment_treat, family=binomial, 
                       data = hundred_and_fifty)

summary(model_treatment, correlation = TRUE)

anova(model_treatment, test = "Chisq")

################################################################################
# When the STOP-HEADACHE 50 mg is taken as a reference, 
# STOP-HEADACHE 100 is significant with p-value of 0.000159 at 5% significant level
# Age is also significant with p-value of 8.24e-08 at 5% significant level
# Correlation is really weak between predictors so no multicollinearity. 
# Coef. of treatment 100 is 0.564408, coef. age is -0.022396
# Anova table shows that Gender is not that significant, but fBassev becomes significant
################################################################################

# Question 2 - b. 

# Adding 3-different interaction terms one-by-one and respectively to each model

# STOP-HEADACHE 100 and placebo

  # Treatment by baseline severity

fit1 <- glm(relief ~ age + fGender_hundred + fBassev_hundred + 
                           fTreatment_hundred + 
                           fTreatment_hundred:fBassev_hundred, 
                           family=binomial, data = placebo_and_hundred)
summary(fit1)

  # Treatment by gender

fit2 <- glm(relief ~ age + fGender_hundred + fBassev_hundred + 
              fTreatment_hundred + 
              fTreatment_hundred:fGender_hundred, 
              family=binomial, data = placebo_and_hundred)

summary(fit2)

  # Treatment by age

fit3 <- glm(relief ~ age + fGender_hundred + fBassev_hundred + 
              fTreatment_hundred + 
              fTreatment_hundred:age, 
              family=binomial, data = placebo_and_hundred)

summary(fit3)

# Placebo vs. STOP-HEADACHE 50

  # Treatment by baseline severity

fit4 <- glm(relief ~ age + fGender_fifty + fBassev_fifty + fTreatment_fifty +
                     fTreatment_fifty:fBassev_fifty,
                     family=binomial, 
                     data = placebo_and_fifty)

summary(fit4)

  # Treatment by gender

fit5 <- glm(relief ~ age + fGender_fifty + fBassev_fifty + fTreatment_fifty +
                     fTreatment_fifty:fGender_fifty,
                     family=binomial, 
                     data = placebo_and_fifty)

summary(fit5)

  # Treatment by age

fit6 <- glm(relief ~ age + fGender_fifty + fBassev_fifty + fTreatment_fifty +
                     fTreatment_fifty:age,
                     family=binomial, 
                     data = placebo_and_fifty)

summary(fit6)


# STOP-HEADACHE 50 vs. STOP-HEADACHE 100

  # Treatment by baseline severity

fit7 <- glm(relief ~ age + fGender_treat + fBassev_treat + fTreatment_treat + 
                     fTreatment_treat:fBassev_treat,
                     family=binomial, 
                     data = hundred_and_fifty)

summary(fit7)

  # Treatment by gender

fit8 <- glm(relief ~ age + fGender_treat + fBassev_treat + fTreatment_treat + 
                     fTreatment_treat:fGender_treat,
                     family=binomial, 
                     data = hundred_and_fifty)

summary(fit8)

  # Treatment by age

fit9 <- glm(relief ~ age + fGender_treat + fBassev_treat + fTreatment_treat + 
                     fTreatment_treat:age,
                     family=binomial, 
                     data = hundred_and_fifty)

summary(fit9)