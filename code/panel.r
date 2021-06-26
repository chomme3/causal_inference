# Library -----------------------------------------------------------------
library(tidyverse)
library(plm)
library(haven)
library(dplyr)
library(estimatr)
library(stargazer)
library(lmtest)
library(sandwich)
library(foreign)

# Data --------------------------------------------------------------------
dat <- read_dta(
  "https://github.com/scunning1975/mixtape/raw/master/sasp_panel.dta")

dat <- na.omit(dat)

dat <- dat %>%
  arrange(id, session)

dat <- dat %>% 
  group_by(id) %>% 
  mutate(count=n())

dat <- dat %>% 
  group_by(id) %>% 
  filter(any(count==4))

dat <- make.pbalanced(dat,
                      balance.type = "shared.individuals")
  
varlist <- c("lnw", "age", "asq", "bmi", "hispanic", 
             "black", "other", "asian", "schooling", 
             "cohab", "married", "divorced","separated",
             "age_cl", "unsafe", "llength", "reg", "asq_cl",
             "appearance_cl", "provider_second", "asian_cl",
             "black_cl", "hispanic_cl", "othrace_cl",
             "hot", "massage_cl")

df2 <- dat %>%
  group_by(id) %>%
    mutate_at(varlist,
              list(demean = ~ . - mean(.)))


# Estimation --------------------------------------------------------------

formula <- as.formula("lnw ~ age+asq+bmi+hispanic+black+other+asian+
                   schooling+cohab+married+divorced+separated+
                   age_cl+unsafe+llength+reg+asq_cl+appearance_cl+
                   provider_second+asian_cl+black_cl+hispanic_cl+
                   othrace_cl+hot+massage_cl")

ols <- lm_robust(formula = formula,
                 data = df2,
                 se_type = "stata")

fe <- lm_robust(formula = formula,
                data = df2,
                fixed_effect = ~id,
                se_type = "stata")

deman <- lm_robust(lnw_demean ~ age_demean+asq_demean+bmi_demean+hispanic_demean+black_demean+other_demean+asian_demean+
                     schooling_demean+cohab_demean+married_demean+divorced_demean+separated_demean+
                     age_cl_demean+unsafe_demean+llength_demean+reg_demean+asq_cl_demean+appearance_cl_demean+
                     provider_second_demean+asian_cl_demean+black_cl_demean+hispanic_cl_demean+
                     othrace_cl_demean+hot_demean+massage_cl_demean,
                   data = df2,
                   clusters = id,
                   se_type = "stata")

summary(ols)
summary(fe)
summary(deman)

ols2 <- lm(formula = formula,
                 data = df2
          )
ols3 <- lm(formula = formula,
           data = df2
)

coeftest(ols2, vcov = vcovHC(ols2, "HC1"))

stargazer(dat)
stargazer(ols2, ols3)
