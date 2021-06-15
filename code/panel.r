library(tidyverse)
library(plm)
library(haven)

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
  
varlist <- list("lnw", "age")

dat <- dat %>% mutate (
  demean_lnw = lnw - mean(lnw)
)