library(tidyverse)
library(collapse)
library(broom)
library(gtsummary)
library(car)

dat <- read_csv("data/psz_impact.csv")

agegrp_lvl <- c(
  "25 and younger",
  "26-35",
  "36-45",
  "46 and older"
)

edu_lvl <- c(
  "No education",
  "Primary or Islamic education",
  "Secondary or matric",
  "Intermediate",
  "Bachelors",
  "Masters"
)

tx_status_lvl <- c(
  "Completed therapy",
  "Loss to follow-up",
  "Referred",
  "Refused therapy"
)

impact <- dat |> 
  rename(visit_date = DATE_ENTERED,
         age = AGE,
         education = Education) |> 
  fsubset(tx_status != "Terminated") |> 
  fsubset(gender != "Transgender") |> 
  fmutate(
    agegrp = case_when(
      age <= 25 ~ agegrp_lvl[1],
      age > 25 & age <= 35 ~ agegrp_lvl[2],
      age > 35 & age <= 45 ~ agegrp_lvl[3],
      age > 45 ~ agegrp_lvl[4],
      .default = "Unknown"
    ) |>
      factor(levels = agegrp_lvl),
    gender = factor(gender),
    factor(education, levels = edu_lvl),
    language = fct_infreq(language) |> fct_lump_min(min = 55),
    marital_status = factor(marital_status),
    tx_status = fct_infreq(tx_status)
  )

# Binary variable -----

## Univariable analysis ----

##### Interactive
# Example: agegrp
# Create a LR model based on surveys2
m1 <- glm(tx_completed ~ agegrp, family = binomial, data = impact2)
# Error in eval(family$initialize) : y values must be 0 <= y <= 1

# Change the outcome variable accordingly
# Also remove two observations of transgender
impact3 <- impact1 |> 
  mutate(tx_completed = (tx_completed == "Completed"))

# Create a LR model based on surveys3
m1 <- glm(tx_completed ~ agegrp, family = binomial, data = impact3)

# Examine the model
summary(m1)
tidy(m1, exponentiate = TRUE, conf.int = TRUE)
Anova(m1)

# Example: education
m2 <- glm(tx_completed ~ education, family = binomial, data = impact3)
tidy(m2, exponentiate = TRUE, conf.int = TRUE)
Anova(m2)

#### Practice
# Build LR models and examine the results using education, ancplace

# language
m_language <- glm(tx_completed ~ language, family = binomial, data = impact3)
summary(m_language)
tidy(m_language, exponentiate = TRUE, conf.int = TRUE)
Anova(m_language)

### Publication-ready table

# Independent variable: agegrp, gender
impact3 |> 
  select(agegrp, gender, tx_completed) |> 
  tbl_uvregression(
    y = tx_completed,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) |> 
  add_global_p()

#### Practice
# Build univariable LR models and examine the results based on all variables except phq9 and gad7 scores
impact3 |> 
  select(-c(phq9, gad7)) |> 
  tbl_uvregression(
    y = tx_completed,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) |> 
  add_global_p()

## Multivariable analysis ----

# Example: education, residence
# Build model
mv1 <- glm(tx_completed ~ agegrp + gender + education,
           family = binomial,
           data = impact3)

# Extract estimates
tidy(mv1, exponentiate = TRUE, conf.int = TRUE)
# Obtain global p value
Anova(mv1)

# Create publication-ready table 
mv <- mv1 |> 
  tbl_regression(exponentiate = TRUE) |> 
  add_global_p()

# Combine uni- and multi-variable analysis results in one table
uv <- impact3 |> 
  select(agegrp, gender, education, tx_completed) |> 
  tbl_uvregression(
    y = tx_completed,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    hide_n = TRUE
  ) |> 
  add_global_p()

umv <- tbl_merge(
  tbls = list(uv, mv),
  tab_spanner = c("Univariable analysis", "Multivariable analysis")
)

umv


# Continuous variable ----

## Univariable analysis ----

## Multivariable analysis ----


# Save impact3 as RDS so we can use it to generate a report.
saveRDS(impact3, "data/impact3.rds")