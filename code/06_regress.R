library(tidyverse)
library(collapse)
library(broom)
library(gtsummary)
library(car)
library(janitor)

impact <- readRDS("data/impact_binary.rds")

# Binary variable -----

## Univariable analysis ----

##### Interactive
# Example: agegrp
# Let's look at the distribution first
impact |> 
  select(agegrp, tx_completed) |> 
  descr(by = ~tx_completed)

# Create a LR model based on surveys2
m1 <- glm(tx_completed ~ agegrp, family = binomial, data = impact)
# Error in eval(family$initialize) : y values must be 0 <= y <= 1

# Change the outcome variable accordingly
# Also remove two observations of transgender
impact1 <- impact |> 
  mutate(tx_completed = as.integer(tx_completed == "Completed"))

# Create a LR model
m1 <- glm(tx_completed ~ agegrp, family = binomial, data = impact1)

# Examine the model
summary(m1)
tidy(m1, exponentiate = TRUE, conf.int = TRUE)
Anova(m1)

# Example: education
impact |> 
  group_by(tx_completed) |> 
  select(education) |> 
  descr()

m2 <- glm(tx_completed ~ education, family = binomial, data = impact1)
tidy(m2, exponentiate = TRUE, conf.int = TRUE)
Anova(m2)

#### Practice
# Build LR models and examine the results using language

# language
impact |> 
  select(language, tx_completed) |> 
  descr(by = ~tx_completed)

m_language <- glm(tx_completed ~ language, family = binomial, data = impact1)
tidy(m_language, exponentiate = TRUE, conf.int = TRUE)
Anova(m_language)

### Publication-ready table

# Independent variable: agegrp, gender
impact1 |> 
  select(agegrp, gender, tx_completed) |> 
  tbl_uvregression(
    y = tx_completed,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) |> 
  add_global_p()

#### Practice
# Build univariable LR models to assess factors associated with completing the therapy
# Explanatory variables: age, gender, educaiton, marital status, phq9
impact1 |> 
  select(age, gender, education, phq9, tx_completed) |> 
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
mv1 <- glm(tx_completed ~ agegrp + gender + education + phq9,
           family = binomial,
           data = impact1)

# Extract estimates
tidy(mv1, exponentiate = TRUE, conf.int = TRUE)
# Obtain global p value
Anova(mv1)

# Create publication-ready table 
mv1 |> 
  tbl_regression(exponentiate = TRUE) |> 
  add_global_p()

# Combine uni- and multi-variable analysis results in one table
uv <- impact1 |> 
  select(agegrp, gender, education, tx_completed) |> 
  tbl_uvregression(
    y = tx_completed,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    hide_n = TRUE
  ) |> 
  add_global_p()

mv <- mv1 |> 
  tbl_regression(exponentiate = TRUE) |> 
  add_global_p()

tbl_merge(
  tbls = list(uv, mv),
  tab_spanner = c("Univariable analysis", "Multivariable analysis")
)

# Continuous variable ----

## Univariable analysis ----
# Age
ggplot(impact1, aes(x = age, y = phq9)) +
  geom_point() +
  geom_smooth(method = "lm")

m_age <- lm(phq9 ~ age, data = impact1)
tidy(m_age)

# Gender
ggplot(impact1, aes(x = gender, y = phq9)) +
  geom_jitter(alpha = 0.2) +
  geom_boxplot(fill = "transparent")

m_gender <- lm(phq9 ~ gender, data = impact1)
tidy(m_gender)

# Education
ggplot(impact1, aes(x = education, y = phq9)) +
  geom_jitter(alpha = 0.2) +
  geom_boxplot(fill = "transparent")

m_edu <- lm(phq9 ~ education, data = impact1)
tidy(m_edu)

# Language
ggplot(impact1, aes(x = language, y = phq9)) +
  geom_jitter(alpha = 0.2) +
  geom_boxplot(fill = "transparent")

m_lan <- lm(phq9 ~ language, data = impact1)
tidy(m_lan)

## Multivariable analysis ----

# Plot the data
mv1 <- lm(phq9 ~ age + gender + education + language, data = impact1)
tidy(mv1)

# Check the residuals
augment(mv1) |> 
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(shape = 21, alpha = 0.8) +
  geom_smooth(method = "lm")

## Publication ready tables ----
# Univariable analysis 
uv <- impact1 |> 
  select(age, gender, education, language, tx_completed) |> 
  tbl_uvregression(
    y = tx_completed,
    method = lm,
    hide_n = TRUE,
    estimate_fun = \(x) style_number(x, digits = 4)
  ) |> 
  add_global_p()

# Multivariable analysis
mv1 <- lm(phq9 ~ age + gender + education + language, data = impact1)
mv <- mv1 |> 
  tbl_regression(
    estimate_fun = \(x) style_number(x, digits = 4)
  ) |> 
  add_global_p()

# Combine uni- and multi-variable analysis
tbl_merge(
  tbls = list(uv, mv),
  tab_spanner = c("Univariable analysis", "Multivariable analysis")
)
