library(tidyverse)
library(broom)
library(car)
library(gtsummary)

surveys2 <- readRDS("data/surveys2.rds")

# Descriptive analysis - table 1 ----

## Table based on a few variables ----
# Let's pick residence and education and make a table using tbl_summary()

surveys2 |> 
  select(residence, education) |> 
  tbl_summary()

# Add variable label

surveys2 |> 
  select(residence, education) |> 
  tbl_summary(
    label = c(residence = "Urban or rural residence",
              education = "Highest education attainment")
  )

### Practice ----
# Create a descriptive table for birth_order_last_child_grp, employ, and ancplace
surveys2 |> 
  select(birth_order_last_child_grp, employ, ancplace) |> 
  tbl_summary(
    label = c(birth_order_last_child_grp = "Birth order of last pregnancy",
              employ = "Mother’s occupation",
              ancplace = "Type of facility where women received antenatal care")
  )

# Distribution by outcome variable (tetanus_vacc)
surveys2 |> 
  select(residence, education, tetanus_vacc) |> 
  tbl_summary(
    by = tetanus_vacc,
    label = c(residence = "Urban or rural residence",
              education = "Highest education attainment")
  ) |> 
  add_overall(last = TRUE) |> 
  add_p()

## All variables ----
names(surveys2)
# Let's remove caseid, age, birth_order_last_child from the dataframe

surveys2 |> 
  select(-c(caseid, age, birth_order_last_child)) |> 
  relocate(agegrp, .before = state_region) |> 
  relocate(birth_order_last_child_grp, .before = education) |> 
  tbl_summary()

# Save the data in an object first to reuse it later
surveys3 <- surveys2 |> 
  select(-c(caseid, age, birth_order_last_child)) |> 
  relocate(agegrp, .before = state_region) |> 
  relocate(birth_order_last_child_grp, .before = education)

### Create a publication-ready table ----
# Create a named vector for labels
var_labels <-  c(
  agegrp = "Age in years",
  birth_order_last_child_grp = "Birth order of last pregnancy",
  state_region = "State or region of residence",
  residence = "Urban or rural residence",
  education = "Highest education level of mothers",
  employ = "Mother's occupation",
  wealth = "Quintiles of mother's wealth index",
  get_help_permission = "Getting medical help for self: getting permission to go",
  person_decides_healthcare = "Person who usually decides on woman's healthcare",
  get_help_not_go_alone = "Getting medical help for self: not wanting to go alone",
  get_help_money = "Getting medical help for self: getting money needed for treatment",
  get_help_distance_health_facility = "Getting medical help for self: distance to health facility",
  ancplace = "Type of facility where women received antenatal care",
  tetanus_vacc = "Mothers vaccinated against tetanus"
)

surveys3 |> 
  tbl_summary(
    by = tetanus_vacc,
    label = var_labels
  ) |> 
  add_overall(last = TRUE) |> 
  add_p()

# Univariable analysis ----

# Outcome of interest: tetanus_vacc

## Interactive ----
# Example: agegrp
# Create a LR model based on surveys2
m1 <- glm(tetanus_vacc ~ agegrp, family = binomial, data = surveys3)
# ERROR: tetanus_vacc must be 0 or 1/ FALSE or TRUE

# Change the outcome variable accordingly
surveys4 <- surveys3 |> 
  mutate(tetanus_vacc = tetanus_vacc == "Unvaccinated")

# Create a LR model based on surveys3
m1 <- glm(tetanus_vacc ~ agegrp, family = binomial, data = surveys4)

# Examine the model
summary(m1)
tidy(m1, exponentiate = TRUE, conf.int = TRUE)
Anova(m1)

# Example: residence
m2 <- glm(tetanus_vacc ~ residence, family = binomial, data = surveys4)
tidy(m2, exponentiate = TRUE, conf.int = TRUE)
Anova(m2)

### Practice ----
# Build LR models and examine the results using education, ancplace

# education
m_education <- glm(tetanus_vacc ~ education, family = binomial, data = surveys4)
summary(m_education)
tidy(m_education, exponentiate = TRUE, conf.int = TRUE)
Anova(m_education)

# ancplace
m_ancplace <- glm(tetanus_vacc ~ ancplace, family = binomial, data = surveys4)
summary(m_ancplace)
tidy(m_ancplace, exponentiate = TRUE, conf.int = TRUE)
Anova(m_ancplace)

## Publication-ready table ----

# Independent variable: agegrp, 
surveys4 |> 
  select(agegrp, residence, tetanus_vacc) |> 
  tbl_uvregression(
    label = c(agegrp = "Age in years",
              residence = "Urban and rural residence"),
    y = tetanus_vacc,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) |> 
  add_global_p()

### Practice ----
# Build univariable LR models and examine the results based on all variables
surveys4 |> 
  tbl_uvregression(
    label = var_labels,
    y = tetanus_vacc,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) |> 
  add_global_p()

# Multivariable analysis ----

# Example: education, residence
# Build model
mv1 <- glm(tetanus_vacc ~ education + residence,
           family = binomial,
           data = surveys4)

# Extract estimates
tidy(mv1, exponentiate = TRUE, conf.int = TRUE)
# Obtain global p value
Anova(mv1)
# Create publication-ready table 
mv <- mv1 |> 
  tbl_regression(
    label = var_labels,
    exponentiate = TRUE
  ) |> 
  add_global_p()

# Combine uni- and multi-variable analysis results in one table
uv <- surveys4 |> 
  select(education, residence, tetanus_vacc) |> 
  tbl_uvregression(
    label = var_labels,
    y = tetanus_vacc,
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

### Practice ----

# 1. Using multivariable analysis to examine the following as possible
# factors associated with maternal tetanus vaccination:
# agegrp, education, employ, residence, wealth, get_help_permission, ancplace

m_mv <- glm(tetanus_vacc ~ agegrp + education + employ + residence + wealth + get_help_permission + ancplace,
          family = binomial,
          data = surveys4)

tidy(m_mv, exponentiate = TRUE, conf.int = TRUE)
Anova(m_mv)

# 2. Create a publication-ready table combining uni- and multivariable analyses
# results

mv <- m_mv |> 
  tbl_regression(
    label = var_labels,
    exponentiate = TRUE
  ) |> 
  add_global_p()

uv <- surveys4 |> 
  select(agegrp, education, employ, residence, wealth, get_help_permission,
         ancplace, tetanus_vacc) |> 
  tbl_uvregression(
    label = var_labels,
    y = tetanus_vacc,
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

# Save surveys4 as RDS so we can use it to generate a report.
saveRDS(surveys4, "data/surveys4.rds")
