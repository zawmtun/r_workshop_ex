library(tidyverse)
library(car)
library(collapse)
library(broom)
library(gtsummary)
library(googlesheets4)

dat <- read_sheet(Sys.getenv("WORKSHOP_GSHEET_DATA"))

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

# Glimpse variables
glimpse(impact)

# Describe summary statistics for all variables
descr(impact)

# Describe summary statistics for selected variables
impact |> 
  select(age, education, tx_status) |> 
  descr()

# Describe selected variables by group
impact |> 
  select(age, gender, education, tx_status) |> 
  group_by(gender) |> 
  descr()

# By input
impact |> 
  select(age, gender, education, tx_status) |> 
  descr(by = ~gender)

# Sort the table by value, default is by frequency
impact |> 
  select(agegrp) |> 
  descr()

impact |> 
  select(agegrp) |> 
  descr(sort.table = "value")


# Publication-ready tables: Summary statistics

## Table based on a few variables ----
# Let's pick gender and education and make a table using tbl_summary()

impact |> 
  select(gender, education) |> 
  tbl_summary()

# Add variable label

vlabels(impact$visit_date) <- "Clinic visit date"
vlabels(impact$patient_id) <- "Patient identifier"
vlabels(impact$age) <- "Participant’s age in years"
vlabels(impact$agegrp) <- "Participant’s age in years"
vlabels(impact$gender) <- "Participant’s gender"
vlabels(impact$education) <- "Participant’s highest educational attainment"
vlabels(impact$language) <- "Participant’s language"
vlabels(impact$marital_status) <- "Participant’s marital status"
vlabels(impact$phq9) <- "Baseline total PHQ-9 score"
vlabels(impact$gad7) <- "Baseline total GAD-7 score"
vlabels(impact$visit_count) <- "Number of interactions with a counselor"
vlabels(impact$tx_status) <- "Current treatment status of participant"

impact |> 
  select(gender, education) |> 
  tbl_summary()

### Practice ----
# Create a descriptive table for age, gender, language
impact |> 
  select(age, gender, language) |> 
  tbl_summary()

# Define binary variable ----
impact1 <- impact |> 
  mutate(
    completed_therapy = case_when(tx_status == "Completed therapy" ~ "Yes",
                                  .default = "No")
  )

vlabels(impact1$completed_therapy) <- "Participant completed therapy"

# Distribution by a grouping variable (tetanus_vacc)
impact1 |> 
  select(gender, education, completed_therapy) |> 
  tbl_summary(by = completed_therapy) |> 
  add_overall(last = TRUE) |> 
  add_p()

## All variables ----
names(impact)
# Let's remove visit_date, patient_id, visit_count, age, completed_therapy from the dataframe
# Save the data in an object first to reuse it later
impact2 <- impact1 |> 
  select(-c(visit_date, patient_id, visit_count, age))

impact2 |> 
  relocate(agegrp, .before = gender) |> 
  tbl_summary()

### Create a publication-ready table ----
impact2 |> 
  tbl_summary(by = completed_therapy) |> 
  add_overall() |> 
  add_p() |> 
  modify_spanning_header(all_stat_cols(stat_0 = FALSE) ~ "**Completed therapy**")

# Univariable analysis ----

# Outcome of interest: completed_therapy

## Interactive ----
# Example: agegrp
# Create a LR model based on surveys2
m1 <- glm(completed_therapy ~ agegrp, family = binomial, data = impact2)
# Error in eval(family$initialize) : y values must be 0 <= y <= 1

# Change the outcome variable accordingly
# Also remove two observations of transgender
impact3 <- impact2 |> 
  mutate(completed_therapy = (completed_therapy == "Yes"))

# Create a LR model based on surveys3
m1 <- glm(completed_therapy ~ agegrp, family = binomial, data = impact3)

# Examine the model
summary(m1)
tidy(m1, exponentiate = TRUE, conf.int = TRUE)
Anova(m1)

# Example: education
m2 <- glm(completed_therapy ~ education, family = binomial, data = impact3)
tidy(m2, exponentiate = TRUE, conf.int = TRUE)
Anova(m2)

### Practice ----
# Build LR models and examine the results using education, ancplace

# language
m_language <- glm(completed_therapy ~ language, family = binomial, data = impact3)
summary(m_language)
tidy(m_language, exponentiate = TRUE, conf.int = TRUE)
Anova(m_language)

## Publication-ready table ----

# Independent variable: agegrp, gender
impact3 |> 
  select(agegrp, gender, completed_therapy) |> 
  tbl_uvregression(
    y = completed_therapy,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) |> 
  add_global_p()

### Practice ----
# Build univariable LR models and examine the results based on all variables except phq9 and gad7 scores
impact3 |> 
  select(-c(phq9, gad7)) |> 
  tbl_uvregression(
    y = completed_therapy,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) |> 
  add_global_p()

# Multivariable analysis ----

# Example: education, residence
# Build model
mv1 <- glm(completed_therapy ~ agegrp + gender + education,
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
  select(agegrp, gender, education, completed_therapy) |> 
  tbl_uvregression(
    y = completed_therapy,
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
saveRDS(impact3, "data/impact3.rds")