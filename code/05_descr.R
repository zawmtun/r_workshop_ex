library(tidyverse)
library(collapse)
library(gtsummary)

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

# Glimpse variables
glimpse(impact)

# Describe summary statistics for all variables
descr(impact)

# Describe summary statistics for selected variables
impact |> 
  select(age, education, tx_status) |> 
  descr()

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

impact <- impact |> 
  mutate(
    visit_date = setLabels(visit_date, "Clinic visit date"),
    patient_id = setLabels(patient_id, "Patient identifier"),
    age = setLabels(age, "Participant’s age in years"),
    agegrp = setLabels(agegrp, "Participant’s age in years"),
    gender = setLabels(gender, "Participant’s gender"),
    education = setLabels(education, "Participant’s highest educational attainme"),
    language = setLabels(language, "Participant’s language"),
    marital_status = setLabels(marital_status, "Participant’s marital status"),
    phq9 = setLabels(phq9, "Baseline total PHQ-9 score"),
    gad7 = setLabels(gad7, "Baseline total GAD-7 score"),
    visit_count = setLabels(visit_count, "Number of interactions with a counselor"),
    tx_status = setLabels(tx_status, "Current treatment status of participant")
  )

impact |> 
  select(gender, education) |> 
  tbl_summary()

### Practice ----
# Create a descriptive table for age, gender, language
impact |> 
  select(age, gender, language) |> 
  tbl_summary()

# Describe selected variables by group ----

# Define a binary variable
impact1 <- impact |> 
  mutate(
    tx_completed = case_when(tx_status == "Completed therapy" ~ "Completed",
                             .default = "Not completed"),
    tx_completed = setLabels(tx_completed, "Whether or not participant completed therapy")
  )

saveRDS(impact1, "data/impact_binary.rds")

# Describe variables by the binary variable
# Using group_by()
impact1 |> 
  select(age, gender, education, tx_completed) |> 
  group_by(tx_completed) |> 
  descr()

# Using `by` input from descr()
impact1 |> 
  select(age, gender, education, tx_completed) |> 
  descr(by = ~tx_completed)

# T-test
t.test(age ~ tx_completed, data = impact1)

# Chi-square test
impact1 |> 
  select(gender, tx_completed) |> 
  table() |> 
  chisq.test()

impact1 |> 
  select(education, tx_completed) |> 
  table() |> 
  chisq.test()

# Distribution by a grouping variable (tx_completed)
impact1 |> 
  select(age, gender, education, tx_completed) |> 
  tbl_summary(by = tx_completed) |> 
  add_overall(last = TRUE) |> 
  add_p()

## All variables ----
names(impact1)
# Let's remove visit_date, patient_id, visit_count, age, tx_completed from the dataframe
# Save the data in an object first to reuse it later
impact2 <- impact1 |> 
  select(-c(visit_date, patient_id, visit_count, age, tx_completed))

impact2 |> 
  relocate(agegrp, .before = gender) |> 
  tbl_summary()

### Create a publication-ready table ----
impact2 |> 
  tbl_summary(by = tx_completed) |> 
  add_overall() |> 
  add_p() |> 
  modify_spanning_header(all_stat_cols(stat_0 = FALSE) ~ "**Completed therapy**")

