# Data Wrangling

# Load package ----
library(tidyverse)

# Read data ----
surveys <- read_csv("data/dhs_myanmar_tetanus.csv")

surveys

# Examine data ----
str(surveys)

View(surveys) # Press F2

# Tidy data ----
# dplyr functions ----

# Select columns ----

# By column names
select(surveys, age, education, ancplace)

# By column index number
select(surveys, 1, 3, 5)

# dplyr helper functions

# Using helper functions

# Select columns that starts with `get_help`.
select(surveys, starts_with("get_help"))

# Select columns that contains `health` in their names.
select(surveys, contains("health"))

# Combine the helper functions with other selection methods.
# Select `caseid`, `age`, and columns that starts with `get_help`.
select(surveys, caseid, age, starts_with("get_help"))

# Remove columns `record_id` and `species_id` from `surveys` dataset.
select(surveys, -age, -education, -ancplace)

### Practice ----

# Select all columns that does not start with `get_help`.
select(surveys, -starts_with("get_help"))

# Filter rows ----

# Selects rows of mothers younger than 17.
filter(surveys, age < 17)

# Selects rows of mothers residing in rural areas.
filter(surveys, residence == "Rural")

# Selects rows of mothers who completed primary school.
filter(surveys, education == "Primary")

# Combine logical comparisons

# Selects rows of mothers younger than 17 residing in rural areas.
filter(surveys, age < 17 & residence == "Rural")

# Select rows of age range 18-22.
filter(surveys, age >= 18 & age <= 22)

# Select rows with mothers younger than 17 or older than 45
filter(surveys, age < 17 | age > 45)

### Practice ----

# Select mothers residing in rural areas with highest education of primary school who gave birth at a public health facility.

filter(surveys, education == "Primary" & residence == "Rural" & ancplace == "Public health facilities")


# Pipes ----

# Select mothers younger than 17 and select columns `age`, `residence`, `employ`

# Option 1: Save the object
surveys2 <- filter(surveys, age < 17)
surveys_demo <- select(surveys2, age, residence, employ)
surveys_demo

# Option 2: Nesting functions
surveys_demo <- select(filter(surveys, age < 17), age, residence, employ)

# Option 3: Use pipes
surveys_demo <- surveys |> # and then; it plugs the output to the first position of the following row.
  filter(age < 17) |> 
  select(age, residence, employ)

surveys_demo

# For completeness, here is another commonly used pipe `%>%`.
surveys %>%
  filter(age < 17) %>%
  select(age, residence, employ)

### Practice ----

# Using pipes, select the `surveys` data to include mothers gave birth to their last child at home
# and retain only columns `age`, `ancplace`, and `tetanus_vacc`

surveys |> 
  filter(ancplace == "Home") |> 
  select(age, ancplace, tetanus_vacc)

# Create new columns ----

# Create a new column that has double the age of mothers.
# Then, select the original and the newly created column.
surveys |> 
  mutate(age2 = age * 2) |> 
  select(age, age2)

# Create a new column of age group including the following categories: 15-24, 25-29, 30-34, 34-49.

# HINT: use `case_when()` in which each age group and the corresponding logical statement are linked
# using a `~`. `.default = "Unknown"` will label the rows that do not match any of the above
# conditions as Unknown.

dat <- surveys |> 
  mutate(
    agegrp = case_when(
      age >= 15 & age <= 24 ~ "15-24",
      age >= 25 & age <= 29 ~ "25-29",
      age >= 30 & age <= 34 ~ "30-34",
      age >= 35 & age <= 49 ~ "34-49",
      .default = "Unknown"
    )
  )

# Let's check if the grouping is done correctly.
dat |> 
  select(age, agegrp) |> 
  table()

### Practice ----

# Create a new dataframe from the surveys with a new column that lumps the birth order of last child
# of 4 and above into a single group. Name the new column as `birth_order_last_child_grp`.

# Hint: Think carefully about what to use as default.

dat <- dat |> 
  mutate(
    birth_order_last_child = case_when(
      birth_order_last_child >= 4 ~ "4 and above",
      .default = as.character(birth_order_last_child)
    )
  )

dat |> 
  select(birth_order_last_child) |> 
  table()

# Aggregate ----

# Calculate the average age by `residence`.

surveys |> 
  group_by(residence) |> 
  summarize(mean_age = mean(age))

# Get average age by `residence` and `education`.

surveys |> 
  group_by(residence, education) |> 
  summarize(mean_age = mean(age))

# Aggregate using multiple summary measures in one `summarise()` call.
# Get average and minimum age by `residence` and `education`.

surveys |>
  group_by(residence, education) |>
  summarize(mean_age = mean(age),
            min_age = min(age))

## Sorting rows ----

# Sort the results by `min_age`.
surveys |>
  group_by(residence, education) |>
  summarize(mean_age = mean(age),
            min_age = min(age)) |>
  ungroup() |> 
  arrange(min_age)

# Sort the results by `min_age` in descending order.
surveys |>
  group_by(residence, education) |>
  summarize(mean_age = mean(age),
            min_age = min(age)) |>
  ungroup() |> 
  arrange(-min_age)

### Practice ----

# Find out average, minimum, and maximum of number of children (last birth order) by education. Sort
# the data by average number of children in descending order.

surveys |>
  group_by(education) |>
  summarize(mean_child = mean(birth_order_last_child),
            min_child = min(birth_order_last_child),
            max_child = max(birth_order_last_child)) |>
  ungroup() |> 
  arrange(-mean_child)

## Aggregate - count the number of rows ----

# Count the number of rows 

# Count the number of mothers by `employ`.
surveys |>
  count(employ)

# `count()` is a shorthand for `group_by()` and `summarise()`, using `n()`. (See `?n` for more details)

surveys |>
  group_by(employ) |>
  summarise(count = n())

# Sort the count in descending order.
surveys |>
  count(employ, sort = TRUE)

# Count by `residence` and `employ`.
surveys |>
  count(residence, employ)

### Practice ----

# 1.  Enumerate eligible mothers surveyed by their education attainment?
surveys |> count(education)

# 2.  Count the mothers surveys by `state_region` and their `wealth` quantile?
surveys |> count(state_region, wealth)

# 3.  What was the oldest `age` of mothers in each type of health facility where they gave birth (`ancplace`)? Sort the maximum age in descending order.
surveys |> 
  group_by(ancplace) |> 
  summarise(age_max = max(age)) |> 
  ungroup() |> 
  arrange(-age_max)
