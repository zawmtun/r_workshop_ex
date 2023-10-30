# Quarto

# - YAML
# - How to add code chunk
# - How to run code chunk
# - Common code chunk
# - How to add in-line code chunk
# - Visual and source editors and their relationship
# - Resources for learning markdown syntax  https://stackedit.io/app#

# Factors ----

# Load package
library(tidyverse)

# Read data
surveys_new <- read_csv("data/surveys_new.csv")

# Convert a character vector to a factor
class(surveys_new$residence)
surveys_new |> count(residence)

# Levels sort by alphabetical order
surveys2 <- surveys_new |> 
  mutate(residence = factor(residence))

surveys2 |> count(residence)

# Factors are integers with labels
class(surveys2$residence)
typeof(surveys2$residence)

# Base R approach
surveys3 <- surveys_new
surveys3 |> count(residence)
surveys3$residence <- factor(surveys3$residence)
surveys3 |> count(residence)

## Practice ----

# Change the columns for age group and birth order of the last child in the surveys data frame into a factor.

surveys2 <- surveys2 |> 
  mutate(
    agegrp = factor(agegrp),
    birth_order_last_child_grp = factor(birth_order_last_child_grp)
  )

# Verify factor conversion was successful
class(surveys2$agegrp)
class(surveys2$birth_order_last_child)

## Converting factors to character vectors
sex <- factor(c("male", "female", "female", "male"))
sex
class(sex)
sex <- as.character(sex)
class(sex)

year_fct <- factor(c(1990, 1983, 1977, 1998, 1990))
year_fct

# Let's say we want to convert `year_fct` to numeric.

as.numeric(year_fct) # Wrong!

as.numeric(as.character(year_fct)) # Works...

## Renaming factor levels

surveys2 |> count(person_decides_healthcare)
class(surveys2$person_decides_healthcare)
# Base R plot cannot plot character vectors directly
plot(surveys2$person_decides_healthcare)
plot(factor(surveys2$person_decides_healthcare))

# How to show the missing values as a category?
surveys2 <- surveys2 |> 
  mutate(person_decides_healthcare = fct_explicit_na(person_decides_healthcare))

levels(surveys2$person_decides_healthcare)
plot(surveys2$person_decides_healthcare)

## Specify the order of levels ----
# Vector example
sex <- factor(c("male", "female", "female", "male"))
sex 
levels(sex)
nlevels(sex)

# If we want male to be the first level, use factor.
sex <- factor(sex, levels = c("male", "female"))
levels(sex)

# count() mothers by education categories
surveys2 |> count(education)

# The categories are not in order. Need to manually specify the order.
surveys2 <- surveys2 |> 
  mutate(
    education = factor(education,
                       levels = c("No education", "Primary", "Secondary", "More than secondary"))
  )

surveys2 |> count(education)

## Practice ----

# 1. The categories of employ, wealth, and ancplace in surveys dataset are not in order. Fix them by
# using factors.

surveys2 |> count(employ)
surveys2 |> count(wealth)
surveys2 |> count(ancplace)

employ_levels <- c(
  "Agricultural",
  "Manual",
  "Sales",
  "Not working",
  "Other"
)

wealth_levels <- c(
  "Lowest",
  "Second",
  "Third",
  "Fourth",
  "Highest"
)

ancplace_levels <- c(
  "Public health facilities",
  "Private health facilities or NGO clinics",
  "Mixed",
  "Home",
  "None"
)

surveys2 <- surveys2 |> 
  mutate(
    employ = factor(employ, levels = employ_levels),
    wealth = factor(wealth, levels = wealth_levels),
    ancplace = factor(ancplace, levels = ancplace_levels)
  )

# 2. Four columns with names starting with `get_help` consist of two categories:
# 1) Big problem; 2) Not a big problem, in that order as it is a character
# column. Reverse the order of these two categories using factors.

surveys2 |> count(get_help_permission)
surveys2 |> count(get_help_not_go_alone)
surveys2 |> count(get_help_money)
surveys2 |> count(get_help_distance_health_facility)

problem_levels <- c("Not a big problem", "Big problem")

surveys2 <- surveys2 |> 
  mutate(
    get_help_permission = factor(get_help_permission, levels = problem_levels),
    get_help_not_go_alone = factor(get_help_not_go_alone, levels = problem_levels),
    get_help_money = factor(get_help_money, levels = problem_levels),
    get_help_distance_health_facility = factor(get_help_distance_health_facility, levels = problem_levels),
  )

# Save data in R Format (RDS)

saveRDS(surveys2, "data/surveys2.rds")

# Working with dates ----
library(lubridate)

# `paste()` to put together a string

d <- 19
m <- 8
y <- 2022

dd_dmy_str <- paste(d, m, y)
dd_dmy_str <- paste(d, m, y, sep = "-")
dd_dmy_str
class(dd_dmy_str)
dd_dmy_date <- lubridate::dmy(dd_dmy)
dd_dmy_date
class(dd_dmy_date)

dd_ymd_str <- paste(y, m, d, sep = "-")
dd_ymd_str
class(dd_ymd_str)
dd_ymd_date <- ymd(dd_ymd_str)
class(dd_ymd_date)

temp <- tibble(
  id = LETTERS[1:5],
  day = c(3, 6, 2, 23, 20),
  mth = c(10, 2, 12, 4, 7),
  year = c(1999, 2000, 2001, 1998, 1999)
)

temp1 <- temp |> 
  mutate(dd_str = paste(year, mth, day, sep = "-"),
         dd_date = ymd(dd_str))

temp1

temp1 <- temp |> 
  mutate(
    dd = ymd(paste(year, mth, day, sep = "-"))
  )

temp1


