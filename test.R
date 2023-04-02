library(tidyverse)
library(broom)
library(gtsummary)

tt <- read_csv("dhs_myanmar_tetanus.csv")


tt1 <- tt |> 
  mutate(
    agegrp = case_when(
      age >= 15 & age <= 24 ~ "15-24",
      age >= 25 & age <= 29 ~ "25-29",
      age >= 30 & age <= 34 ~ "30-34",
      age >= 35 & age <= 49 ~ "34-49",
      .default = "Unknown"
    ),
    birth_order_last_child_grp = case_when(
      birth_order_last_child >= 4 ~ "4 and higher",
      .default = as.character(birth_order_last_child)
    )
  )

tt2 <- tt1 |> 
  select(agegrp, birth_order_last_child_grp, state_region, residence,
        education, employ, wealth, starts_with("get_"), person_decides_healthcare,
        ancplace, tetanus_vacc)

tbl_summary(
  tt2,
  label = list(agegrp ~ "Age in years",
               birth_order_last_child_grp ~ "Birth order of last pregnancy",
               state_region ~ "State or region of residence",
               residence ~ "Urban or rural residence",
               education ~ "Highest education level of mothers",
               employ ~ "Mother's occupation",
               wealth ~ "Quintiles of mother's wealth index",
               get_help_permission ~ "Getting medical help for self: getting permission to go",
               person_decides_healthcare ~ "Person who usually decides on woman's healthcare",
               get_help_not_go_alone ~ "Getting medical help for self: not wanting to go alone",
               get_help_money ~ "Getting medical help for self: getting money needed for treatment",
               get_help_distance_health_facility ~ "Getting medical help for self: distance to health facility",
               ancplace ~ "Type of facility where women received antenatal care",
               tetanus_vacc ~ "Maternal tetanus vaccination")
)

# TODO
# Convert to factors
# Univariable table
# Multivariable table
