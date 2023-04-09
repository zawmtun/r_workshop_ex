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

education_levels <- c("No education", "Primary", "Secondary", "More than secondary")
employ_levels <- c("Agricultural", "Manual", "Sales", "Not working", "Other")
wealth_levels <- c("Lowest", "Second", "Third", "Fourth", "Highest")
problem_levels <- c("Not a big problem", "Big problem")
ancplace_levels <- c("Public health facilities",
                     "Private health facilities or NGO clinics",
                     "Mixed",
                     "Home",
                     "None")

tt2 <- tt1 |> 
  select(-c(caseid, age, birth_order_last_child)) |> 
  relocate(agegrp, .before = everything()) |> 
  relocate(birth_order_last_child_grp, .after = education) |> 
  mutate(
    education = factor(education, levels = edu_levels),
    employ = factor(employ, levels = employ_levels),
    wealth = factor(wealth, levels = wealth_levels),
    get_help_permission = factor(get_help_permission, levels = problem_levels),
    get_help_not_go_alone = factor(get_help_not_go_alone, levels = problem_levels),
    get_help_money = factor(get_help_money, levels = problem_levels),
    get_help_distance_health_facility = factor(get_help_distance_health_facility, levels = problem_levels),
    ancplace = factor(ancplace, levels = ancplace_levels),
    tetanus_vacc = (tetanus_vacc == "Vaccinated")
  )

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
  tetanus_vacc = "Maternal tetanus vaccination"
)

# Descriptive table

desc <- tbl_summary(tt2, label = var_labels)

# Univariable regression

m1 <- glm(tetanus_vacc ~ agegrp, family = binomial, data = tt2)
summary(m1)
coef(m1)
tidy(m1, exponentiate = TRUE, conf.int = TRUE)


uv_regress <- tbl_uvregression(
  tt2,
  label = var_labels,
  method = glm,
  y = tetanus_vacc,
  method.args = list(family = binomial),
  exponentiate = TRUE,
  hide_n = TRUE
) |> 
  add_global_p()

uv_regress

t_merge <- tbl_merge(
  tbls = list(desc, uv_regress),
  tab_spanner = c("Summary statistics", "Univariable regression")
)

t_merge

m2 <- glm(tetanus_vacc ~ agegrp + education + ancplace, family = binomial, data = tt2)
summary(m2)

var_labels <-  c(
  agegrp = "Age in years",
  education = "Highest education level of mothers",
  ancplace = "Type of facility where women received antenatal care"
)

mv_regress <- tbl_regression(
  m2, label = var_labels,
  exponentiate = TRUE
) |> 
  add_global_p()

mv_regress |> 
  as_tibble()

# TODO
# Univariable table
# Multivariable table
