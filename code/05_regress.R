library(tidyverse)
library(broom)
library(gtsummary)

surveys2 <- readRDS("data/surveys2.rds")

# Get a first glance of relevant variables using tbl_summary()

surveys2 |> 
  select(-caseid) |> 
  tbl_summary()




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
