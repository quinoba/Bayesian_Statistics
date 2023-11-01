rm(list=ls())
library(readr)
library(StanHeaders)
library(rstan) 
library(footBayes)
library(lubridate)
library(readxl)
library(openxlsx)
library(dplyr)
library(bayesplot)
library(tidyverse)
library(loo)
setwd("~/Dropbox/MA_Econ_Upp/Bayesian_Statistics/code/final_assignment/code")
# Import entire dataset
data <- read.xlsx("../data/final_data/all_seasons_ranking.xlsx")



# Filter out so it is only Allsvenskan and 2022 season
filtered_gamesALL22 <- data %>%
  filter(League == "Allsvenskan" & Season == 2022)


# Ranking Dataset
ranking <- filtered_gamesALL22 %>% select(Home, home_avg_rating) %>% 
  rename(rank_team = Home, points = home_avg_rating)
ranking <- ranking[!duplicated(ranking), ]
###############################################################################
# Bivariate Poission dynamic

stan_dataALL22 <- data.frame(
  season = filtered_gamesALL22$Season,
  home = filtered_gamesALL22$Home,
  away = filtered_gamesALL22$Away,
  homegoals = filtered_gamesALL22$HG,
  awaygoals = filtered_gamesALL22$AG
)
options(mc.cores = parallel::detectCores())

fitALL22 <- stan_foot(
  data = stan_dataALL22,
  model = "biv_pois",
  predict = 32,
  ranking = ranking,
  dynamic_type = "weekly",
  ind_home = "TRUE"  # Set as a character string
)


###############################################################################
# Bivariate Poission static 
options(mc.cores = parallel::detectCores())
fitALL22stat <- stan_foot(
  data = stan_dataALL22,
  model = "biv_pois",
  predict = 32,
  ind_home = "TRUE"  # Set as a character string
)

###############################################################################
# Skellam dynamic

fitALL22skell <- stan_foot(
  data = stan_dataALL22,
  model = "skellam",
  predict = 32,
  dynamic_type = "weekly",
  ind_home = "TRUE"  # Set as a character string
)

###############################################################################
# Skellam static 
fitALL22skellstat<- stan_foot(
  data = stan_dataALL22,
  model = "skellam",
  predict = 32,
  ind_home = "TRUE"  # Set as a character string
)

###############################################################################
load("../code/all_objects_ranking.RData") 
load("../code/all_objects_ranking_np.RData") 
# Experimenting
abilities <- foot_abilities(fitALL22, stan_dataALL22, type= "both", cex.var= 1)
plot(abilities)

pp_foot(stan_dataALL22, fitALL22, type= "aggregated")
pp_foot(stan_dataALL22, fitALL22skell, type= "aggregated")
pp_foot(stan_dataALL22, fitALL22stat, type= "aggregated")
pp_foot(stan_dataALL22, fitALL22skellstat, type= "aggregated")

posterior1 <- as.matrix(fitALL22)
mcmc_areas(posterior1, regex_pars = "att")
mcmc_acf(posterior1, regex_pars = "att")


sims <- rstan::extract(fitALL22)
goal_diff <- stan_dataALL22$homegoals-stan_dataALL22$awaygoals
goal_diff <- goal_diff[-((length(goal_diff-31):length(goal_diff)))]
ppc_dens_overlay(goal_diff, sims$y_rep[,,1]-sims$y_rep[,,2], bw = 0.5)


sims_skell <- rstan::extract(fitALL22skell)

sims_stat <- rstan::extract(fitALL22stat)

sims_skell_stat <- rstan::extract(fitALL22skellstat)

# Do the LOOIC and WAIC for each of the models 

log_lik_bivpoisdyn <- extract_log_lik(fitALL22)
log_lik_1_bivpoisstat <- extract_log_lik(fitALL22stat)
log_lik_skelldyn <- extract_log_lik(fitALL22skell)
log_lik_skellstat <- extract_log_lik(fitALL22skellstat)

loo1 <- loo(fitALL22)
loo2 <- loo(fitALL22stat)
loo3 <- loo(fitALL22skell)
loo4 <- loo(fitALL22skellstat)

loo_compare(loo1, loo2, loo3, loo4)
loo1
loo2
loo3
loo4


# Abilities
abilities1 <- foot_abilities(fitALL22, stan_dataALL22, type= "both", cex.var= 1)
abilities1
abilities2 <- foot_abilities(fitALL22skell, stan_dataALL22, type= "both", cex.var= 1)
abilities2
abilities3 <- foot_abilities(fitALL22skellstat, stan_dataALL22, type= "both", cex.var= 1)
abilities3
abilities4 <- foot_abilities(fitALL22stat, stan_dataALL22, type= "both", cex.var= 1)
abilities4


save.image('all_objects_ranking.RData')



#### descriptive stats

pivot_data <- stan_dataALL22 %>%  mutate(difference = homegoals- awaygoals)%>%
  pivot_longer(cols = c(homegoals, awaygoals, difference), names_to = "Variable", values_to = "Value")


summary_stats <- pivot_data %>%
  group_by(Variable) %>%
  summarize(
    Min = min(Value),
    Q1 = quantile(Value, 0.25),
    Median = median(Value),
    Mean = mean(Value),
    Q3 = quantile(Value, 0.75),
    Max = max(Value),
    sd = sd(Value)
  )


latex_table <- xtable(summary_stats, caption = "Descriptive Statistics Allsvenskan 2022")


#Result plots
foot_rank(data = stan_dataALL22, object = fitALL22skellstat)
foot_round_robin(data = stan_dataALL22, object = fitALL22skellstat)


############################## New Priors ###################################
fitALL22_np <- stan_foot(
  data = stan_dataALL22,
  model = "biv_pois",
  prior = student_t(4,0,NULL),
  prior_sd = laplace(0,1),
  predict = 32,
  ranking = ranking,
  dynamic_type = "weekly",
  ind_home = "TRUE"  # Set as a character string
)


###############################################################################
# Bivariate Poission static 
options(mc.cores = parallel::detectCores())
fitALL22stat_np <- stan_foot(
  data = stan_dataALL22,
  model = "biv_pois",
  prior = student_t(4,0,NULL),
  prior_sd = laplace(0,1),
  predict = 32,
  ind_home = "TRUE"  # Set as a character string
)

###############################################################################
# Skellam dynamic

fitALL22skell_np <- stan_foot(
  data = stan_dataALL22,
  model = "skellam",
  prior = student_t(4,0,NULL),
  prior_sd = laplace(0,1),
  predict = 32,
  dynamic_type = "weekly",
  ind_home = "TRUE"  # Set as a character string
)

###############################################################################
# Skellam static 
fitALL22skellstat_np<- stan_foot(
  data = stan_dataALL22,
  model = "skellam",
  prior = student_t(4,0,NULL),
  prior_sd = laplace(0,1),
  predict = 32,
  ind_home = "TRUE"  # Set as a character string
)

library(posterior)
stan_est = rstan::extract(fitALL22skellstat_np, permuted=FALSE)
df_summary_conv <- summarise_draws(fitALL22skellstat_np, default_convergence_measures())

traceplot(fitALL22skell_np)
Rhat(stan_est)
ess_bulk(stan_est)
save.image('all_objects_ranking_np.RData')


log_lik_bivpoisdyn <- extract_log_lik(fitALL22)
log_lik_1_bivpoisstat <- extract_log_lik(fitALL22stat)
log_lik_skelldyn <- extract_log_lik(fitALL22skell)
log_lik_skellstat <- extract_log_lik(fitALL22skellstat)

log_lik_bivpoisdyn_np <- extract_log_lik(fitALL22_np)
log_lik_1_bivpoisstat_np <- extract_log_lik(fitALL22stat_np)
log_lik_skelldyn_np <- extract_log_lik(fitALL22skell_np)
log_lik_skellstat_np <- extract_log_lik(fitALL22skellstat_np)

loo1 <- loo(fitALL22)
loo2 <- loo(fitALL22stat)
loo3 <- loo(fitALL22skell)
loo4 <- loo(fitALL22skellstat)

loo1_np <- loo(fitALL22_np)
loo2_np <- loo(fitALL22stat_np)
loo3_np <- loo(fitALL22skell_np)
loo4_np <- loo(fitALL22skellstat_np)

loo4_np
loo2$estimates


loo_compare(loo1, loo2, loo3, loo4, loo1_np, loo2_np, loo3_np, loo4_np)




###### Latex table loo


estimate_list <- list(
  loo1$estimates,
  loo2$estimates,
  loo3$estimates,
  loo4$estimates,
  loo1_np$estimates,
  loo2_np$estimates,
  loo3_np$estimates,
  loo4_np$estimates
)


# Nombres de los modelos y las columnas
model_names <- c("loo1", "loo2", "loo3", "loo4", "loo5", "loo6", "loo7", "loo8")
col_names <- c("elpd_loo", "p_loo", "looic")

# Crear un data frame para los valores de Estimate y SE
estimates_data <- data.frame(Model = model_names)
for (col in col_names) {
  estimates_data[[col]] <- NA
}

# Llenar el data frame con los valores de Estimate y SE entre paréntesis
for (i in 1:length(estimate_list)) {
  estimates_data[i, col_names] <- paste0(
    round(estimate_list[[i]], 2)
  )
}

# Generar la tabla de LaTeX con kable
library(knitr)
latex_table <- kable(estimates_data, caption = "Estimates and SE for loo1 to loo8", format = "latex")

# Imprimir la tabla
latex_table

