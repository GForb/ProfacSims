library(tidyverse)

sim_name <- "240203-cluster-x"
i <- 1


results_folder <- here::here("Results/ClusterSims/", sim_name)
save_time = lubridate::now()


sim_no <- formatC(i, width = 4, format = "d", flag = "0")
file <- paste0("results", sim_no, ".RDS")

print(file)

data <- readRDS(here::here(results_folder, file)) |> tibble()
cn <- colnames(data)

scenario <- data |> filter(ICC == 0.05, n_studies ==4, study_sample_size_train==50, b_w_ratio == 2, pred_icc == 0.5) |>
  select(-se, -ci.lb, -ci.ub, -pi.lb, -pi.ub, -test_ss,
         -rng_state, -model_function_list, -n_studies_test,
         -predictor_intercepts, -study_sample_size_test,
         -n_predictors ,-intercept_est_sample_size)

scenario |> select(model) |> table()

xbar_model <- scenario |> filter(model == "Random intercetp - REML x1bar", metric =="var_u")
xbar_model$betas

ri_model <- scenario |> filter(model == "Random intercetp - REML", metric =="var_u")
ri_model$betas


sigmas <- get_sigmas(n_predictors = 1, ICC = 0.05, R2 = 0.4,
                     int_pred_corr = 0, pred_icc = 0.5, b_w_ratio  = 2, single_x = TRUE)
train_data <- generate_continuous(n_studies = 10, study_sample_size = 10000, n_predictors = 1, sigmas = sigmas)

model <- model_lm_fixed_int(train_data) |> summary()

# What happens to the between bit: It goes into the intercept
study <- 10
train_data$study_intercept[study]  +(sigmas$beta_b - sigmas$beta_w)*train_data$X_b[study]
coef(model)[1,1] + coef(model)[study,1]

# What does the model have for betweene effects:
# beta_w*X_b

# what does the data have:
#beta_b*X_b

# If the model sticks it all in the intercept then intercept =beta_b*X_b - beta_w*X_b





train_data$X_b[study]*sigmas$beta_b


# When there is a lot of between study correlation in predicors there is little variance to go round.
# Conditional on study, the total variance is var x_w + var_e
# Therefore r-squared is much lower

# Theoretical r-squared:

sigmas <- get_sigmas(n_predictors = 1, ICC = 0.05, R2 = 0.4,
                     int_pred_corr = 0, pred_icc = 0.9, b_w_ratio  = 2, single_x = TRUE)
sigmas$x_w^2*sigmas$beta_w^2 /(sigmas$x_w^2*sigmas$beta_w^2 +sigmas$e^2)

plot |> group_by(model, n_studies, pred_icc) |> summarise(mean = mean(est), se = sd(est)) |> print(n = 100)

sigmas$e^2
sigmas$x_w^2
sigmas$u^2
sigmas$beta_w


#what does a prediction for an individual look like?
# how does the centred model not do better?
# is there actually a difference between cetnreed and reml?
train_data


