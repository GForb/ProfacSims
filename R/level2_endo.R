# Investigating level 2 endogeneity

sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.4, int_pred_corr = 0.5)

data <- generate_continuous(n_studies =  1000, 100, sigmas = sigmas)

data_numbers <- data[, 2:14]

cor(data_numbers)

fe <- model_lm_fixed_int(data)
re <- model_lmm_random_int_ml(data)

coefs_fe <- coef(fe)[grep("^[xX].*", names(coef(fe)), value=FALSE)]
coefs_re <- coef(re)[[1]][1, 2:13] |> as.numeric()

coefs_re
coefs_fe
sigmas$beta_x

mean(coefs_fe - sigmas$beta_x)/sigmas$beta_x*100
mean(coefs_re - sigmas$beta_x)/sigmas$beta_x*100

summary(re)
sigmas$u^2
