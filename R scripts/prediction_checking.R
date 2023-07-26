sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)

data <- generate_continuous(10,100,sigmas=sigmas)
test_data <- generate_continuous(10,100,sigmas=sigmas, intercepts_data = data)
model<- lme4::lmer("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12+ (1|studyid)",
                   data = data)

pred1 <- predict(model, random.only = TRUE, newdata = data)
pred2 <- predict(model, random.only = TRUE, newdata = test_data)
pred3 <- get_rand_int(model, data)
pred4 <- get_rand_int(model, test_data)
data.frame(pred3, pred4, lme4::ranef(model))
