set.seed(1234)
sigmas <- get_sigmas(n_predictors = 1, ICC = 0.05, R2 = 0.7, int_pred_corr = 0.2)

data <- generate_continuous(n_studies = 100, study_sample_size = 100, n_predictor = 1, sigmas = sigmas)
haven::write_dta(data = data, path = "Results/int_pred_corr2.dta")

model <- lme4::lmer("x1~1 + (1|studyid)", data = data)
performance::icc(model)

rand_model <- model_lmm_random_int_reml(data)
fixed_model <- model_lm_fixed_int(data)

phtest_glmer <- function (glmerMod, glmMod, ...)  {  ## changed function call

  # code from here: https://stackoverflow.com/questions/23630214/hausmans-specification-test-for-glmer-from-lme4

  coef.wi <- coef(glmMod)
  coef.re <- lme4::fixef(glmerMod)  ## changed coef() to fixef() for glmer
  vcov.wi <- stats::vcov(glmMod)
  vcov.re <- stats::vcov(glmerMod)
  names.wi <- names(coef.wi)
  names.re <- names(coef.re)
  coef.h <- names.re[names.re %in% names.wi]
  coef.h <- coef.h[coef.h != c("(Intercept)")]
  dbeta <- coef.wi[coef.h] - coef.re[coef.h]
  df <- length(dbeta)
  dvcov <- vcov.re[coef.h, coef.h] - vcov.wi[coef.h, coef.h]
  stat <- abs(t(dbeta) %*% as.matrix(solve(dvcov)) %*% dbeta)  ## added as.matrix()
  pval <- pchisq(stat, df = df, lower.tail = FALSE)
  names(stat) <- "chisq"
  parameter <- df
  names(parameter) <- "df"
  alternative <- "one model is inconsistent"
  res <- list(statistic = stat, p.value = pval, parameter = parameter,
              method = "Hausman Test",  alternative = alternative,
              data.name=deparse(getCall(glmerMod)$data))  ## changed
  class(res) <- "htest"
  return(res)
}

phtest_glmer(glmerMod = rand_model, glmMod = fixed_model)


glmerMod = rand_model
glmMod = fixed_model

form <- y ~ x1 + (1|studyid)
wi <- plm::plm(form, data = data, model = "within")
re <- plm::plm(form, data = data, model = "random")
plm::phtest(wi, re)
