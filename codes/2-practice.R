library(nonprobsvy)
library(survey)
library(ggplot2)

## setup 
data("jvs")
jvs_svy <- svydesign(ids = ~ 1, 
                     weights = ~ weight,
                     strata = ~ size + nace + region,
                     data = jvs)

data("admin")

## ----ipw-stadnard----------------------------------------------------------------------------------------------------
est1_logit <- nonprob(
  selection = ~ region,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit"
)

### Exercise 1 Add nace and size variables to the model -- compare the results.

### Exercise 2 Carry out an analogous inference using probit as the linking function.

### Exercise 3 Carry out an analogous inference using cloglog as the linking function.


est_ipw_standard <- rbind(cbind(est1_logit$output, est1_logit$confidence_interval),
                          cbind(est1_probit$output, est1_probit$confidence_interval),
                          cbind(est1_cloglog$output, est1_cloglog$confidence_interval))
est_ipw_standard$est <- "ipw"
rownames(est_ipw_standard) <- NULL
est_ipw_standard


## ----ipw-structure---------------------------------------------------------------------------------------------------
str(est1_logit,1)


## ----ipw-structure-sel-----------------------------------------------------------------------------------------------
str(est1_logit$selection,1)


## ----ipw-summary-----------------------------------------------------------------------------------------------------
summary(est1_logit)


## ----ipw-cal---------------------------------------------------------------------------------------------------------
est2_logit <- nonprob(
  selection = ~ size,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  control_selection = control_sel(gee_h_fun = 1, est_method = "gee")
)

### Exercise 4 Estimate with an another choice for h function.

### Exercise 5 Change the set of dependent variables and compare the results.

### Exercise 6 Carry out an analogous inference using probit as the linking function.

### Exercise 7 Carry out an analogous inference using cloglog as the linking function.

est_ipw_calib <- rbind(cbind(est2_logit$output, est2_logit$confidence_interval),
                       cbind(est2_probit$output, est2_probit$confidence_interval),
                       cbind(est2_cloglog$output, est2_cloglog$confidence_interval))
est_ipw_calib$est <- "ipw calib"
rownames(est_ipw_calib) <- NULL
est_ipw_calib


## ----ipw-cal-weights----------------------------------------------------------------------------------------------------
admin$ipw1_weight <- weights(est1_logit)
admin$ipw2_weight <- weights(est2_logit)


## ----ipw-cal-totals--------------------------------------------------------------------------------------------------
c(jvs=sum(weights(jvs_svy)), ipw1_mle=sum(admin$ipw1_weight), ipw2_gee=sum(admin$ipw2_weight))


## ----ipw-cal-klasa1--------------------------------------------------------------------------------------------------
svytotal(~size, jvs_svy)


## ----ipw-cal-klasa2--------------------------------------------------------------------------------------------------
xtabs(ipw1_weight ~ size, admin)
xtabs(ipw2_weight ~ size, admin)


## ----ipw-bootstrap---------------------------------------------------------------------------------------------------
### Exercise 8 Estimate the mean using the inverse probability weighting method with the bootstrap method for variance.
### In addition, try playing with the number of iterations and arguments
set.seed(2024-11-27)

## bootstrap here
est3_logit

## ----ipw-bootstrap-summary-------------------------------------------------------------------------------------------
summary(est3_logit)


## ----ipw-scad--------------------------------------------------------------------------------------------------------
### Exercise 9 Conduct inference with an additional variable selection step, e.g. SCAD
set.seed(2024-11-27)


## ----ipw-scad-summary------------------------------------------------------------------------------------------------
summary(est4_logit)


## ----ipw-comparison---------------------------------------------------------------------------------------------------
ipw_summary <- rbind(extract(est1_logit$output),
                     extract(est2_logit$output),
                     extract(est3_logit$output),
                     extract(est4_logit$output))
rownames(ipw_summary) <- NULL
ipw_summary$est <- c("ipw (st)", "ipw (cal)", "ipw (boot)", "ipw (scad)")
ipw_summary


## ----mi-glm-lp-------------------------------------------------------------------------------------------------------
est5_glm <- nonprob(
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "glm",
  family_outcome = "gaussian"
)

extract(est5_glm)


## ----mi-glm-binom----------------------------------------------------------------------------------------------------
### Exercise 10 Use a binomial model instead of a Gaussian model for the single_shift variable
extract(est5_glm_biom)


## ----mi-glm-binom-summary--------------------------------------------------------------------------------------------
summary(est5_glm_biom)


## ----mi-glm-binom-structure------------------------------------------------------------------------------------------
str(est5_glm_biom,1)


## ----mi-glm-nn-------------------------------------------------------------------------------------------------------
### Exercise 11 Use another mass imputation method - NN - in addition the number of neighbours (k) can be dealt with
extract(est6_glm_nn)


## ----mi-glm-pmm-1----------------------------------------------------------------------------------------------------
### Exercise 12 Use another mass imputation method - PMM - in addition the number of neighbours (k) can be dealt with
set.seed(2024-11-27)
extract(est6_glm_pmm1)


## ----mi-glm-pmm-2----------------------------------------------------------------------------------------------------
set.seed(2024-11-27)

extract(est6_glm_pmm2$output)


## ----mi-glm-scad-----------------------------------------------------------------------------------------------------
### Exercise 13 Add an additional variable selection step, e.g. SCAD
set.seed(2024-11-27)

## ----mi-glm-scad-result-----------------------------------------------------------------------------------------------
extract(est7_glm_sel)

## ----mi-glm-scad-summary---------------------------------------------------------------------------------------------
summary(est7_glm_sel)


## ----mi-summary--------------------------------------------------------------------------------------------------
mi_summary <- rbind(extract(est5_glm$output),
                    extract(est5_glm_biom$output),
                    extract(est6_glm_nn$output),
                    extract(est6_glm_pmm1$output),
                    extract(est6_glm_pmm2$output),
                    extract(est7_glm_sel$output))
rownames(mi_summary) <- NULL
mi_summary$est <- c("mi (lm)", "mi (glm)", "mi (nn)", "mi (pmm1)", "mi (pmm2)", "mi (glm, scad)")
mi_summary


## ----dr-glm-binom----------------------------------------------------------------------------------------------------
est8_dr1 <- nonprob(
  selection = ~ region + private + nace + size,
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  method_outcome = "glm",
  family_outcome = "binomial",
  pop_size = sum(weights(jvs_svy))
)

extract(est8_dr1)

## ----dr-glm-binom-summary--------------------------------------------------------------------------------------------
summary(est8_dr1)


## ----dr-glm-binom-structure------------------------------------------------------------------------------------------
str(est8_dr1,1)


## ----dr-glm-calib----------------------------------------------------------------------------------------------------
### EX 13 Use a calibration method for inverse probability weighting part
extract(est8_dr2)


## ----dr-glm-bootstrap------------------------------------------------------------------------------------------------
### EX 14 Use a bootstrap variance for DR estimator
set.seed(2024-11-27)
extract(est8_dr3)


## ----dr-glm-scad-----------------------------------------------------------------------------------------------------
### EX 15 Add an additional variable selection step, e.g. SCAD
set.seed(2024-11-27)
set.seed(est9_dr1)


## ----dr-glm-scad-bias-min--------------------------------------------------------------------------------------------
### EX 16 Add an additional variable selection step, e.g. SCAD and estimate using bias minimization approach
set.seed(2024-11-27)
extract(est9_dr2)


## ----dr-summary-------------------------------------------------------------------------------------------------
dr_summary <- rbind(extract(est8_dr1$output),
                    extract(est8_dr2$output),
                    extract(est8_dr3$output),
                    extract(est9_dr1$output),
                    extract(est9_dr2$output))
rownames(dr_summary) <- NULL
dr_summary$est <- c("dr (ipw)", "dr (ipw cal)", "dr (ipw, boot)", "dr (scad)", "dr (scad, min)")
dr_summary


## ----summary-plot------------------------------------------------------------------------------------------------
results <- rbind(ipw_summary, mi_summary, dr_summary)

ggplot(data = results, aes(y = est, x = mean, xmin = lower_bound, xmax = upper_bound)) +
  geom_point() +
  geom_vline(xintercept = mean(admin$single_shift), linetype = "dotted", color = "red") +
  geom_errorbar() +
  labs(x = "Point estimator and confidence interval", y = "estimators")