# ============================================================================
# Nonprobability Survey Estimation: IPW, MI, and DR Methods with Exercises
# ============================================================================
# This script demonstrates various methods for estimating population parameters
# from nonprobability survey data using the 'nonprobsvy' package.
# Includes practical exercises to explore different modeling approaches.
# ============================================================================

# Load required libraries
library(nonprobsvy)  # For nonprobability survey estimation
library(survey)      # For survey sampling methods
library(ggplot2)     # For data visualization

# ============================================================================
# SETUP: DATA LOADING AND SURVEY DESIGN
# ============================================================================

# Load the nonprobability (JVS) survey data
data("jvs")

# Create a survey design object with sampling weights and stratification
jvs_svy <- svydesign(
  ids = ~ 1,
  weights = ~ weight,
  strata = ~ size + nace + region,
  data = jvs
)

# Load the population/administrative data (target population)
data("admin")

# ============================================================================
# SECTION 1: INVERSE PROBABILITY WEIGHTING (IPW) - STANDARD METHOD
# ============================================================================

# Baseline IPW estimation using logit selection model
est1_logit <- nonprob(
  selection = ~ region,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit"
)

# ============================================================================
# EXERCISE 1: Add more variables to the selection model
# ============================================================================
# TODO: Estimate IPW using region, nace, and size variables
# Compare results with the baseline model (region only)
# Are the estimates substantially different?
# Do confidence intervals overlap?

est1_logit_extended <- nonprob(
  selection = ~ region + nace + size,  # ADD nace and size to the model
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit"
)
summary(est1_logit_extended)
extract(est1_logit_extended)

# ============================================================================
# EXERCISE 2: Use probit linking function instead of logit
# ============================================================================
# TODO: Repeat the IPW estimation using probit linking function
# Compare with logit results - are estimates similar?
# Are standard errors different?

est1_probit <- nonprob(
  selection = ~ region,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "probit"  # USE probit INSTEAD OF logit
)
summary(est1_probit)
extract(est1_probit)

# ============================================================================
# EXERCISE 3: Use complementary log-log (cloglog) linking function
# ============================================================================
# TODO: Repeat the IPW estimation using cloglog linking function
# Compare with logit and probit results
# Which linking function gives the most stable estimates?

est1_cloglog <- nonprob(
  selection = ~ region,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "cloglog"  # USE cloglog INSTEAD OF logit
)
summary(est1_cloglog)
extract(est1_cloglog)

# Summary table: Compare all three linking functions for standard IPW
est_ipw_standard <- rbind(
  extract(est1_logit),
  extract(est1_probit),
  extract(est1_cloglog)
)
est_ipw_standard$est <- c("ipw (logit)", "ipw (probit)", "ipw (cloglog)")
rownames(est_ipw_standard) <- NULL
print(est_ipw_standard)

# Explore the structure of the IPW result object
str(est1_logit, 1)
str(est1_logit$selection, 1)
summary(est1_logit)

# ============================================================================
# SECTION 2: IPW WITH CALIBRATION (GEE METHOD)
# ============================================================================

# IPW with calibration: Uses Generalized Estimating Equations (GEE)
# to improve variance estimation and weight stability
est2_logit <- nonprob(
  selection = ~ size,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  control_selection = control_sel(
    gee_h_fun = 1,
    est_method = "gee"
  )
)

# ============================================================================
# EXERCISE 4: Change covariates in the selection model
# ============================================================================
# TODO: Estimate IPW calibration with different covariate sets
# Try: ~ region, ~ nace, ~ region + nace + size
# Compare estimates and standard errors across models
# Does model complexity improve inference?

est2_extended <- nonprob(
  selection = ~ region + nace + size,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  control_selection = control_sel(gee_h_fun = 1, est_method = "gee")
)
summary(est2_extended)
extract(est2_extended)

# ============================================================================
# EXERCISE 5: Use probit linking function with calibration
# ============================================================================
# TODO: Repeat calibrated IPW estimation using probit
# Compare with logit results from est2_logit
# Is the method robust to linking function choice?

est2_probit <- nonprob(
  selection = ~ size,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "probit",
  control_selection = control_sel(gee_h_fun = 1, est_method = "gee")
)
summary(est2_probit)
extract(est2_probit)

# ============================================================================
# EXERCISE 6: Use cloglog linking function with calibration
# ============================================================================
# TODO: Repeat calibrated IPW estimation using cloglog
# Compare all three linking functions together
# Which provides the most robust estimates?

est2_cloglog <- nonprob(
  selection = ~ size,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "cloglog",
  control_selection = control_sel(gee_h_fun = 1, est_method = "gee")
)
summary(est2_cloglog)
extract(est2_cloglog)

# Summary table: Compare calibrated IPW with different linking functions
est_ipw_calib <- rbind(
  extract(est2_logit),
  extract(est2_probit),
  extract(est2_cloglog)
)
est_ipw_calib$est <- c("ipw calib (logit)", "ipw calib (probit)", "ipw calib (cloglog)")
rownames(est_ipw_calib) <- NULL
print(est_ipw_calib)

# Extract and inspect weights from IPW estimations
admin$ipw1_weight <- weights(est1_logit)
admin$ipw2_weight <- weights(est2_logit)
summary(admin$ipw1_weight)
summary(admin$ipw2_weight)

# Check if weights sum to population size
c(
  jvs = sum(weights(jvs_svy)),
  ipw1_mle = sum(admin$ipw1_weight),
  ipw2_gee = sum(admin$ipw2_weight)
)

# Compare weighted distributions of firm size
svytotal(~size, jvs_svy)           # Survey totals
xtabs(ipw1_weight ~ size, admin)   # IPW weighted totals
xtabs(ipw2_weight ~ size, admin)   # Calibrated IPW weighted totals

# ============================================================================
# SECTION 3: IPW WITH BOOTSTRAP VARIANCE ESTIMATION
# ============================================================================

# ============================================================================
# EXERCISE 7: IPW with bootstrap variance estimation
# ============================================================================
# TODO: Estimate mean using IPW with bootstrap variance
# Set seed to 2024-11-27 for reproducibility
# Try different numbers of bootstrap iterations (num_boot)
# Compare with analytic variance from est1_logit
# Are bootstrap confidence intervals wider or narrower?

set.seed(20241127)

est3_logit <- nonprob(
  selection = ~ region,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  control_inference = control_inf(
    var_method = "bootstrap",
    num_boot = 100
  )
)
summary(est3_logit)
extract(est3_logit)

# ============================================================================
# SECTION 4: IPW WITH VARIABLE SELECTION (SCAD)
# ============================================================================

# ============================================================================
# EXERCISE 8: IPW with SCAD variable selection
# ============================================================================
# TODO: Estimate mean using IPW with SCAD variable selection
# SCAD = Smoothly Clipped Absolute Deviation penalty
# This automatically selects relevant covariates
# Set seed to 2024-11-27 for reproducibility
# Compare selected variables with full model
# Does variable selection change the point estimate significantly?
# Are confidence intervals narrower with fewer variables?

set.seed(20241127)

est4_logit <- nonprob(
  selection = ~ region + private + nace + size,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  verbose = TRUE,
  control_inference = control_inf(vars_selection = TRUE),
  control_selection = control_sel(
    penalty = "SCAD",
    nfolds = 5
  )
)
summary(est4_logit)
extract(est4_logit)

# ============================================================================
# IPW SUMMARY: Compare all IPW variants
# ============================================================================

ipw_summary <- rbind(
  extract(est1_logit),
  extract(est2_logit),
  extract(est3_logit),
  extract(est4_logit)
)
rownames(ipw_summary) <- NULL
ipw_summary$est <- c("ipw (st)", "ipw (cal)", "ipw (boot)", "ipw (scad)")
print(ipw_summary)

# ============================================================================
# SECTION 5: MASS IMPUTATION (MI) - OUTCOME MODELS
# ============================================================================

# MI using outcome model (regression imputation)
# Models the target variable using survey and population data
est5_glm <- nonprob(
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "glm",
  family_outcome = "gaussian"
)

# MI with Gaussian (Linear) Model
extract(est5_glm)

# ============================================================================
# EXERCISE 9: MI with binomial model for binary outcome
# ============================================================================
# TODO: Repeat MI estimation using binomial family
# The single_shift variable is actually binary (0/1)
# So binomial/logistic model is more appropriate
# Compare Gaussian vs Binomial model results
# Which model gives more reasonable estimates?

est5_glm_biom <- nonprob(
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "glm",
  family_outcome = "binomial"
)
summary(est5_glm_biom)
extract(est5_glm_biom)

# ============================================================================
# EXERCISE 10: MI with Nearest Neighbor imputation
# ============================================================================
# TODO: Estimate mean using Nearest Neighbor (NN) imputation
# NN imputes missing values by finding similar observations
# Try different k values (number of neighbors): 3, 5, 10
# How does k affect the point estimate?
# How does k affect the standard error?
# Is NN more or less stable than GLM imputation?

est6_glm_nn <- nonprob(
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "nn",
  control_outcome = control_out(k = 5)
)
summary(est6_glm_nn)
extract(est6_glm_nn)

# ============================================================================
# EXERCISE 11: MI with Predictive Mean Matching (PMM)
# ============================================================================
# TODO: Estimate mean using Predictive Mean Matching (PMM)
# PMM combines GLM predictions with NN matching
# Try different k values (number of matches): 3, 5, 10
# Set seed to 2024-11-27 for reproducibility
# Compare with NN and GLM imputation methods
# Which method gives the best estimates?

set.seed(20241127)

est6_glm_pmm1 <- nonprob(
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "pmm",
  family_outcome = "binomial",
  control_outcome = control_out(k = 5)
)
summary(est6_glm_pmm1)
extract(est6_glm_pmm1)

# Try alternative PMM specification with different k
set.seed(20241127)

est6_glm_pmm2 <- nonprob(
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "pmm",
  family_outcome = "binomial",
  control_outcome = control_out(k = 10)
)
summary(est6_glm_pmm2)
extract(est6_glm_pmm2)

# ============================================================================
# EXERCISE 12: MI with SCAD variable selection
# ============================================================================
# TODO: Estimate mean using MI with SCAD variable selection
# Automatically selects relevant outcome model covariates
# Set seed to 2024-11-27 for reproducibility
# Compare selected variables with full outcome model
# Does selection change the point estimate?
# Are confidence intervals more precise with fewer variables?

set.seed(20241127)

est7_glm_sel <- nonprob(
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "glm",
  family_outcome = "binomial",
  verbose = TRUE,
  control_inference = control_inf(vars_selection = TRUE),
  control_outcome = control_out(
    nfolds = 5,
    nlambda = 25,
    penalty = "SCAD"
  )
)
summary(est7_glm_sel)
extract(est7_glm_sel)

# ============================================================================
# MI SUMMARY: Compare all MI variants
# ============================================================================

mi_summary <- rbind(
  extract(est5_glm),
  extract(est5_glm_biom),
  extract(est6_glm_nn),
  extract(est6_glm_pmm1),
  extract(est6_glm_pmm2),
  extract(est7_glm_sel)
)
rownames(mi_summary) <- NULL
mi_summary$est <- c("mi (lm)", "mi (glm)", "mi (nn)", "mi (pmm1)", "mi (pmm2)", "mi (glm, scad)")
print(mi_summary)

# ============================================================================
# SECTION 6: DOUBLY ROBUST (DR) ESTIMATION
# ============================================================================

# DR combines selection model and outcome model
# Remains consistent if EITHER model is correctly specified
# More robust than IPW or MI alone

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

# DR with Standard IPW and GLM Outcome
extract(est8_dr1)
summary(est8_dr1)
str(est8_dr1, 1)

# ============================================================================
# EXERCISE 13 (DR): Use calibration for IPW component
# ============================================================================
# TODO: Estimate DR using calibrated IPW (GEE) instead of standard IPW
# Add control_selection = control_sel(gee_h_fun = 1, est_method = 'gee')
# Compare with standard DR (est8_dr1)
# Does calibration improve the estimates?
# Are standard errors smaller or larger?

est8_dr2 <- nonprob(
  selection = ~ region + private + nace + size,
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  method_outcome = "glm",
  family_outcome = "binomial",
  control_selection = control_sel(gee_h_fun = 1, est_method = "gee"),
  pop_size = sum(weights(jvs_svy))
)
summary(est8_dr2)
extract(est8_dr2)

# ============================================================================
# EXERCISE 14 (DR): Use bootstrap variance estimation
# ============================================================================
# TODO: Estimate DR using bootstrap variance
# Set seed to 2024-11-27 for reproducibility
# Try different num_boot values: 50, 100, 200
# Compare bootstrap vs analytic variance from est8_dr1
# Are bootstrap confidence intervals wider or narrower?

set.seed(20241127)

est8_dr3 <- nonprob(
  selection = ~ region + private + nace + size,
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  method_outcome = "glm",
  family_outcome = "binomial",
  verbose = TRUE,
  control_inference = control_inf(
    var_method = "bootstrap",
    num_boot = 100
  ),
  pop_size = sum(weights(jvs_svy))
)
summary(est8_dr3)
extract(est8_dr3)

# ============================================================================
# EXERCISE 15 (DR): Use SCAD variable selection
# ============================================================================
# TODO: Estimate DR with SCAD variable selection
# Apply SCAD to selection model (IPW part)
# Set seed to 2024-11-27 for reproducibility
# Compare selected variables with full model
# Does selection improve the point estimate?

set.seed(20241127)

est9_dr1 <- nonprob(
  selection = ~ region + private + nace + size,
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  method_outcome = "glm",
  family_outcome = "binomial",
  verbose = TRUE,
  control_inference = control_inf(vars_selection = TRUE),
  control_selection = control_sel(
    penalty = "SCAD",
    nfolds = 5
  ),
  pop_size = sum(weights(jvs_svy))
)
summary(est9_dr1)
extract(est9_dr1)

# ============================================================================
# EXERCISE 16 (DR): SCAD with bias correction
# ============================================================================
# TODO: Estimate DR with SCAD and bias correction
# Uses bias minimization approach for more robust inference
# Set seed to 2024-11-27 for reproducibility
# Compare with standard DR (est8_dr1) and SCAD DR (est9_dr1)
# Which approach gives the most stable estimates?

set.seed(20241127)

est9_dr2 <- nonprob(
  selection = ~ region + size,
  outcome = single_shift ~ size,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  method_outcome = "glm",
  family_outcome = "binomial",
  verbose = TRUE,
  control_inference = control_inf(
    vars_selection = TRUE,
    bias_correction = TRUE,
    vars_combine = TRUE
  ),
  control_selection = control_sel(
    penalty = "SCAD",
    nfolds = 5
  ),
  pop_size = sum(weights(jvs_svy))
)
summary(est9_dr2)
extract(est9_dr2)

# ============================================================================
# DR SUMMARY: Compare all DR variants
# ============================================================================

dr_summary <- rbind(
  extract(est8_dr1),
  extract(est8_dr2),
  extract(est8_dr3),
  extract(est9_dr1),
  extract(est9_dr2)
)
rownames(dr_summary) <- NULL
dr_summary$est <- c("dr (ipw)", "dr (ipw cal)", "dr (ipw, boot)", "dr (scad)", "dr (scad, bc)")
print(dr_summary)

# ============================================================================
# FINAL COMPARISON: ALL METHODS
# ============================================================================
# Combine all results

results <- rbind(ipw_summary, mi_summary, dr_summary)

# Visualize all estimates with confidence intervals
# The red dotted line shows the true population mean
ggplot(
  data = results,
  aes(y = est, x = mean, xmin = lower_bound, xmax = upper_bound)
) +
  geom_point() +
  geom_vline(
    xintercept = mean(admin$single_shift),
    linetype = "dotted",
    color = "red"
  ) +
  geom_errorbar() +
  labs(
    x = "Point estimator and confidence interval",
    y = "Estimation Methods"
  ) +
  theme_bw()
