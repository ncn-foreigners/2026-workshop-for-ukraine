# ============================================================================
# Nonprobability Survey Estimation: IPW, MI, and DR Methods
# ============================================================================
# This script demonstrates various methods for estimating population parameters
# from nonprobability survey data using the 'nonprobsvy' package.
# Key methods: Inverse Probability Weighting (IPW), Missing Imputation (MI), 
# and Doubly Robust (DR) estimation.
# ============================================================================

# Load required libraries
library("nonprobsvy")  # For nonprobability survey estimation
library("ggplot2")    # For data visualization

# ============================================================================
# 1. DATA LOADING AND PREPARATION
# ============================================================================

# Load and inspect the probability (JVS) survey data
data("jvs")
head(jvs)
View(jvs)

# Create a survey design object from the JVS data
# This incorporates sampling weights and stratification information
jvs_svy <- svydesign(
  ids = ~ 1,                        # No clustering
  weights = ~ weight,               # Sampling weights variable
  strata = ~ size + nace + region,  # Stratification by firm size, industry, region
  data = jvs
)

# Load and inspect the nonprobability sample (target population)
data("admin")
head(admin)
View(admin)

# ============================================================================
# 2. INVERSE PROBABILITY WEIGHTING (IPW) ESTIMATION
# ============================================================================

# P(nonprob sample = 1 | region, private, ...)

# IPW Method 1: Using MLE for selection model estimation
# This method estimates the probability of being in the nonprobability sample
# given observed covariates, then inverts these probabilities for weighting
ipw_est1 <- nonprob(
  selection = ~ region + private + nace + size,  # Selection model formula
  target = ~ single_shift,                        # Target variable to estimate
  svydesign = jvs_svy,                            # Reference survey design
  data = admin,                                   # Non-probability sample
  method_selection = "logit"                      # Logistic regression for selection
)

ipw_est1
summary(ipw_est1)
extract(ipw_est1)  # Extract point estimates and confidence intervals

# IPW Method 2: Using GEE for selection model estimation
# GEE (Generalized Estimating Equations) can provide alternative variance estimation
ipw_est2 <- nonprob(
  selection = ~ region + private + nace + size,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  control_selection = control_sel(
    gee_h_fun = 1,      # calibration constraint
    est_method = "gee"   # Use GEE estimation method
  )
)

ipw_est2

# Compare covariate balance between MLE and GEE methods
# Checks if the methods balance covariates in the sample
data.frame(
  ipw_mle = check_balance(~size-1, ipw_est1, 1)$balance,
  ipw_gee = check_balance(~size-1, ipw_est2, 1)$balance
)

weights(ipw_est1) |> summary()

# ============================================================================
# 3. MASS IMPUTATION (MI) / OUTCOME MODEL METHODS
# ============================================================================

# MI Method 1: Generalized Linear Model (GLM) for outcome imputation (Kim et al JRSS A paper)
# Uses logistic regression to model the target variable
mi_est1 <- nonprob(
  outcome = single_shift ~ region + private + nace + size,  # Outcome model formula
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "glm",           # Generalized linear model
  family_outcome = "binomial"       # Binary outcome (logistic regression)
)

mi_est1

# MI Method 2: Nearest Neighbor (NN) imputation for outcome
# Imputes missing values using k nearest neighbors
mi_est2 <- nonprob(
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "nn",
  control_outcome = control_out(k = 5)  # Use 5 nearest neighbors
)

# MI Method 3: Predictive Mean Matching (PMM) imputation for outcome
# Combines GLM predictions with nearest neighbor matching
mi_est3 <- nonprob(
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "pmm",
  family_outcome = "binomial",
  control_outcome = control_out(k = 5)  # Use 5 matches for imputation
)

# Compare NN and PMM imputation methods
rbind(
  "NN" = extract(mi_est2)[, 2:3],   # Extract point estimate and SE
  "PMM" = extract(mi_est3)[, 2:3]
)

# ============================================================================
# 4. DOUBLY ROBUST (DR) ESTIMATION
# ============================================================================

# DR Method 1: Standard doubly robust estimation (Chen et al JASA paper)
# Combines selection model and outcome model for robustness
# Remains consistent if EITHER the selection OR outcome model is correctly specified
dr_est1 <- nonprob(
  selection = ~ region + private + nace + size,
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  method_outcome = "glm",
  family_outcome = "binomial"
)

dr_est1
summary(dr_est1)

# DR Method 2: Doubly robust with bias correction (Yang and Kim JRSS B paper)
# Applies additional bias correction and variance estimation improvements
set.seed(2024)
dr_est2 <- nonprob(
  selection = ~ private + size,
  outcome = single_shift ~ private + size,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  method_outcome = "glm",
  family_outcome = "binomial",
  verbose = TRUE,  # Print progress information
  control_inference = control_inf(
    bias_correction = TRUE,   # Apply bias correction
    vars_combine = TRUE,       # Combine variances optimally
    vars_selection = TRUE      # Include selection model 
  )
)

dr_est2

# ============================================================================
# 5. COMPARISON OF ALL ESTIMATION METHODS
# ============================================================================

# Combine results from all estimation methods
df_s <- rbind(
  extract(ipw_est1),
  extract(ipw_est2),
  extract(mi_est1),
  extract(mi_est2),
  extract(mi_est3),
  extract(dr_est1),
  extract(dr_est2)
)

# Label each estimation method
df_s$est <- c(
  "IPW (MLE)",
  "IPW (GEE)",
  "MI (GLM)",
  "MI (NN)",
  "MI (PMM)",
  "DR",
  "DR (BM)"  # BM = Bias Modified
)

# Visualize point estimates and 95% confidence intervals from all methods
# The red dotted line shows the true population mean
ggplot(data = df_s, 
       aes(y = est, x = mean, xmin = lower_bound, xmax = upper_bound)) + 
  geom_point() +  # Plot point estimates
  geom_vline(xintercept = mean(admin$single_shift), 
             linetype = "dotted", color = "red") +  # True population mean
  geom_errorbar() +  # Plot confidence intervals
  labs(x = "Point estimator and confidence interval", y = "Estimators") +
  theme_bw()

# ============================================================================
# 6. VARIANCE ESTIMATION METHODS
# ============================================================================

# Bootstrap variance estimation for IPW method
# Resamples the data 50 times to estimate sampling variability
set.seed(2024)
ipw_est1_boot <- nonprob(
  selection = ~ region + private + nace + size,
  target = ~ single_shift,
  svydesign = jvs_svy,
  data = admin,
  method_selection = "logit",
  control_inference = control_inf(
    var_method = "bootstrap",  # Use bootstrap for variance
    num_boot = 50              # Number of bootstrap replicates
  ),
  verbose = TRUE
)

# Compare analytic vs bootstrap variance estimates
rbind(
  "IPW analytic variance"  = extract(ipw_est1)[, 2:3],
  "IPW bootstrap variance" = extract(ipw_est1_boot)[, 2:3]
)

# Inspect bootstrap samples (first 3 replicate estimates)
head(ipw_est1_boot$boot_sample, n = 3)

# ============================================================================
# 7. VARIABLE SELECTION IN OUTCOME MODELS
# ============================================================================

# MI estimation with LASSO variable selection
# Automatically selects relevant covariates using regularization
set.seed(2024)
mi_est1_sel <- nonprob(
  outcome = single_shift ~ region + private + nace + size,
  svydesign = jvs_svy,
  data = admin,
  method_outcome = "glm",
  family_outcome = "binomial",
  control_outcome = control_out(
    nfolds = 5,       # 5-fold cross-validation for LASSO
    nlambda = 25,     # Test 25 penalty parameters
    penalty = "lasso" # Use LASSO penalty
  ),
  control_inference = control_inf(
    vars_selection = TRUE  # Include variable selection in inference
  ),
  verbose = TRUE
)

# Compare MI with and without variable selection
rbind(
  "MI without var sel" = extract(mi_est1)[, 2:3],
  "MI with var sel"    = extract(mi_est1_sel)[, 2:3]
)

# Extract outcome model coefficients from variable selection model
round(coef(mi_est1_sel)$coef_out[, 1], 4)

# Extract selection model coefficients from IPW model
round(coef(ipw_est1)$coef_sel[, 1], 4)

# ============================================================================
# 8. MODEL DIAGNOSTICS AND INFERENCE
# ============================================================================

# Get number of observations used in DR estimation
nobs(dr_est1)

# Compute 99% confidence intervals (wider than default 95%)
confint(dr_est1, level = 0.99)

# Summarize the estimated weights (IPW weights for DR estimator)
summary(weights(dr_est1))

# ============================================================================
# 9. DIRECT OUTCOME MODEL METHOD (WITHOUT SELECTION MODEL)
# ============================================================================

# Alternative: Apply outcome model directly using GLM method
# This is a simple outcome-only approach without selection model
res_glm <- method_glm(
  y_nons = admin$single_shift,                           # Target variable in population
  X_nons = model.matrix(~ region + private + nace + size, admin),  # Covariates in population
  X_rand = model.matrix(~ region + private + nace + size, jvs),    # Covariates in sample
  svydesign = jvs_svy                                    # Survey design for reference data
)

res_glm

# ============================================================================
# END OF SCRIPT
# ============================================================================