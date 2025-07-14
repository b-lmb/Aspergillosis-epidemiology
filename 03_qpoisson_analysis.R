# 03_qpoisson_analysis.R
# =============
# This script runs the main statistical analysis for aspergillosis.
# Output: Models and summary statistics saved to /results/

# load libraries
library(dplyr)
library(ggplot2)
library(jtools)
library(sandwich)
library(emmeans)
library(lattice)
library(car)
library(MASS)
library(broom)

# load transformed data
df1 <- read_csv('data/processed/aspergillosis_transformed.csv')

# filter to remove race*ethnicity*rural groups with 0 cases
df1_filtered <- glm(aspergillosis ~ age_group + gender + region, data = asp_transformed, family = binomial())

# 01. Main Model

# run model
qpois <- glm(n_fungal ~ race2*covid + ethnicity*covid + rural*covid + male + age + state + 
               year_center*covid,
             family = "quasipoisson",
             offset = log(n_rwdpts),
             data = df1_filtered)

# exponentiate
summ(qpois, exp = T)

# 02. Robust Standard Errors

# Extract model coefficients
regression_results <- tidy(qpois)

# compute robust standard errors
robust_se <- vcovHC(qpois, type = "HC0")

# Extract test statistics and coefficients with robust SEs
regression_results <- coeftest(qpois, vcov. = robust_se)

# combine into dataframe
robust_results <- data.frame(
  Term = rownames(regression_results),
  Estimate = regression_results[, 1],
  Robust_SE = regression_results[, 2],
  z_value = regression_results[, 3],
  p_values = regression_results[, 4]
)

# add 95% confidence intervals
z_value <- 1.96
robust_results$conf.low <- robust_results$Estimate - z_value * robust_results$Robust_SE
robust_results$conf.high <- robust_results$Estimate + z_value * robust_results$Robust_SE

print(robust_results)

# 03. Post Hoc Analysis COVID-19 Interactions

# get ratios for different groups
emm_options(rg.limit = 50000)
emm <- emmeans(qpois, ~ race2*ethnicity*rural*covid,
               at = list(year_center = 0),
               type = "response")

# compare Post- vs. Pre-COVID within each group
contrast_results <- contrast(emm, "revpairwise", by = c("race2", "ethnicity", "rural"), 
                             adjust = "sidak")

contrast_summary <- summary(contrast_results, infer = c(TRUE, TRUE), type = "response")

contrast_df <- as.data.frame(contrast_summary) %>%
  mutate(rural = ifelse(rural == "0", "urban", "rural"), # replace binary indicator with label
         group = paste(race2, ethnicity, rural, sep = " - "), # combine groups with separators
         p.value = signif(p.value, 3)) # format p-values to 3 digits

# 04. Test for Trends
lm_model <- lm(ratio ~ race2 + ethnicity, data = contrast_df)
summary(lm_model)

# 05. Tune Model

# check residual deviance
deviance(qpois) / df.residual(qpois)

# create model matrix (excluding intercept)
X <- model.matrix(~ race2 * covid + ethnicity * covid + rural * covid + male + state, data = df1)[, -1]

# compute Variance Inflation Factor manually
vif_manual <- diag(solve(cor(X)))  # inverse of correlation matrix

print(vif_manual)

# check residuals
df_resid <- data.frame(Fitted = fitted(qpois), Residuals = residuals(qpois))

# save model output
saveRDS(qpois, 'results/models/aspergillosis_model.rds')
# save residuals table
saveRDS(df_resid, 'results/models/residuals.rds')
# save robust table
saveRDS(robust_results, 'results/models/robust_results.rds')
# save contrast df
saveRDS(contrast_df, 'results/models/contrasts.rds')
