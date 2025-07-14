# 04_visualizations.R
# ====================
# This script generates visualizations from the analysis results.
# Output: Plots saved to /results/figures/

# 00. Load Libraries
library(ggplot2)
library(wesanderson)
library(RColorBrewer)

# 01. Load results

# model
qpois <- readRDS('results/models/aspergillosis_model.rds')
# residuals
df_resid <- readRDS('results/models/residuals.rds')
# robust table
robust_results <- readRDS('results/models/robust_results.rds')
# contrasts
contrast_df <- readRDS('results/models/contrasts.rds')

# 02. Generate Plots
# model residuals
ggplot(df_resid, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  theme_minimal() +
  labs(title = "Residuals vs Fitted Values")

# RR instead of log, remove state and year variables
robust_results <- robust_results %>%
  mutate(RR = exp(Estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>%
  filter(str_detect(Term, "Intercept|race2|ethnicity|rural|male|age35|age65|covid|year"))

# forest plot of regression results
p_forest_regression <- ggplot(robust_results, aes(x = reorder(Term, RR), 
                                                  y = RR, ymin = conf.low, 
                                                  ymax = conf.high)) +
  geom_pointrange(size = 1.2, color = "black") +  # No p-value coloring
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  
  coord_flip() +  
  theme_minimal() +
  labs(y = "Rate Ratio (RR)", x = "Predictors",
       title = "Estimated Rate Ratios with Robust Standard Errors",
       subtitle = "Confidence Intervals for Each Predictor") 

# forest plot of contrasts
p_forest_contrast <- ggplot(contrast_df, aes(x = group, y = ratio, ymin = asymp.LCL, 
                                    ymax = asymp.UCL, color = p.value < 0.05)) +
  geom_pointrange(size = 1.2) + # confidence intervals
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # reference line at no change (0)
  coord_flip() +
  theme_minimal() +
  labs(y = "Change in Rate Ratio (Post-COVID vs. Pre-COVID)", x = "Group", 
       title = "Post- vs. Pre-COVID Aspergillosis Rate Ratio Changes",
       subtitle = "Statistically significant comparisons (p < 0.05) are highlighted") +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"))

# boxplot for COVID-19 and rural
p_box_rural <- ggplot(contrast_df, aes(x = race2, y = ratio, fill = rural)) +
  geom_boxplot(width = 0.4, position = position_dodge(width = 0.5)) +
  theme_minimal() +
  labs(y = "aPR Change", x = "Racial Group",
       title = "Comparison of PR Changes Across Race & Rural Status",
       legend.title = "Urbanicity",
       fill = "Residence Type") +
  scale_fill_manual(values = c("rural" = "#E3B710", 
                               "urban" = "#54A5B9"))

# boxplot for COVID-19 and ethnicity
p_box_ethnicity <- ggplot(contrast_df, aes(x = race2, y = ratio, fill = ethnicity)) +
  geom_boxplot(width = 0.4, position = position_dodge(width = 0.2)) +
  theme_minimal() +
  labs(y = "aPR Change", x = "Racial Group",
       title = "Comparison of PR Changes Across Race & Ethnicity Status",
       fill = "Ethnicity") +
  scale_fill_manual(values = c("Non-Hispanic" = "#E3B710", 
                               "Hispanic or Latino" = "#54A5B9"),
                    labels = c("Non-Hispanic" = "Non-Hispanic or Latino",
                               "Hispanic or Latino" = "Hispanic or Latino"))

# 03. Save Plots
ggsave('results/figures/p_forest_regression.png')
ggsave('results/figures/p_forest_contrast.png')
ggsave('results/figures/p_box_rural.png')
ggsave('results/figures/p_box_ethnicity.png')