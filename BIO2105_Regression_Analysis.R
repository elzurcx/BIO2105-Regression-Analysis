# ============================================================
# BIO2105: CHAPTER 9 - REGRESSION ANALYSIS
# Correlation, Simple Linear Regression, and Residual Diagnostics
#
# Student Name: Jed Izrael J. Sta. Ana
# Section: Section 4
#
# Datasets: trees and women (built-in R datasets)
# No external datasets or packages are required.
# ============================================================


# ============================================================
# PART I: BLACK CHERRY TREES DATASET
# ============================================================


# ------------------------------------------------------------
# TASK 1: VARIABLE ROLES AND DATA EXPLORATION
# ------------------------------------------------------------

# Load the built-in trees dataset
data(trees)

# Display the first six observations, structure, and summary statistics
print(head(trees))
str(trees)
print(summary(trees[, c("Girth", "Volume")]))

# Predictor (x): Girth, measured in inches
# Response (y): Volume, measured in cubic feet


# ------------------------------------------------------------
# TASK 2: SCATTER PLOT WITH FITTED REGRESSION LINE
# ------------------------------------------------------------

# Fit the simple linear regression model
tree_model <- lm(Volume ~ Girth, data = trees)

# Create the scatter plot
plot(trees$Girth, trees$Volume,
     xlab = "Tree Girth (inches)",
     ylab = "Timber Volume (cubic feet)",
     main = "Black Cherry Trees: Girth vs. Timber Volume (n = 31)",
     pch = 19,
     col = "steelblue",
     cex = 1.2)

grid(col = "gray80")
abline(tree_model, col = "darkorange", lwd = 2)

legend("topleft",
       legend = c("Observed Trees", "OLS Regression Line"),
       pch = c(19, NA),
       lty = c(NA, 1),
       col = c("steelblue", "darkorange"),
       lwd = c(NA, 2))


# ------------------------------------------------------------
# TASK 3: PEARSON'S CORRELATION COEFFICIENT
# BUILT-IN VS. MANUAL CALCULATION
# ------------------------------------------------------------

# Built-in Pearson correlation
r_builtin <- cor(trees$Girth, trees$Volume)

cat("\nTASK 3: PEARSON CORRELATION\n")
cat("Built-in Pearson correlation:\n")
print(round(r_builtin, 6))

# Manual calculation using the standardized average product formula
n <- nrow(trees)
x_bar <- mean(trees$Girth)
y_bar <- mean(trees$Volume)
s_x <- sd(trees$Girth)
s_y <- sd(trees$Volume)

r_manual <- (1 / (n - 1)) *
  sum(((trees$Girth - x_bar) / s_x) *
      ((trees$Volume - y_bar) / s_y))

cat("Manual Pearson correlation:\n")
print(round(r_manual, 6))

# Test the significance of the correlation
cat("Pearson correlation significance test:\n")
print(cor.test(trees$Girth, trees$Volume))


# ------------------------------------------------------------
# TASK 4: INVARIANCE UNDER LINEAR TRANSFORMATION
# ------------------------------------------------------------

# Convert Girth from inches to centimeters
girth_cm <- 2.54 * trees$Girth

# Convert Volume from cubic feet to liters
volume_L <- 28.3168 * trees$Volume

# Calculate correlation after unit conversion
r_metric <- cor(girth_cm, volume_L)

cat("\nTASK 4: UNIT CONVERSION AND CORRELATION\n")
cat("Original correlation (inches and cubic feet):\n")
print(round(r_builtin, 6))

cat("Metric correlation (centimeters and liters):\n")
print(round(r_metric, 6))

cat("Absolute difference between correlations:\n")
print(abs(r_builtin - r_metric))

# Compare regression slopes before and after unit conversion.
# The numerical slope changes because its measurement units change.
metric_model <- lm(volume_L ~ girth_cm)

cat("Original regression slope (cubic feet per inch):\n")
print(round(coef(tree_model)["Girth"], 6))

cat("Converted regression slope (liters per centimeter):\n")
print(round(coef(metric_model)["girth_cm"], 6))


# ------------------------------------------------------------
# TASK 5: SIMPLE LINEAR REGRESSION ESTIMATION
# AND CENTROID PROPERTY
# ------------------------------------------------------------

# Extract regression coefficients
coeffs <- coef(tree_model)
b0 <- coeffs[1]
b1 <- coeffs[2]

cat("\nTASK 5: REGRESSION ESTIMATION\n")
cat("Regression coefficients:\n")
print(coeffs)

cat("Regression equation:\n")
cat("Volume =", round(b0, 4), "+",
    round(b1, 4), "(Girth)\n")

# Verify the slope formula: b1 = r * (sy / sx)
b1_from_r <- r_builtin * (s_y / s_x)

cat("Slope from lm():\n")
print(round(b1, 6))

cat("Slope from r * (sy / sx):\n")
print(round(b1_from_r, 6))

# Verify that the regression line passes through the centroid
y_hat_at_xbar <- b0 + b1 * x_bar

cat("Mean Girth:\n")
print(round(x_bar, 6))

cat("Mean Volume:\n")
print(round(y_bar, 6))

cat("Predicted Volume at mean Girth:\n")
print(round(y_hat_at_xbar, 6))


# ------------------------------------------------------------
# TASK 6: COEFFICIENT OF DETERMINATION (R-SQUARED)
# ------------------------------------------------------------

r_squared <- summary(tree_model)$r.squared

cat("\nTASK 6: COEFFICIENT OF DETERMINATION\n")
cat("R-squared from regression model:\n")
print(round(r_squared, 6))

cat("Squared Pearson correlation:\n")
print(round(r_builtin^2, 6))

cat("Percentage of variation explained:\n")
print(round(r_squared * 100, 2))


# ------------------------------------------------------------
# TASK 7: INFERENCE FOR SLOPE AND
# CONFIDENCE VS. PREDICTION INTERVALS
# ------------------------------------------------------------

model_summary <- summary(tree_model)

cat("\nTASK 7: SLOPE INFERENCE AND PREDICTION INTERVALS\n")
cat("Full regression model summary:\n")
print(model_summary)

# 95% confidence intervals for the intercept and slope
cat("95% confidence intervals for regression parameters:\n")
print(confint(tree_model, level = 0.95))

# Test H0: beta1 = 0 against Ha: beta1 != 0
t_stat <- model_summary$coefficients["Girth", "t value"]
df <- tree_model$df.residual
p_value <- model_summary$coefficients["Girth", "Pr(>|t|)"]

cat("Slope hypothesis test:\n")
cat("t-statistic:", round(t_stat, 4), "\n")
cat("Degrees of freedom:", df, "\n")
cat("p-value:", format.pval(p_value, digits = 4), "\n")

# Predict at Girth values of 10, 14, and 18 inches
new_trees <- data.frame(Girth = c(10, 14, 18))

# 95% confidence intervals for the mean response
ci_mean <- predict(
  tree_model,
  newdata = new_trees,
  interval = "confidence",
  level = 0.95
)

cat("95% confidence intervals (mean response):\n")
print(round(ci_mean, 4))

# 95% prediction intervals for individual trees
pi_indiv <- predict(
  tree_model,
  newdata = new_trees,
  interval = "prediction",
  level = 0.95
)

cat("95% prediction intervals (individual trees):\n")
print(round(pi_indiv, 4))


# ============================================================
# PART II: AMERICAN WOMEN HEIGHT AND WEIGHT DATASET
# ============================================================


# ------------------------------------------------------------
# TASK 8: MODEL FITTING AND SCATTER PLOT
# ------------------------------------------------------------

data(women)

cat("\nTASK 8: AMERICAN WOMEN HEIGHT AND WEIGHT\n")
cat("First six observations:\n")
print(head(women))

cat("Dataset structure:\n")
str(women)

# Fit the simple linear regression model
women_model <- lm(weight ~ height, data = women)

cat("Regression model summary:\n")
print(summary(women_model))

women_coeffs <- coef(women_model)

cat("Regression coefficients:\n")
print(women_coeffs)

cat("Regression equation:\n")
cat("Weight =", round(women_coeffs[1], 4), "+",
    round(women_coeffs[2], 4), "(Height)\n")

# Create scatter plot with fitted regression line
plot(women$height, women$weight,
     xlab = "Height (inches)",
     ylab = "Weight (lbs)",
     main = "American Women (Ages 30-39): Height vs. Weight (n = 15)",
     pch = 19,
     col = "steelblue",
     cex = 1.2)

grid(col = "gray80")
abline(women_model, col = "darkorange", lwd = 2)

legend("topleft",
       legend = c("Observed Data", "OLS Regression Line"),
       pch = c(19, NA),
       lty = c(NA, 1),
       col = c("steelblue", "darkorange"),
       lwd = c(NA, 2))


# ------------------------------------------------------------
# TASK 9: ANOVA TABLE AND SUM OF SQUARES DECOMPOSITION
# ------------------------------------------------------------

women_anova <- anova(women_model)

cat("\nTASK 9: ANOVA AND SUM OF SQUARES\n")
cat("ANOVA table:\n")
print(women_anova)

# Calculate observed, fitted, and residual values
y_obs <- women$weight
y_hat <- fitted(women_model)
res <- residuals(women_model)
y_mean <- mean(y_obs)

# Calculate total, regression, and error sums of squares
ss_total <- sum((y_obs - y_mean)^2)
ss_reg <- sum((y_hat - y_mean)^2)
ss_error <- sum(res^2)

cat("Total sum of squares (SS_Total):\n")
print(round(ss_total, 4))

cat("Regression sum of squares (SS_Regression):\n")
print(round(ss_reg, 4))

cat("Error sum of squares (SS_Error):\n")
print(round(ss_error, 4))

cat("SS_Regression + SS_Error:\n")
print(round(ss_reg + ss_error, 4))

# Verify SS_Total = SS_Regression + SS_Error
cat("Verification of sum-of-squares identity:\n")
print(isTRUE(all.equal(ss_total, ss_reg + ss_error)))

# Verify R-squared from the sums of squares
r_squared_anova <- ss_reg / ss_total

cat("R-squared from ANOVA calculations:\n")
print(round(r_squared_anova, 6))

cat("R-squared from regression model:\n")
print(round(summary(women_model)$r.squared, 6))

# Verify F = t^2 for simple linear regression
f_anova <- women_anova[1, "F value"]
t_slope <- summary(women_model)$coefficients["height", "t value"]

cat("ANOVA F-statistic:\n")
print(unname(f_anova))

cat("Squared t-statistic for slope:\n")
print(unname(t_slope^2))

cat("Verification of F = t-squared (allowing for numerical tolerance):\n")
print(isTRUE(all.equal(
  as.numeric(f_anova),
  as.numeric(t_slope^2),
  tolerance = 1e-10
)))


# ------------------------------------------------------------
# TASK 10: RESIDUAL DIAGNOSTICS AND ASSUMPTION CHECKING
# ------------------------------------------------------------

# Display the standard 2-by-2 regression diagnostic panel
old_par <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(women_model)
par(old_par)

# Create a separate Residuals vs. Fitted plot with zero reference line
plot(fitted(women_model), residuals(women_model),
     xlab = "Fitted Values (lbs)",
     ylab = "Residuals (lbs)",
     main = "Residuals vs. Fitted Values (women_model)",
     pch = 19,
     col = "steelblue",
     cex = 1.2)

grid(col = "gray80")
abline(h = 0, col = "red", lty = 2, lwd = 2)


# ============================================================
# END OF BIO2105 REGRESSION ANALYSIS
# ============================================================

cat("\nBIO2105 Regression Analysis completed successfully.\n")
