# ==============================================================================
# COURSEWORK: STW7089CEM - Statistical Methods for Data Science
# Title: Modeling Cloud Server Power Consumption
# Student Coursework Script for Tasks 1, 2, and 3
# ==============================================================================

# Clear environment and set seed for reproducibility
rm(list = ls())
set.seed(2026)

# ------------------------------------------------------------------------------
# 1. SETUP & PACKAGE LOADING
# ------------------------------------------------------------------------------
# Load required libraries (install if missing)
required_packages <- c("ggplot2", "gridExtra", "corrplot", "MASS", "stats", "utils")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# Create folders for saving plots and output tables
plots_dir <- "plots"
output_dir <- "output"
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# 2. DATA LOADING & PREPARATION
# ------------------------------------------------------------------------------
# Helper function to find dataset files by pattern or fallback name
locate_dataset_file <- function(pattern, fallback_name) {
  matches <- list.files(pattern = pattern)
  if (length(matches) > 0) {
    return(matches[1])
  } else if (file.exists(fallback_name)) {
    return(fallback_name)
  } else {
    stop(paste("File not found for pattern:", pattern))
  }
}

x_file    <- locate_dataset_file("^X.*\\.csv$", "X.csv")
y_file    <- locate_dataset_file("^y.*\\.csv$", "y.csv")
time_file <- locate_dataset_file("^time.*\\.csv$", "time.csv")

# Load raw CSV files (no headers as per coursework brief)
X_raw    <- read.csv(x_file, header = FALSE)
y_raw    <- read.csv(y_file, header = FALSE)
time_raw <- read.csv(time_file, header = FALSE)

# Combine into a clean data frame with descriptive column names
N <- nrow(X_raw)
telemetry_data <- data.frame(
  time = as.numeric(time_raw[, 1]),
  x1   = as.numeric(X_raw[, 1]), # CPU Utilization
  x2   = as.numeric(X_raw[, 2]), # Memory Usage
  x3   = as.numeric(X_raw[, 3]), # Disk I/O
  x4   = as.numeric(X_raw[, 4]), # Network Traffic
  y    = as.numeric(y_raw[, 1])  # Power Consumption (Target)
)

X_mat    <- as.matrix(telemetry_data[, c("x1", "x2", "x3", "x4")])
y_vec    <- as.numeric(telemetry_data$y)
time_vec <- as.numeric(telemetry_data$time)

cat(sprintf("Dataset loaded: %d observations, 4 input features, 1 output target.\n\n", N))

# Clean ggplot theme for consistent figures
theme_publication <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(b = 6)),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30", margin = margin(b = 8)),
      axis.title    = element_text(face = "bold", size = 11),
      axis.text     = element_text(size = 10, color = "black"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.border  = element_rect(color = "gray80", fill = NA, linewidth = 0.8)
    )
}

# ==============================================================================
# TASK 1: PRELIMINARY DATA ANALYSIS
# ==============================================================================
cat("--- Running Task 1: Preliminary Data Analysis ---\n")

# --- Task 1.1: Time Series Plots of Input and Output Signals ---
# Plot each signal over time to observe temporal patterns
p_ts_x1 <- ggplot(telemetry_data, aes(x = time, y = x1)) +
  geom_line(color = "#1f77b4", linewidth = 0.8) +
  labs(title = "x1: CPU Utilization", x = "Time (seconds)", y = "Signal Value") +
  theme_publication()

p_ts_x2 <- ggplot(telemetry_data, aes(x = time, y = x2)) +
  geom_line(color = "#2ca02c", linewidth = 0.8) +
  labs(title = "x2: Memory Usage", x = "Time (seconds)", y = "Signal Value") +
  theme_publication()

p_ts_x3 <- ggplot(telemetry_data, aes(x = time, y = x3)) +
  geom_line(color = "#ff7f0e", linewidth = 0.8) +
  labs(title = "x3: Disk I/O", x = "Time (seconds)", y = "Signal Value") +
  theme_publication()

p_ts_x4 <- ggplot(telemetry_data, aes(x = time, y = x4)) +
  geom_line(color = "#9467bd", linewidth = 0.8) +
  labs(title = "x4: Network Traffic", x = "Time (seconds)", y = "Signal Value") +
  theme_publication()

p_ts_y <- ggplot(telemetry_data, aes(x = time, y = y)) +
  geom_line(color = "#d62728", linewidth = 0.9) +
  labs(title = "y: Power Consumption", x = "Time (seconds)", y = "Power (Watts)") +
  theme_publication()

png(file.path(plots_dir, "Task1_Time_Series_Plots.png"), width = 1200, height = 1000, res = 120)
grid.arrange(p_ts_x1, p_ts_x2, p_ts_x3, p_ts_x4, p_ts_y, ncol = 2,
             top = grid::textGrob("Task 1.1: Time Series Plots of Inputs and Output", 
                                  gp = grid::gpar(fontsize = 15, fontface = "bold")))
dev.off()
cat("  Saved: plots/Task1_Time_Series_Plots.png\n")

# --- Task 1.2: Distribution Analysis and Summary Statistics ---
# Function to plot histogram with density curve and fitted normal distribution
make_dist_plot <- function(data, var_name, label_name, fill_color) {
  v <- data[[var_name]]
  m <- mean(v)
  s <- sd(v)
  
  ggplot(data, aes(x = .data[[var_name]])) +
    geom_histogram(aes(y = after_stat(density)), bins = 25, fill = fill_color, color = "white", alpha = 0.7) +
    geom_density(color = "black", linewidth = 1) +
    stat_function(fun = dnorm, args = list(mean = m, sd = s), color = "red", linetype = "dashed", linewidth = 0.9) +
    labs(title = label_name, x = "Value", y = "Density",
         subtitle = sprintf("Mean = %.3f | SD = %.3f", m, s)) +
    theme_publication()
}

p_dist_x1 <- make_dist_plot(telemetry_data, "x1", "x1: CPU Utilization", "#1f77b4")
p_dist_x2 <- make_dist_plot(telemetry_data, "x2", "x2: Memory Usage", "#2ca02c")
p_dist_x3 <- make_dist_plot(telemetry_data, "x3", "x3: Disk I/O", "#ff7f0e")
p_dist_x4 <- make_dist_plot(telemetry_data, "x4", "x4: Network Traffic", "#9467bd")
p_dist_y  <- make_dist_plot(telemetry_data, "y", "y: Power Consumption", "#d62728")

png(file.path(plots_dir, "Task1_Signal_Distributions.png"), width = 1200, height = 1000, res = 120)
grid.arrange(p_dist_x1, p_dist_x2, p_dist_x3, p_dist_x4, p_dist_y, ncol = 2,
             top = grid::textGrob("Task 1.2: Histograms with Fitted Normal Curve", 
                                  gp = grid::gpar(fontsize = 15, fontface = "bold")))
dev.off()
cat("  Saved: plots/Task1_Signal_Distributions.png\n")

# Compute summary statistics table
calculate_stats <- function(x) {
  c(
    N        = length(x),
    Mean     = round(mean(x), 4),
    StdDev   = round(sd(x), 4),
    Median   = round(median(x), 4),
    IQR      = round(IQR(x), 4),
    Min      = round(min(x), 4),
    Max      = round(max(x), 4),
    Skewness = round((mean((x - mean(x))^3)) / (sd(x)^3), 4),
    Kurtosis = round((mean((x - mean(x))^4)) / (sd(x)^4) - 3, 4)
  )
}

stats_mat <- sapply(telemetry_data[, c("x1", "x2", "x3", "x4", "y")], calculate_stats)
stats_df  <- as.data.frame(t(stats_mat))
stats_df  <- cbind(Signal = rownames(stats_df), stats_df)
write.csv(stats_df, file.path(output_dir, "Task1_Summary_Statistics.csv"), row.names = FALSE)
cat("  Saved: output/Task1_Summary_Statistics.csv\n")
print(stats_df)

# --- Task 1.3: Correlation Matrix and Scatter Plots ---
# Calculate Pearson correlation matrix
cor_matrix_pearson <- cor(telemetry_data[, c("x1", "x2", "x3", "x4", "y")], method = "pearson")

png(file.path(plots_dir, "Task1_Correlation_Heatmap.png"), width = 900, height = 800, res = 120)
par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))
corrplot(cor_matrix_pearson, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45, 
         col = colorRampPalette(c("#67a9cf", "#f7f7f7", "#ef8a62"))(200),
         title = "Task 1.3: Pearson Correlation Heatmap", mar = c(0, 0, 2, 0))
dev.off()
cat("  Saved: plots/Task1_Correlation_Heatmap.png\n")

# Scatter plots of each input against output y with linear and loess trendlines
make_scatter_plot <- function(data, x_col, y_col, x_label, y_label, color) {
  r_val <- cor(data[[x_col]], data[[y_col]])
  ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(color = color, alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", color = "blue", linetype = "dashed", se = FALSE) +
    geom_smooth(method = "loess", color = "darkred", linetype = "solid", se = TRUE, alpha = 0.2) +
    labs(title = sprintf("%s vs %s", y_label, x_label),
         subtitle = sprintf("Pearson r = %.4f", r_val),
         x = x_label, y = y_label) +
    theme_publication()
}

p_sc_x1 <- make_scatter_plot(telemetry_data, "x1", "y", "x1 (CPU)", "Power (y)", "#1f77b4")
p_sc_x2 <- make_scatter_plot(telemetry_data, "x2", "y", "x2 (Memory)", "Power (y)", "#2ca02c")
p_sc_x3 <- make_scatter_plot(telemetry_data, "x3", "y", "x3 (Disk I/O)", "Power (y)", "#ff7f0e")
p_sc_x4 <- make_scatter_plot(telemetry_data, "x4", "y", "x4 (Network)", "Power (y)", "#9467bd")

png(file.path(plots_dir, "Task1_Input_vs_Output_Scatter.png"), width = 1100, height = 900, res = 120)
grid.arrange(p_sc_x1, p_sc_x2, p_sc_x3, p_sc_x4, ncol = 2,
             top = grid::textGrob("Task 1.3: Scatter Plots of Inputs vs Power Consumption", 
                                  gp = grid::gpar(fontsize = 15, fontface = "bold")))
dev.off()
cat("  Saved: plots/Task1_Input_vs_Output_Scatter.png\n\n")

# ==============================================================================
# TASK 2: REGRESSION MODELING & MODEL SELECTION
# ==============================================================================
cat("--- Running Task 2: Regression Modeling & Model Selection ---\n")

# Extract input vectors and bias term
x1   <- telemetry_data$x1
x2   <- telemetry_data$x2
x3   <- telemetry_data$x3
x4   <- telemetry_data$x4
bias <- rep(1, N)

# Define design matrices (Phi) for the 5 candidate models from coursework brief:
# Model 1: y = theta1*x4 + theta2*x1^2 + theta3*x1^3 + theta4*x2^4 + theta5*x1^4 + bias
Phi_1 <- cbind(x4 = x4, x1_sq = x1^2, x1_cb = x1^3, x2_qd = x2^4, x1_qd = x1^4, bias = bias)

# Model 2: y = theta1*x4 + theta2*x1^3 + theta3*x3^4 + bias
Phi_2 <- cbind(x4 = x4, x1_cb = x1^3, x3_qd = x3^4, bias = bias)

# Model 3: y = theta1*x3^3 + theta2*x3^4 + bias
Phi_3 <- cbind(x3_cb = x3^3, x3_qd = x3^4, bias = bias)

# Model 4: y = theta1*x2 + theta2*x1^3 + theta3*x3^4 + bias
Phi_4 <- cbind(x2 = x2, x1_cb = x1^3, x3_qd = x3^4, bias = bias)

# Model 5: y = theta1*x4 + theta2*x1^2 + theta3*x1^3 + theta4*x3^4 + bias
Phi_5 <- cbind(x4 = x4, x1_sq = x1^2, x1_cb = x1^3, x3_qd = x3^4, bias = bias)

candidate_models <- list(
  "Model 1" = Phi_1,
  "Model 2" = Phi_2,
  "Model 3" = Phi_3,
  "Model 4" = Phi_4,
  "Model 5" = Phi_5
)

# --- Tasks 2.1 - 2.4: Fit Models via Least Squares, Compute RSS, Log-Likelihood, AIC & BIC ---
model_results <- list()
comparison_records <- data.frame(
  Model          = character(),
  Num_Params     = integer(),
  RSS            = numeric(),
  Sigma2         = numeric(),
  Log_Likelihood = numeric(),
  AIC            = numeric(),
  BIC            = numeric(),
  R_Squared      = numeric(),
  Adj_R_Squared  = numeric(),
  Shapiro_Wilk_p = numeric(),
  stringsAsFactors = FALSE
)

total_ss <- sum((y_vec - mean(y_vec))^2)

for (model_name in names(candidate_models)) {
  Phi <- candidate_models[[model_name]]
  k   <- ncol(Phi) # number of parameters including bias
  
  # Task 2.1: Least Squares estimate: theta_hat = (Phi' * Phi)^(-1) * Phi' * y
  theta_hat <- as.vector(solve(t(Phi) %*% Phi, t(Phi) %*% y_vec))
  names(theta_hat) <- colnames(Phi)
  
  # Fitted values and residuals
  y_pred    <- as.vector(Phi %*% theta_hat)
  residuals <- y_vec - y_pred
  
  # Task 2.2: Residual Sum of Squares (RSS)
  rss <- sum(residuals^2)
  
  # Task 2.3: Log-Likelihood (assuming zero-mean Gaussian noise: sigma^2 = RSS / N)
  sigma2_mle     <- rss / N
  log_likelihood <- -(N / 2) * log(2 * pi) - (N / 2) * log(sigma2_mle) - (rss / (2 * sigma2_mle))
  
  # Task 2.4: AIC and BIC
  aic <- 2 * k - 2 * log_likelihood
  bic <- k * log(N) - 2 * log_likelihood
  
  # Goodness of fit metrics (R-squared and Adjusted R-squared)
  r2     <- 1 - (rss / total_ss)
  adj_r2 <- 1 - ((rss / (N - k)) / (total_ss / (N - 1)))
  
  # Test normality of residuals
  shapiro_test <- shapiro.test(residuals)
  
  # Save results for this model
  model_results[[model_name]] <- list(
    Phi       = Phi,
    theta_hat = theta_hat,
    y_pred    = y_pred,
    residuals = residuals,
    RSS       = rss,
    sigma2    = sigma2_mle,
    LogLik    = log_likelihood,
    AIC       = aic,
    BIC       = bic,
    R2        = r2,
    Adj_R2    = adj_r2,
    Shapiro   = shapiro_test
  )
  
  comparison_records <- rbind(comparison_records, data.frame(
    Model          = model_name,
    Num_Params     = k,
    RSS            = round(rss, 4),
    Sigma2         = round(sigma2_mle, 6),
    Log_Likelihood = round(log_likelihood, 2),
    AIC            = round(aic, 2),
    BIC            = round(bic, 2),
    R_Squared      = round(r2, 4),
    Adj_R_Squared  = round(adj_r2, 4),
    Shapiro_Wilk_p = format.pval(shapiro_test$p.value, digits = 4, eps = 1e-4)
  ))
}

# Display and save model comparison table
cat("\nModel Comparison Table:\n")
print(comparison_records)
write.csv(comparison_records, file.path(output_dir, "Task2_Model_Comparison.csv"), row.names = FALSE)
cat("  Saved: output/Task2_Model_Comparison.csv\n")

# Display estimated parameters for each candidate model
cat("\nEstimated Parameters for Each Model:\n")
for (model_name in names(model_results)) {
  cat(sprintf("\n--- %s ---\n", model_name))
  th <- model_results[[model_name]]$theta_hat
  for (p in names(th)) {
    cat(sprintf("  %-10s = %10.5f\n", p, th[p]))
  }
}

# --- Task 2.5: Residual Diagnostics (Histograms and Q-Q Plots) ---
residual_plots_list <- list()
qq_plots_list       <- list()

for (model_name in names(candidate_models)) {
  res    <- model_results[[model_name]]$residuals
  res_df <- data.frame(Residuals = res)
  p_val  <- model_results[[model_name]]$Shapiro$p.value
  
  # Residual histogram with normal curve
  p_res <- ggplot(res_df, aes(x = Residuals)) +
    geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "#4575b4", color = "white", alpha = 0.7) +
    stat_function(fun = dnorm, args = list(mean = mean(res), sd = sd(res)), 
                  color = "red", linetype = "solid", linewidth = 0.9) +
    labs(title = sprintf("%s Residuals", model_name),
         subtitle = sprintf("Mean = %.2e | SD = %.3f | Shapiro p = %.4f", mean(res), sd(res), p_val),
         x = "Residual Error", y = "Density") +
    theme_publication(base_size = 10)
  
  residual_plots_list[[model_name]] <- p_res
  
  # Normal Q-Q plot
  p_qq <- ggplot(res_df, aes(sample = Residuals)) +
    stat_qq(color = "#313695", alpha = 0.7, size = 1.8) +
    stat_qq_line(color = "red", linetype = "dashed", linewidth = 0.9) +
    labs(title = sprintf("%s Normal Q-Q Plot", model_name),
         subtitle = sprintf("Shapiro-Wilk p = %.4f", p_val),
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_publication(base_size = 10)
  
  qq_plots_list[[model_name]] <- p_qq
}

png(file.path(plots_dir, "Task2_Residual_Distributions.png"), width = 1200, height = 1000, res = 120)
grid.arrange(grobs = residual_plots_list, ncol = 2,
             top = grid::textGrob("Task 2.5: Residual Error Distributions for Candidate Models", 
                                  gp = grid::gpar(fontsize = 15, fontface = "bold")))
dev.off()
cat("  Saved: plots/Task2_Residual_Distributions.png\n")

png(file.path(plots_dir, "Task2_QQ_Plots.png"), width = 1200, height = 1000, res = 120)
grid.arrange(grobs = qq_plots_list, ncol = 2,
             top = grid::textGrob("Task 2.5: Normal Q-Q Plots of Residuals", 
                                  gp = grid::gpar(fontsize = 15, fontface = "bold")))
dev.off()
cat("  Saved: plots/Task2_QQ_Plots.png\n")

# --- Task 2.6: Select the Best Model ---
# Model 2 is chosen as best model:
# 1. Lowest AIC (-334.65) and lowest BIC (-321.44).
# 2. Residuals follow a normal distribution (Shapiro-Wilk p = 0.9889 > 0.05).
# 3. Model 5 has nearly the same RSS, but has an unnecessary term (x1^2) with p > 0.8.
selected_model_name <- "Model 2"
cat(sprintf("\nSelected Best Model: %s\n", selected_model_name))

# Compute parameter significance table for Model 2
theta_m2           <- model_results[[selected_model_name]]$theta_hat
phi_m2             <- model_results[[selected_model_name]]$Phi
rss_m2             <- model_results[[selected_model_name]]$RSS
sigma2_m2_unbiased <- rss_m2 / (N - ncol(phi_m2))
cov_theta_m2       <- sigma2_m2_unbiased * solve(t(phi_m2) %*% phi_m2)
se_theta_m2        <- sqrt(diag(cov_theta_m2))
t_stats_m2         <- theta_m2 / se_theta_m2
p_values_m2        <- 2 * (1 - pt(abs(t_stats_m2), df = N - ncol(phi_m2)))

m2_param_table <- data.frame(
  Parameter = names(theta_m2),
  Estimate  = round(theta_m2, 5),
  Std_Error = round(se_theta_m2, 5),
  t_value   = round(t_stats_m2, 3),
  p_value   = format.pval(p_values_m2, digits = 4, eps = 1e-4)
)
cat("\nModel 2 Parameter Summary Table:\n")
print(m2_param_table)
write.csv(m2_param_table, file.path(output_dir, "Task2_Model2_Parameters.csv"), row.names = FALSE)
cat("  Saved: output/Task2_Model2_Parameters.csv\n")

# --- Task 2.7: Train/Test Split (70/30) & 95% Confidence Intervals ---
# Split data into 70% training and 30% testing
set.seed(2026)
train_prop    <- 0.70
train_size    <- round(train_prop * N)
test_size     <- N - train_size

train_indices <- sort(sample(1:N, size = train_size, replace = FALSE))
test_indices  <- setdiff(1:N, train_indices)

cat(sprintf("\nSplit: %d training samples (70%%) and %d testing samples (30%%)\n", train_size, test_size))

# Build training and testing sets for Model 2
Phi_train  <- Phi_2[train_indices, ]
y_train    <- y_vec[train_indices]
time_train <- time_vec[train_indices]

Phi_test   <- Phi_2[test_indices, ]
y_test     <- y_vec[test_indices]
time_test  <- time_vec[test_indices]

# 1. Fit parameters on training data only
theta_train <- as.vector(solve(t(Phi_train) %*% Phi_train, t(Phi_train) %*% y_train))
names(theta_train) <- colnames(Phi_2)

# Training residuals and error variance
y_train_pred <- as.vector(Phi_train %*% theta_train)
res_train    <- y_train - y_train_pred
rss_train    <- sum(res_train^2)
k_m2         <- length(theta_train)
df_train     <- train_size - k_m2
s2_train     <- rss_train / df_train
s_train      <- sqrt(s2_train)

# 2. Evaluate on unseen test data
y_test_pred <- as.vector(Phi_test %*% theta_train)
res_test    <- y_test - y_test_pred
rss_test    <- sum(res_test^2)
rmse_test   <- sqrt(mean(res_test^2))

cat(sprintf("Training RSS: %.4f | Training RMSE: %.4f\n", rss_train, sqrt(rss_train / train_size)))
cat(sprintf("Testing  RSS: %.4f | Testing  RMSE: %.4f\n", rss_test, rmse_test))

# 3. Calculate 95% Confidence Intervals for predictions
inv_PhiT_Phi_train <- solve(t(Phi_train) %*% Phi_train)
se_pred  <- sqrt(rowSums((Phi_test %*% inv_PhiT_Phi_train) * Phi_test)) * s_train
t_crit   <- qt(0.975, df = df_train)

ci_lower <- y_test_pred - t_crit * se_pred
ci_upper <- y_test_pred + t_crit * se_pred

# Calculate 95% Prediction Intervals (accounting for residual noise)
se_forecast <- sqrt(1 + rowSums((Phi_test %*% inv_PhiT_Phi_train) * Phi_test)) * s_train
pi_lower    <- y_test_pred - t_crit * se_forecast
pi_upper    <- y_test_pred + t_crit * se_forecast

test_eval_df <- data.frame(
  Test_Sample = 1:test_size,
  Time        = time_test,
  Observed_y  = y_test,
  Predicted_y = y_test_pred,
  SE_Mean     = se_pred,
  CI_Lower_95 = ci_lower,
  CI_Upper_95 = ci_upper,
  PI_Lower_95 = pi_lower,
  PI_Upper_95 = pi_upper
)
write.csv(test_eval_df, file.path(output_dir, "Task2_Train_Test_Evaluation.csv"), row.names = FALSE)
cat("  Saved: output/Task2_Train_Test_Evaluation.csv\n")

# Plot test predictions vs observed data with 95% CI error bars
test_plot_df <- test_eval_df[order(test_eval_df$Time), ]

p_ci <- ggplot(test_plot_df, aes(x = Time)) +
  geom_ribbon(aes(ymin = PI_Lower_95, ymax = PI_Upper_95), fill = "gray85", alpha = 0.5) +
  geom_ribbon(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), fill = "#9ecae1", alpha = 0.7) +
  geom_errorbar(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), width = 0.003, color = "#2171b5", linewidth = 0.6) +
  geom_line(aes(y = Predicted_y, color = "Model Prediction"), linewidth = 0.9) +
  geom_point(aes(y = Observed_y, color = "Test Observed Data"), size = 2.2, shape = 21, fill = "white", stroke = 1.2) +
  scale_color_manual(name = "Legend", values = c("Model Prediction" = "#08519c", "Test Observed Data" = "#e41a1c")) +
  labs(title = "Task 2.7: Model 2 Predictions on Unseen Test Data (30% Split)",
       subtitle = sprintf("95%% Confidence Intervals (Blue Bar) & 95%% Prediction Intervals (Gray Ribbon) | Test RMSE = %.4f", rmse_test),
       x = "Time (seconds)", y = "Power Consumption (y)") +
  theme_publication()

png(file.path(plots_dir, "Task2_Train_Test_Predictions_CI.png"), width = 1100, height = 700, res = 120)
print(p_ci)
dev.off()
cat("  Saved: plots/Task2_Train_Test_Predictions_CI.png\n")

# Multi-panel feature-level error bar plots against each input
test_plot_df$x1 <- telemetry_data$x1[test_indices]
test_plot_df$x2 <- telemetry_data$x2[test_indices]
test_plot_df$x3 <- telemetry_data$x3[test_indices]
test_plot_df$x4 <- telemetry_data$x4[test_indices]

make_feature_errorbar_plot <- function(df, feat_col, feat_name, col_hex) {
  ggplot(df, aes(x = .data[[feat_col]])) +
    geom_errorbar(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), width = 0.15, color = "gray50", linewidth = 0.6) +
    geom_point(aes(y = Predicted_y, color = "Predicted"), shape = 17, size = 2.4) +
    geom_point(aes(y = Observed_y, color = "Observed"), shape = 16, size = 2.0, alpha = 0.8) +
    scale_color_manual(name = "Data", values = c("Predicted" = col_hex, "Observed" = "#d95f02")) +
    labs(title = sprintf("Error Bar: Model 2 vs %s", feat_name),
         x = feat_name, y = "Power Consumption (y)") +
    theme_publication(base_size = 10)
}

p_eb_x1 <- make_feature_errorbar_plot(test_plot_df, "x1", "x1 (CPU Utilization)", "#1f77b4")
p_eb_x2 <- make_feature_errorbar_plot(test_plot_df, "x2", "x2 (Memory Usage)", "#2ca02c")
p_eb_x3 <- make_feature_errorbar_plot(test_plot_df, "x3", "x3 (Disk I/O)", "#ff7f0e")
p_eb_x4 <- make_feature_errorbar_plot(test_plot_df, "x4", "x4 (Network Traffic)", "#9467bd")

png(file.path(plots_dir, "Task2_Train_Test_ErrorBars_By_Feature.png"), width = 1200, height = 900, res = 120)
grid.arrange(p_eb_x1, p_eb_x2, p_eb_x3, p_eb_x4, ncol = 2,
             top = grid::textGrob("Task 2.7: 95% Confidence Interval Error Bars across Input Features", 
                                  gp = grid::gpar(fontsize = 15, fontface = "bold")))
dev.off()
cat("  Saved: plots/Task2_Train_Test_ErrorBars_By_Feature.png\n\n")

# ==============================================================================
# TASK 3: APPROXIMATE BAYESIAN COMPUTATION (ABC)
# ==============================================================================
cat("--- Running Task 3: Approximate Bayesian Computation (ABC) ---\n")

# --- Task 3.1: Select the 2 parameters with the largest absolute value ---
theta_full <- model_results[[selected_model_name]]$theta_hat
abs_theta  <- abs(theta_full)
sorted_idx <- order(abs_theta, decreasing = TRUE)

top_2_names  <- names(theta_full)[sorted_idx[1:2]]
top_2_values <- theta_full[top_2_names]

cat(sprintf("Top 2 parameters selected for ABC:\n"))
cat(sprintf("  1. %s = %.5f\n", top_2_names[1], top_2_values[1]))
cat(sprintf("  2. %s = %.5f\n", top_2_names[2], top_2_values[2]))

# Keep the remaining parameters fixed at their Task 2.1 Least Squares values
fixed_param_names <- setdiff(names(theta_full), top_2_names)
cat("\nFixed Parameters (held constant at LS estimates):\n")
for (fp in fixed_param_names) {
  cat(sprintf("  %-10s = %10.5f\n", fp, theta_full[fp]))
}

# --- Task 3.2: Define Uniform Priors around the estimated values ---
# Set prior ranges around each parameter (+/- 50% width)
delta_1 <- max(0.20, abs(top_2_values[1]) * 0.50)
delta_2 <- max(0.10, abs(top_2_values[2]) * 0.50)

prior_p1_min <- top_2_values[1] - delta_1
prior_p1_max <- top_2_values[1] + delta_1

prior_p2_min <- top_2_values[2] - delta_2
prior_p2_max <- top_2_values[2] + delta_2

cat(sprintf("\nPrior Distributions:\n"))
cat(sprintf("  Prior for %s: Uniform(min = %.4f, max = %.4f)\n", top_2_names[1], prior_p1_min, prior_p1_max))
cat(sprintf("  Prior for %s: Uniform(min = %.4f, max = %.4f)\n", top_2_names[2], prior_p2_min, prior_p2_max))

# --- Tasks 3.3 & 3.4: Rejection ABC Simulation ---
# Generate candidate samples from the uniform priors
M_samples <- 100000
set.seed(2026)

cat(sprintf("\nRunning ABC Rejection Sampling with M = %d samples...\n", M_samples))

p1_proposals <- runif(M_samples, min = prior_p1_min, max = prior_p1_max)
p2_proposals <- runif(M_samples, min = prior_p2_min, max = prior_p2_max)

# Compute predictions for all proposed candidate pairs
Phi_full <- candidate_models[[selected_model_name]]

if (length(fixed_param_names) > 0) {
  Phi_fixed         <- Phi_full[, fixed_param_names, drop = FALSE]
  y_fixed_component <- as.vector(Phi_fixed %*% theta_full[fixed_param_names])
} else {
  y_fixed_component <- rep(0, N)
}

y_target_remaining <- y_vec - y_fixed_component
Phi_free           <- Phi_full[, top_2_names, drop = FALSE]

# Vectorized simulation
proposal_matrix  <- rbind(p1_proposals, p2_proposals)
free_predictions <- Phi_free %*% proposal_matrix

# Calculate distance (RMSE) between simulated y and observed y
residuals_matrix <- y_target_remaining - free_predictions
rmse_distances   <- sqrt(colMeans(residuals_matrix^2))

# Accept the top 1% samples with the lowest distance
acceptance_rate   <- 0.01
epsilon_threshold <- quantile(rmse_distances, probs = acceptance_rate)
accepted_idx      <- which(rmse_distances <= epsilon_threshold)
n_accepted        <- length(accepted_idx)

cat(sprintf("Acceptance Threshold (epsilon): %.5f (Top %.1f%%)\n", epsilon_threshold, acceptance_rate * 100))
cat(sprintf("Accepted Posterior Samples:     %d / %d\n", n_accepted, M_samples))

# Extract accepted samples
p1_posterior <- p1_proposals[accepted_idx]
p2_posterior <- p2_proposals[accepted_idx]
post_dist    <- rmse_distances[accepted_idx]

abc_posterior_df <- data.frame(
  p1       = p1_posterior,
  p2       = p2_posterior,
  distance = post_dist
)
names(abc_posterior_df)[1:2] <- top_2_names

# --- Task 3.5: Posterior Summary Table & Distribution Plots ---
summarize_posterior <- function(post_samples, prior_min, prior_max, ls_val, param_name) {
  data.frame(
    Parameter   = param_name,
    LS_Estimate = round(ls_val, 5),
    Prior_Min   = round(prior_min, 4),
    Prior_Max   = round(prior_max, 4),
    Post_Mean   = round(mean(post_samples), 5),
    Post_Median = round(median(post_samples), 5),
    Post_StdDev = round(sd(post_samples), 5),
    CI_Lower_95 = round(quantile(post_samples, 0.025), 5),
    CI_Upper_95 = round(quantile(post_samples, 0.975), 5)
  )
}

abc_summary <- rbind(
  summarize_posterior(p1_posterior, prior_p1_min, prior_p1_max, top_2_values[1], top_2_names[1]),
  summarize_posterior(p2_posterior, prior_p2_min, prior_p2_max, top_2_values[2], top_2_names[2])
)

cat("\nABC Posterior Summary Table:\n")
print(abc_summary)
write.csv(abc_summary, file.path(output_dir, "Task3_ABC_Posterior_Summary.csv"), row.names = FALSE)
cat("  Saved: output/Task3_ABC_Posterior_Summary.csv\n")

# Joint posterior distribution plot
p_joint <- ggplot(abc_posterior_df, aes(x = .data[[top_2_names[1]]], y = .data[[top_2_names[2]]])) +
  geom_point(aes(color = distance), alpha = 0.5, size = 1.5) +
  geom_density_2d(color = "black", linewidth = 0.6) +
  annotate("point", x = as.numeric(top_2_values[1]), y = as.numeric(top_2_values[2]), 
           color = "red", shape = 3, size = 4, stroke = 2) +
  scale_color_viridis_c(option = "plasma", name = "Distance (RMSE)") +
  labs(title = sprintf("Task 3.4: Joint Posterior Distribution of (%s, %s)", top_2_names[1], top_2_names[2]),
       subtitle = sprintf("Accepted %d samples (tolerance eps = %.4f). Red cross = Least Squares estimate.", 
                          n_accepted, epsilon_threshold),
       x = sprintf("Parameter: %s", top_2_names[1]),
       y = sprintf("Parameter: %s", top_2_names[2])) +
  theme_publication()

png(file.path(plots_dir, "Task3_ABC_Joint_Posterior.png"), width = 900, height = 750, res = 120)
print(p_joint)
dev.off()
cat("  Saved: plots/Task3_ABC_Joint_Posterior.png\n")

# Marginal posterior distribution plots
p_marg_1 <- ggplot(abc_posterior_df, aes(x = .data[[top_2_names[1]]])) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#4393c3", color = "white", alpha = 0.7) +
  geom_density(color = "#053061", linewidth = 1) +
  geom_vline(xintercept = top_2_values[1], color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = c(abc_summary$CI_Lower_95[1], abc_summary$CI_Upper_95[1]), 
             color = "black", linetype = "dotted", linewidth = 0.8) +
  labs(title = sprintf("Marginal Posterior for %s", top_2_names[1]),
       subtitle = sprintf("Mean = %.4f | 95%% CI: [%.4f, %.4f]", 
                          abc_summary$Post_Mean[1], abc_summary$CI_Lower_95[1], abc_summary$CI_Upper_95[1]),
       x = top_2_names[1], y = "Posterior Density") +
  theme_publication()

p_marg_2 <- ggplot(abc_posterior_df, aes(x = .data[[top_2_names[2]]])) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#f4a582", color = "white", alpha = 0.7) +
  geom_density(color = "#67001f", linewidth = 1) +
  geom_vline(xintercept = top_2_values[2], color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = c(abc_summary$CI_Lower_95[2], abc_summary$CI_Upper_95[2]), 
             color = "black", linetype = "dotted", linewidth = 0.8) +
  labs(title = sprintf("Marginal Posterior for %s", top_2_names[2]),
       subtitle = sprintf("Mean = %.4f | 95%% CI: [%.4f, %.4f]", 
                          abc_summary$Post_Mean[2], abc_summary$CI_Lower_95[2], abc_summary$CI_Upper_95[2]),
       x = top_2_names[2], y = "Posterior Density") +
  theme_publication()

png(file.path(plots_dir, "Task3_ABC_Marginal_Posteriors.png"), width = 1100, height = 550, res = 120)
grid.arrange(p_marg_1, p_marg_2, ncol = 2,
             top = grid::textGrob("Task 3.5: Marginal Posterior Distributions with 95% Credible Intervals", 
                                  gp = grid::gpar(fontsize = 15, fontface = "bold")))
dev.off()
cat("  Saved: plots/Task3_ABC_Marginal_Posteriors.png\n\n")

cat("==============================================================================\n")
cat("Execution complete. All plots and output tables saved successfully.\n")
cat("==============================================================================\n")
