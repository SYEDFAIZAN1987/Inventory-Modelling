#---------------------------------------------------------#
# Syed Faizan - Inventory Modelling                       #
#                                                         #
#                                                         #
#                                                         #
#                                                         #
#                                                         #
#---------------------------------------------------------#

#Starting with a clean environment----

rm(list=ls())


# Clearing the Console
cat("\014")    # Clears the console

# Clearing scientific notation

options(scipen = 999)

# Part 1 
library(ggplot2)
library(dplyr)

# Defining the parameters
annual_demand <- 15000
unit_cost <- 80
holding_cost_rate <- 0.18
order_cost <- 220
holding_cost <- unit_cost * holding_cost_rate

# Mathematical formulation of functions
EOQ <- round(sqrt((2 * annual_demand * order_cost) / holding_cost))
N_order <- annual_demand / EOQ

AOC <- (annual_demand / EOQ) * order_cost
AHC <- (EOQ / 2) * holding_cost
ATC <- AOC + AHC

# Print initial results
cat("EOQ:", EOQ, "\n")
cat("Annual Ordering Cost:", AOC, "\n")
cat("Annual Holding Cost:", AHC, "\n")
cat("Annual Total Cost:", ATC, "\n")

# Define functions for graphing
holding_cost <- function(Q) {
  return((Q / 2) * H)
}
ordering_cost <- function(Q) {
  return((D / Q) * S)
}
total_cost <- function(Q) {
  return(holding_cost(Q) + ordering_cost(Q))
}

# Visualize Optimal Order Quantity
Q_values <- seq(100, 3000, by=10)
total_costs <- sapply(Q_values, total_cost)
optimal_Q <- Q_values[which.min(total_costs)]

#Plot Total Cost vs Order Quantity
df <- data.frame(OrderQuantity = Q_values, TotalCost = total_costs)
ggplot(df, aes(x = OrderQuantity, y = TotalCost)) +
  geom_line() +
  labs(title = "Total Cost vs Order Quantity",
       x = "Order Quantity",
       y = "Total Cost") +
  geom_vline(xintercept = EOQ, linetype="dashed", color = "red") +
  annotate("text", x = EOQ, y = min(total_costs), label = paste("Optimal Q =", EOQ), vjust = -1)


#Part 2
# Load necessary libraries

library(triangle)
library(ggplot2)
library(MASS)
library(ggthemes)
library(gridExtra)

# Define constants and parameters
annual_demand <- 15000
unit_cost <- 80
holding_cost_rate <- 0.18
order_cost <- 220
holding_cost <- unit_cost * holding_cost_rate

# Calculate EOQ and costs from Part I
EOQ <- round(sqrt((2 * annual_demand * order_cost) / holding_cost))
N_order <- annual_demand / EOQ

AOC <- (annual_demand / EOQ) * order_cost
AHC <- (EOQ / 2) * holding_cost
ATC <- AOC + AHC

# Print initial results
cat("EOQ:", EOQ, "\n")
cat("Annual Ordering Cost:", AOC, "\n")
cat("Annual Holding Cost:", AHC, "\n")
cat("Annual Total Cost:", ATC, "\n")

# set seed for reproducibility
set.seed(314)
# Define the triangular distribution parameters for demand
demand_min <- 13000
demand_max <- 17000
demand_mode <- 15000

# Define other constants
holding_cost <- 14.4
order_cost <- 220

# Define function to calculate total inventory cost
total_cost <- function(demand, order_qty) {
  AHC <- (order_qty / 2) * holding_cost
  AOC <- (demand / order_qty) * order_cost
  total_cost <- AHC + AOC
  return(total_cost)
}

# Simulate 1000 occurrences
simulations <- 1000
min_cost <- numeric(simulations)
order_qty <- numeric(simulations)
n_order <- numeric(simulations)
demand_values <- numeric(simulations)

for (i in 1:simulations) {
  demand <- rtriangle(n = 1, a = demand_min, b = demand_max, c = demand_mode)
  demand_values[i] <- demand
  order_qty[i] <- optimize(f = total_cost, interval = c(1, demand), demand = demand)$minimum
  min_cost[i] <- total_cost(demand, order_qty[i])
  n_order[i] <- demand / order_qty[i]
}

# Create a data frame with all simulation results
simulation_results <- data.frame(
  Simulation = 1:simulations,
  Demand = demand_values,
  OrderQuantity = order_qty,
  NumberOfOrders = n_order,
  TotalCost = min_cost
)

# Display the first few rows of the table
head(simulation_results)

# Save the table to a CSV file
write.csv(simulation_results, "simulation_results.csv", row.names = FALSE)

# Print the summary of the total costs
summary(simulation_results$TotalCost)

# Estimate the mean minimum cost
Estimate_min_cost <- mean(min_cost)
Estimate_min_cost

# Create a histogram to visualize the distribution of minimum total costs
ggplot(simulation_results, aes(x = TotalCost)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Minimum Total Costs", x = "Total Cost", y = "Frequency")

# Create a summary table
summary_values <- as.numeric(summary(min_cost))
summary_table <- data.frame(
  Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
  Value = round(summary_values, 2)
)

print(summary_table)

# Save the summary table as an image
png("summary_table.png")
grid.table(summary_table)
dev.off()

# Display the full table using the View function (only works in interactive R sessions)
View(simulation_results)

# Minimum Total Cost Analysis

# Calculate statistics and confidence intervals
Estimate_min_cost <- mean(min_cost)
t_value <- qt(p = 0.975, df = simulations - 1)
standard_error <- sd(min_cost) / sqrt(simulations)
lower <- Estimate_min_cost - t_value * standard_error
upper <- Estimate_min_cost + t_value * standard_error

cat("Minimum total cost:", Estimate_min_cost, "\n")
cat("95% confidence interval for expected minimum total cost: (", lower, ", ", upper, ")", "\n")

# Plot histogram and density for minimum total cost
histogram_mc <- ggplot(data = data.frame(min_cost), aes(x = min_cost)) + 
  geom_histogram(aes(y = ..density..), binwidth = 50, fill = "red", colour = "black") +
  xlab("Minimum Total Cost") + ylab("Density") +
  geom_density(alpha = .3, fill = "lightyellow") +
  ggtitle("Distribution of Minimum Total Cost") +
  theme_minimal()
print(histogram_mc)

# QQ plot for normality check of minimum total cost
regulate <- function(x) {
  return((x - mean(x)) / sd(x))
}
regulate_min_cost <- regulate(min_cost)
qqnorm(regulate_min_cost, main = "Probability Distribution of Minimum Total Cost")
abline(0, 1, lwd = 2, col = "red")

# Testing probability distributions fit for Minimum Total Cost



library(MASS)
library(triangle)
library(ggplot2)
library(ggthemes)

# Fit Gamma distribution
fit_gamma <- fitdistr(min_cost, "gamma")
gamma_params <- fit_gamma$estimate


# Fit Normal distribution
fit_normal <- fitdistr(min_cost, "normal")
normal_params <- fit_normal$estimate

# Define Triangular PDF
triangular_pdf <- function(x, min, mode, max) {
  ifelse(x < min, 0,
         ifelse(x < mode, 2 * (x - min) / ((max - min) * (mode - min)),
                ifelse(x < max, 2 * (max - x) / ((max - min) * (max - mode)), 0)))
}

# Define wrapper function for triangular CDF
triangular_cdf <- function(x, min, mode, max) {
  ifelse(x < min, 0,
         ifelse(x < mode, (x - min)^2 / ((max - min) * (mode - min)),
                ifelse(x < max, 1 - (max - x)^2 / ((max - min) * (max - mode)), 1)))
}

# Parameters for Triangular distribution
tri_params <- list(min = 9000, mode = 9500, max = 11000)

# Plot Gamma distribution
ggplot(data = data.frame(min_cost), aes(x = min_cost)) +
  geom_histogram(aes(y = ..density..), binwidth = 50, fill = "red", color = "black") +
  stat_function(fun = dgamma, args = list(shape = gamma_params[1], rate = gamma_params[2]), color = "purple", size = 1) +
  ggtitle("Gamma Distribution for Minimum Total Cost") +
  xlab("Minimum Total Cost") +
  ylab("Probability Density") +
  theme_few()

# Plot Normal distribution
ggplot(data = data.frame(min_cost), aes(x = min_cost)) +
  geom_histogram(aes(y = ..density..), binwidth = 50, fill = "green", color = "black") +
  stat_function(fun = dnorm, args = list(mean = normal_params[1], sd = normal_params[2]), color = "purple", size = 1) +
  ggtitle("Normal Distribution for Minimum Total Cost") +
  xlab("Minimum Total Cost") +
  ylab("Probability Density") +
  theme_few()

# Plot Triangular distribution
ggplot(data = data.frame(min_cost), aes(x = min_cost)) +
  geom_histogram(aes(y = ..density..), binwidth = 50, fill = "blue", color = "black") +
  stat_function(fun = triangular_pdf, args = tri_params, color = "purple", size = 1) +
  ggtitle("Triangular Distribution for Minimum Total Cost") +
  xlab("Minimum Total Cost") +
  ylab("Probability Density") +
  theme_few()

# KS test for Gamma distribution
ks_gamma <- ks.test(min_cost, "pgamma", shape = gamma_params[1], rate = gamma_params[2])

# KS test for Normal distribution
ks_normal <- ks.test(min_cost, "pnorm", mean = normal_params[1], sd = normal_params[2])

# KS test for Triangular distribution
ks_tri <- ks.test(min_cost, triangular_cdf, min = tri_params$min, mode = tri_params$mode, max = tri_params$max)

# Print KS test results
ks_gamma
ks_normal
ks_tri

#Shapiro Wilk for normality
shapiro.test(min_cost)

# compare AIC for Gamma and Normal as KS and Shapiro test results conflict

AIC(fit_gamma)
AIC(fit_normal)

# Expected Order Quantity

# Confidence interval for expected order quantity
t_value <- qt(p = 0.975, df = simulations - 1)
standard_error <- sd(order_qty) / sqrt(simulations)
lower_oq <- mean(order_qty) - t_value * standard_error
upper_oq <- mean(order_qty) + t_value * standard_error

cat("95% confidence interval for expected order quantity: (", lower_oq, ", ", upper_oq, ")", "\n")
Estimate_order_qty <- mean(order_qty)
cat("Estimate order quantity:", Estimate_order_qty, "\n")



# Plot histogram and density for order quantity
histogram_oq <- ggplot(data = data.frame(order_qty), aes(x = order_qty)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "red", colour = "black") +
  xlab("Order Quantity") + ylab("Density") +
  geom_density(alpha = .2, fill = "lightyellow") +
  ggtitle("Distribution of Order Quantity") +
  theme_minimal()
print(histogram_oq)

# QQ plot for normality check of order quantity
regulate_order_qty <- regulate(order_qty)
qqnorm(regulate_order_qty, main = "P.D.F of Order Quantity")
abline(0, 1, lwd = 2, col = "red")

# Fit distributions
fit_gamma_oq <- fitdistr(order_qty, "gamma")
gamma_params_oq <- fit_gamma_oq$estimate

fit_normal_oq <- fitdistr(order_qty, "normal")
normal_params_oq <- fit_normal_oq$estimate


# Plot Gamma distribution
ggplot(data = data.frame(order_qty), aes(x = order_qty)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "red", color = "black") +
  stat_function(fun = dgamma, args = list(shape = gamma_params_oq[1], rate = gamma_params_oq[2]), color = "purple", size = 1) +
  ggtitle("Gamma Distribution for Order Quantity") +
  xlab("Order Quantity") +
  ylab("Probability Density") +
  theme_few()

# Plot Normal distribution
ggplot(data = data.frame(order_qty), aes(x = order_qty)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "green", color = "black") +
  stat_function(fun = dnorm, args = list(mean = normal_params_oq[1], sd = normal_params_oq[2]), color = "purple", size = 1) +
  ggtitle("Normal Distribution for Order Quantity") +
  xlab("Order Quantity") +
  ylab("Probability Density") +
  theme_few()

# KS test for Gamma distribution
ks_gamma_oq <- ks.test(order_qty, "pgamma", shape = gamma_params_oq[1], rate = gamma_params_oq[2])

# KS test for Normal distribution
ks_normal_oq <- ks.test(order_qty, "pnorm", mean = normal_params_oq[1], sd = normal_params_oq[2])

# Print KS test results
ks_gamma_oq
ks_normal_oq

#Shapiro Wilk 

shapiro.test(order_qty)


#Comparing AIC 
AIC(fit_gamma_oq)
AIC(fit_normal_oq)

#expected annual number of orders

# Confidence interval for expected annual number of orders
standard_error <- sd(n_order) / sqrt(simulations)
lower_norder <- mean(n_order) - t_value * standard_error
upper_norder <- mean(n_order) + t_value * standard_error
cat("95% confidence interval for expected Annual Number of Orders: (", lower_norder, ", ", upper_norder, ")", "\n")

Estimate_n_order <- mean(n_order)
cat("Estimate Annual Order Number:", Estimate_n_order, "\n")


# Plot histogram and density for annual number of orders
histogram_NO <- ggplot(data = data.frame(n_order), aes(x = n_order)) + 
  geom_histogram(aes(y = ..density..), binwidth = .1, fill = "red", colour = "black") +
  xlab("Annual Number of Orders") + ylab("Density") +
  geom_density(alpha = .2, fill = "lightyellow") +
  ggtitle("Distribution of Annual Number of Orders") +
  theme_minimal()
print(histogram_NO)

# QQ plot for normality check of annual number of orders
regulate_n_order <- regulate(n_order)
qqnorm(regulate_n_order, main = "P.D.F of Annual Number of Orders")
abline(0, 1, lwd = 2, col = "red")


# Fit distributions
fit_gamma_no <- fitdistr(n_order, "gamma")
gamma_params_no <- fit_gamma_no$estimate

fit_normal_no <- fitdistr(n_order, "normal")
normal_params_no <- fit_normal_no$estimate

# Plot Gamma distribution
ggplot(data = data.frame(n_order), aes(x = n_order)) +
  geom_histogram(aes(y = ..density..), binwidth = .1, fill = "red", color = "black") +
  stat_function(fun = dgamma, args = list(shape = gamma_params_no[1], rate = gamma_params_no[2]), color = "purple", size = 1) +
  ggtitle("Gamma Distribution for Number of Orders") +
  xlab("Number of Orders") +
  ylab("Probability Density") +
  theme_few()

# Plot Normal distribution
ggplot(data = data.frame(n_order), aes(x = n_order)) +
  geom_histogram(aes(y = ..density..), binwidth = .1, fill = "green", color = "black") +
  stat_function(fun = dnorm, args = list(mean = normal_params_no[1], sd = normal_params_no[2]), color = "purple", size = 1) +
  ggtitle("Normal Distribution for Number of Orders") +
  xlab("Number of Orders") +
  ylab("Probability Density") +
  theme_few()

# KS test for Gamma distribution
ks_gamma_no <- ks.test(n_order, "pgamma", shape = gamma_params_no[1], rate = gamma_params_no[2])

# KS test for Normal distribution
ks_normal_no <- ks.test(n_order, "pnorm", mean = normal_params_no[1], sd = normal_params_no[2])

# Print KS test results
ks_gamma_no
ks_normal_no

#Shapiro Wilk test

shapiro.test(n_order)

#AIC Comparison
AIC(fit_gamma_no)
AIC(fit_normal_no)

#END OF R ANALYSIS
