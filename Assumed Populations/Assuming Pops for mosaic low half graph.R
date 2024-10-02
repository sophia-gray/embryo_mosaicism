# Clinical data
clinical_data <- c(6512, 799, 499, 4088)

# Constants for mosaic high and aneuploid categories
constant_mosaic_high <- 499
constant_aneuploid <- 4088

# Calculate the initial total for variable data
initial_total <- sum(clinical_data) - constant_mosaic_high - constant_aneuploid

# Initial simulated data
simulated_data <- c(0, 7273, constant_mosaic_high, constant_aneuploid)

# Number of steps
num_steps <- 200

# Function to generate intermediate populations
generate_intermediate_populations <- function(clinical, simulated, steps, constant_high, constant_aneu) {
  euploid_initial <- simulated[1]
  mosaic_low_initial <- simulated[2]
  euploid_final <- clinical[1]
  mosaic_low_final <- clinical[2]
  
  # Calculate the total initial and final sum for the changing categories
  total_initial <- euploid_initial + mosaic_low_initial
  total_final <- euploid_final + mosaic_low_final
  
  # Calculate the step size
  euploid_step <- (euploid_final - euploid_initial) / steps
  mosaic_low_step <- (mosaic_low_final - mosaic_low_initial) / steps
  
  populations <- list()
  for (i in 0:steps) {
    euploid <- round(euploid_initial + i * euploid_step)
    mosaic_low <- round(mosaic_low_initial + i * mosaic_low_step)
    
    # Ensure the sum of the changing categories remains constant
    if (euploid + mosaic_low > total_initial) {
      mosaic_low <- total_initial - euploid
    }
    
    populations[[i + 1]] <- c(euploid, mosaic_low, constant_high, constant_aneu)
  }
  return(populations)
}

# Generate populations
intermediate_populations <- generate_intermediate_populations(clinical_data, simulated_data, num_steps, constant_mosaic_high, constant_aneuploid)

# Function to run chi-squared tests
run_chi_squared_tests <- function(clinical, populations) {
  p_values <- numeric(length(populations))
  for (i in 1:length(populations)) {
    chi_vals <- matrix(c(populations[[i]], clinical), ncol = 4, byrow = TRUE)
    test_result <- chisq.test(chi_vals)
    p_values[i] <- test_result$p.value
  }
  return(p_values)
}

# Run chi-squared tests
p_values <- run_chi_squared_tests(clinical_data, intermediate_populations)

# Check increments
check_increments <- function(populations, steps) {
  increments <- numeric(steps)
  for (i in 1:(steps - 1)) {
    prev_population <- populations[[i]]
    next_population <- populations[[i + 1]]
    increments[i] <- (next_population[1] - prev_population[1]) / prev_population[1] * 100
  }
  return(increments)
}

# Get increments
increments <- check_increments(intermediate_populations, num_steps)

# Print increments
print(increments)
print(p_values)

# Find the largest p-value
max_p_value <- max(p_values, na.rm = TRUE)
print(paste("Maximum p-value:", max_p_value))

# Define the threshold for comparison (e.g., 0.5)
threshold <- 0.5

# Count how many p-values are greater than the threshold
large_p_values_count <- sum(p_values > threshold, na.rm = TRUE)

# Calculate the percentage of large p-values
percentage_large <- (large_p_values_count / length(p_values)) * 100
print(paste("Percentage of p-values above", threshold, ":", percentage_large, "%"))

# Plot p-values
plot(seq(0, length(p_values) - 1), p_values, type = "b", col = "blue",
     xlab = "Step", ylab = "P-value", main = "Chi-Squared Test P-values for Assumed Populations",
     ylim = c(0, 1))

# Log scale plot to handle very small p-values
plot(seq(0, length(p_values) - 1), -log10(p_values), type = "b", col = "red",
     xlab = "Step", ylab = "-Log10(P-value)", main = "Log-Scale of Chi-Squared Test P-values",
     ylim = c(0, max(-log10(p_values), na.rm = TRUE)))
