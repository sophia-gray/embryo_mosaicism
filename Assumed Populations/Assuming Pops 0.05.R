# Clinical data
clinical_data <- c(6512, 799, 499, 4088)

# Constants for mosaic high and aneuploid categories
constant_mosaic_high <- 499
constant_aneuploid <- 4088

# Calculate the total number of cases (excluding constants)
total_cases <- sum(clinical_data) - constant_mosaic_high - constant_aneuploid

# Initial simulated data
initial_simulated_data <- c(total_cases, 0, constant_mosaic_high, constant_aneuploid)

# Number of steps for 0.5% change
step_size <- 0.5
num_steps <- round(100 / step_size)

# Function to generate intermediate populations with smaller step sizes
generate_intermediate_populations <- function(total_cases, steps, constant_high, constant_aneu) {
  populations <- list()
  percentages <- numeric(steps + 1)
  
  for (i in 0:steps) {
    proportion_euploid <- (steps - i) / steps
    proportion_mosaic_low <- i / steps
    
    euploid <- round(total_cases * proportion_euploid)
    mosaic_low <- round(total_cases * proportion_mosaic_low)
    
    # Ensure the sum of the changing categories does not exceed the total_cases
    if (euploid + mosaic_low > total_cases) {
      mosaic_low <- total_cases - euploid
    }
    
    populations[[i + 1]] <- c(euploid, mosaic_low, constant_high, constant_aneu)
    percentages[i + 1] <- proportion_mosaic_low * 100
  }
  return(list(populations = populations, percentages = percentages))
}

# Generate populations from 100% euploid to 100% mosaic low with 0.5% steps
results <- generate_intermediate_populations(total_cases, num_steps, constant_mosaic_high, constant_aneuploid)
intermediate_populations <- results$populations
percentages <- results$percentages

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

# Print increments and p-values
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

# Define the p-value threshold
threshold <- 0.05

# Find the index where the p-value first exceeds the threshold
matching_index <- which(p_values > threshold)[1]

# Retrieve the corresponding percentage of Mosaic Low
matching_percentage <- percentages[matching_index]

# Print the result
print(paste("The populations start to meet the clinical distribution at Mosaic Low percentage:", matching_percentage, "%"))

# Define the p-value threshold
threshold <- 0.05

# Find the index where the p-value first exceeds the threshold
matching_index_start <- which(p_values > threshold)[1]

# Find the index where the p-value last exceeds the threshold before dropping off
matching_index_end <- tail(which(p_values > threshold), 1)

# Retrieve the corresponding percentage of Mosaic Low for the drop-off
drop_off_percentage <- percentages[matching_index_end]

# Print the result
print(paste("The populations drop back off from the clinical distribution at Mosaic Low percentage:", drop_off_percentage, "%"))

# Clinical categories
categories <- c("Euploid", "Mosaic Low", "Constant Mosaic High", "Constant Aneuploid")

# Identify indices for percentages in the 10% to 12% range
indexes_in_range <- which(percentages >= 10 & percentages <= 12)

# Extract corresponding populations
populations_in_range <- intermediate_populations[indexes_in_range]
percentages_in_range <- percentages[indexes_in_range]

# Create a data frame with the population details
pops1012_df <- data.frame(
  Percentage = percentages_in_range,
  Euploid = sapply(populations_in_range, function(x) x[1]),
  Mosaic_Low = sapply(populations_in_range, function(x) x[2]),
  Constant_Mosaic_High = sapply(populations_in_range, function(x) x[3]),
  Constant_Aneuploid = sapply(populations_in_range, function(x) x[4])
)

# Print the data frame
print(pops1012_df)
