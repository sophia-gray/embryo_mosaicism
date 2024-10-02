# Install necessary packages if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")
if (!requireNamespace("stats", quietly = TRUE)) install.packages("stats")

# Load libraries
library(devtools)
library(tidyverse)
library(tidyr)
library(MASS)
library(stats)

# Install and load the tessera package from GitHub
devtools::install_github("bmskinner/tessera")
library(tessera)

# Define the makepops function to generate populations
makepops <- function(total_population_size = 11861, euploid = 0.5, aneuploid = 0.5,
                     mosaic_high = 0.25, mosaic_low = 0.25, n_cells = 5, n_chrs = 1, dispersal = 0.5) {
  
  # Define BIOPSY_SIZE based on n_cells
  BIOPSY_SIZE <- n_cells
  
  # Initialize population_id counter
  population_id_counter <- 1
  
  # Function to generate hardcoded populations
  generate_hardcoded_population <- function() {
    hardcoded_population <- data.frame(
      population_id = population_id_counter:(population_id_counter + 3),
      n.cells = rep(n_cells, 4),
      n.chrs = rep(n_chrs, 4),
      total_population_size = rep(total_population_size, 4),
      embryo_type = c("euploid", "aneuploid", "mixed", "mixed"),
      prop.euploid = c(1, 0, 0.5, 0.25),
      prop.aneuploid = c(0, 1, 0.5, 0.25),
      prop.mosaic_high = c(0, 0, 0, 0.25),
      prop.mosaic_low = c(0, 0, 0, 0.25),
      dispersal = rep(dispersal, 4)
    )
    population_id_counter <<- population_id_counter + 4
    return(hardcoded_population)
  }
  
  # Function to generate specific low-level mosaic populations with fixed aneuploidy rates
  generate_fixed_mosaic_populations <- function() {
    aneuploidy_rates <- c(0.3, 0.4, 0.5)
    fixed_mosaic_populations <- lapply(aneuploidy_rates, function(rate) {
      pop <- data.frame(
        population_id = population_id_counter,
        n.cells = n_cells,
        n.chrs = n_chrs,
        total_population_size = total_population_size,
        embryo_type = "mosaic_low",
        prop.euploid = 0,
        prop.aneuploid = rate,
        prop.mosaic_high = 0,
        prop.mosaic_low = 1,
        dispersal = dispersal
      )
      population_id_counter <<- population_id_counter + 1
      return(pop)
    })
    fixed_mosaic_populations <- do.call(rbind, fixed_mosaic_populations)
    return(fixed_mosaic_populations)
  }
  
  # Function to generate mixed populations
  generate_mixed_populations <- function() {
    mixed_populations <- data.frame(
      population_id = population_id_counter:(population_id_counter + 4),
      n.cells = rep(n_cells, 5),
      n.chrs = rep(n_chrs, 5),
      total_population_size = rep(total_population_size, 5),
      embryo_type = rep("mixed", 5),
      prop.euploid = seq(0.5, 0.1, by = -0.1),
      prop.aneuploid = rep(0.3, 5),
      prop.mosaic_high = rep(0.1, 5),
      prop.mosaic_low = seq(0.1, 0.5, by = 0.1),
      dispersal = rep(dispersal, 5)
    )
    population_id_counter <<- population_id_counter + 5
    return(mixed_populations)
  }
  
  combined_population <- rbind(
    generate_hardcoded_population(),
    generate_fixed_mosaic_populations(),
    generate_mixed_populations()
  )
  
  return(combined_population)
}

# Example of running the function to generate populations
populations_df <- makepops()
print("Generated Populations:")
print(populations_df)

# Check populations with mosaic high embryos
print("Populations with mosaic high embryos:")
print(populations_df %>% filter(prop.mosaic_high > 0))

# Function to create an embryo with specific properties
Embryo <- function(n.cells, n.chrs, prop.aneuploid, dispersal) {
  embryo <- list(
    n.cells = n.cells,
    n.chrs = n.chrs,
    prop.aneuploid = prop.aneuploid,
    dispersal = dispersal
  )
  return(embryo)
}

# Function to take a biopsy from an embryo
takeBiopsy <- function(embryo, biopsy.size, index.cell, chromosome) {
  biopsy <- sample(1:embryo$n.cells, biopsy.size, replace = TRUE)
  aneuploid_cells <- round(biopsy.size * embryo$prop.aneuploid)
  return(aneuploid_cells)
}

# Function to create a biopsy for each embryo in a population
create_biopsy <- function(n.cells, n.chrs, prop.aneuploid, dispersal, rng.seed) {
  set.seed(rng.seed)
  embryo <- Embryo(n.cells, n.chrs, prop.aneuploid, dispersal)
  biopsy <- takeBiopsy(embryo, biopsy.size = n.cells, index.cell = 1, chromosome = 0)
  return(biopsy)
}

# Loop through each population and create biopsy results for each embryo
biopsy_results <- do.call(rbind, lapply(1:nrow(populations_df), function(i) {
  replicate(populations_df$total_population_size[i], create_biopsy(
    populations_df$n.cells[i], populations_df$n.chrs[i], 
    populations_df$prop.aneuploid[i], populations_df$dispersal[i], 
    rng.seed = 42
  ), simplify = FALSE) %>%
    unlist() %>%
    data.frame(aneuploid_counts = ., population_id = populations_df$population_id[i])
}))

# Print biopsy results before classification
print("Biopsy Results Before Classification:")
print(head(biopsy_results))

# Combine biopsy results into a dataframe
combined_results <- data.frame(
  aneuploid_counts = biopsy_results$aneuploid_counts,
  population_id = biopsy_results$population_id
)

# Categorize biopsy results based on counts
combined_results <- combined_results %>%
  mutate(clinical_category = case_when(
    aneuploid_counts == 0 ~ "euploid",
    aneuploid_counts %in% c(1, 2) ~ "mosaic_low",
    aneuploid_counts %in% c(3, 4) ~ "mosaic_high",
    aneuploid_counts == 5 ~ "aneuploid",
    TRUE ~ NA_character_
  ))

# Print the combined results to check if categories are assigned correctly
print("Combined Results After Classification:")
print(head(combined_results))

# Clinical data counts
clinical_data <- data.frame(
  clinical_category = c("euploid", "mosaic_low", "mosaic_high", "aneuploid"),
  count = c(6512, 799, 462, 4088)
)

print("Clinical Data:")
print(clinical_data)

# Aggregate computed data
computed_aggregated <- combined_results %>%
  count(clinical_category) %>%
  rename(count = n)

print("Computed Aggregated Data:")
print(computed_aggregated)

# Perform chi-squared test
chi_test <- chisq.test(x = observed_counts, p = expected_counts / sum(expected_counts))

# Print the chi-squared test results
print("Chi-squared Test Results:")
print(chi_test)

# Extract and print the p-value
p_value <- chi_test$p.value
print(paste("P-value:", p_value))
