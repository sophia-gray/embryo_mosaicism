# Install and load required packages
if (!require("devtools")) install.packages("devtools")
if (!require("tessera")) devtools::install_github("bmskinner/tessera")
if (!require("dplyr")) install.packages("dplyr")

library(devtools)
library(tessera)
library(dplyr)

# Function to create a test mixed population
create_test_mixed_population <- function(total_population_size = 11860,
                                         num_euploid = 0,
                                         num_aneuploid_100 = 0,
                                         num_aneuploid_90 = 0,
                                         num_aneuploid_80 = 0,
                                         num_aneuploid_70 = 0,
                                         num_aneuploid_60 = 0,
                                         num_aneuploid_50 = 0,
                                         num_aneuploid_40 = 0,
                                         num_aneuploid_30 = 0,
                                         num_aneuploid_20 = 0,
                                         num_aneuploid_10 = 0,
                                         dispersal = 0.5) {
  # Ensure the total number of embryos does not exceed the total population size
  if (num_euploid + num_aneuploid_100 + num_aneuploid_90 + num_aneuploid_80 +
      num_aneuploid_70 + num_aneuploid_60 + num_aneuploid_50 + num_aneuploid_40 +
      num_aneuploid_30 + num_aneuploid_20 + num_aneuploid_10 > total_population_size) {
    stop("The sum of all embryo types exceeds the total population size.")
  }
  
  # Create data frame with different embryo types
  data.frame(
    n.cells = rep(200, total_population_size),
    n.chrs = rep(1, total_population_size),
    total_population_size = rep(total_population_size, total_population_size),
    prop.aneuploid = c(
      rep(1.00, num_aneuploid_100),
      rep(0.90, num_aneuploid_90),
      rep(0.80, num_aneuploid_80),
      rep(0.70, num_aneuploid_70),
      rep(0.60, num_aneuploid_60),
      rep(0.50, num_aneuploid_50),
      rep(0.40, num_aneuploid_40),
      rep(0.30, num_aneuploid_30),
      rep(0.20, num_aneuploid_20),
      rep(0.10, num_aneuploid_10),
      rep(0.00, num_euploid),
      rep(NA, total_population_size - (num_euploid + num_aneuploid_100 + num_aneuploid_90 + num_aneuploid_80 +
                                         num_aneuploid_70 + num_aneuploid_60 + num_aneuploid_50 + num_aneuploid_40 +
                                         num_aneuploid_30 + num_aneuploid_20 + num_aneuploid_10))  # Fill in with NA if any remaining slots
    ),
    dispersal = rep(dispersal, total_population_size),
    simulation_id = 1:total_population_size
  )
}

# Function to generate embryos with progress updates
make.embryos <- function(pops) {
  embryos_list <- list()
  
  for (i in 1:nrow(pops)) {
    pop <- pops[i, ]
    
    # Print progress for generating embryos every 100 embryos
    if (i %% 100 == 0) {
      cat("Generating embryo", i, "of", pop$total_population_size, "for population", pop$PopulationName, "\n")
    }
    
    embryos <- tessera::Embryo(
      n.cells = pop$n.cells,
      n.chrs = pop$n.chrs,
      prop.aneuploid = pop$prop.aneuploid,
      dispersal = pop$dispersal
    )
    
    embryos_list[[i]] <- embryos
  }
  
  return(embryos_list)
}

# Function to take biopsies from embryos with progress updates
biopsy.embryos <- function(embryos_list, biopsy_size = 5, index_cell = 1, chromosome = 0) {
  biopsies_list <- list()
  
  for (i in 1:length(embryos_list)) {
    # Print progress for taking biopsies every 100 embryos
    if (i %% 100 == 0) {
      cat("Taking biopsy for embryo", i, "of", length(embryos_list), "\n")
    }
    
    embryo <- embryos_list[[i]]
    biopsy <- tessera::takeBiopsy(embryo, biopsy.size = biopsy_size, index.cell = index_cell, chromosome = chromosome)
    biopsies_list[[i]] <- biopsy
  }
  
  return(biopsies_list)
}

# Function to classify biopsies based on the numeric vector structure
classify_biopsies <- function(biopsies_list) {
  classifications <- lapply(biopsies_list, function(pop_biopsies) {
    # Classify each biopsy in the population
    sapply(pop_biopsies, function(biopsy) {
      if (is.numeric(biopsy)) {
        result <- biopsy[1]  # Extract the single numeric value
        if (result == 0) {
          return("euploid")
        } else if (result %in% c(1, 2)) {
          return("mosaic_low")
        } else if (result %in% c(3, 4)) {
          return("mosaic_high")
        } else if (result == 5) {
          return("aneuploid")
        } else {
          return(NA)  # Handle unexpected values
        }
      } else {
        return(NA)  # Handle unexpected biopsy structure
      }
    })
  })
  
  # Flatten the list into a single vector of classifications
  unlist(classifications)
}

# Load the CSV file
populations_df <- read.csv("Boundary Pops 10 euploid.csv")

# Data frame to store the summary results
summary_results <- data.frame(
  PopulationID = integer(),
  Euploid_Count = integer(),
  Mosaic_Low_Count = integer(),
  Mosaic_High_Count = integer(),
  Aneuploid_Count = integer(),
  ChiSquaredStatistic = numeric(),
  ChiSquaredPValue = numeric()
)

# Reference data for the chi-squared test
reference_data <- c(
  euploid = 6512,
  mosaic_low = 799,
  mosaic_high = 462,
  aneuploid = 4088
)

# Loop over each population
for (i in 1:nrow(populations_df)) {
  population_row <- populations_df[i, ]
  
  # Extract the number of embryos for each type using column names
  nums <- population_row[, grepl("^num_", names(population_row))]
  
  # Convert nums to a named list to pass to the function
  nums_list <- as.list(nums)
  names(nums_list) <- names(nums)
  
  # Print population ID for tracking
  cat("Generating embryos for Population", population_row$PopulationID, "\n")
  
  # Generate the test population with specific numbers of embryos
  test_population <- do.call(create_test_mixed_population, c(list(total_population_size = 11860), nums_list))
  
  # Print population ID for tracking
  cat("Generated embryos for Population", population_row$PopulationID, "\n")
  
  # Save the generated population to an RDS file
  saveRDS(test_population, paste0("Population_", population_row$PopulationID, "_population.RDS"))
  
  # Generate embryos for the test population
  test_embryos <- make.embryos(test_population)
  
  # Print population ID for tracking
  cat("Performed biopsies for Population", population_row$PopulationID, "\n")
  
  # Perform biopsies on the generated embryos
  test_biopsies <- biopsy.embryos(test_embryos, biopsy_size = 5)
  
  # Classify the biopsies
  classified_biopsies <- classify_biopsies(test_biopsies)
  
  # Ensure all categories are present in the observed counts
  all_categories <- names(reference_data)
  category_counts <- table(factor(classified_biopsies, levels = all_categories))
  
  # Convert category counts to a numeric vector
  observed_counts <- as.numeric(category_counts)
  names(observed_counts) <- all_categories
  
  # Ensure the expected counts are in the same order
  expected_counts <- as.numeric(reference_data[all_categories])
  
  # Create the matrix of observed and expected counts
  chi_vals <- matrix(c(observed_counts, expected_counts), ncol = 4, byrow = TRUE)
  
  # Perform the chi-squared test
  chi_squared_result <- chisq.test(chi_vals)
  
  # Create a readable list of results
  population_results <- list(
    PopulationID = population_row$PopulationID,
    ClassificationCounts = as.list(category_counts),
    ChiSquaredStatistic = chi_squared_result$statistic,
    ChiSquaredPValue = chi_squared_result$p.value
  )
  
  # Save the results to an RDS file
  saveRDS(population_results, paste0("Population_", population_row$PopulationID, "_results.RDS"))
  
  # Print the summary results
  print(population_results)
  
  # Add summary results to the data frame
  summary_results <- rbind(
    summary_results,
    data.frame(
      PopulationID = population_row$PopulationID,
      Euploid_Count = category_counts["euploid"],
      Mosaic_Low_Count = category_counts["mosaic_low"],
      Mosaic_High_Count = category_counts["mosaic_high"],
      Aneuploid_Count = category_counts["aneuploid"],
      ChiSquaredStatistic = chi_squared_result$statistic,
      ChiSquaredPValue = chi_squared_result$p.value
    )
  )
}

# Save the summary results to a CSV file
write.csv(summary_results, "summary_results.csv", row.names = FALSE)

# Print a message when all populations have been processed
cat("All populations processed and summary results saved.\n")
