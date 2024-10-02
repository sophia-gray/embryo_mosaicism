# Install and load required packages
if (!require("devtools")) install.packages("devtools")
if (!require("tessera")) devtools::install_github("bmskinner/tessera")
if (!require("dplyr")) install.packages("dplyr")

library(devtools)
library(tessera)
library(dplyr)

# Function to create a test mixed population
create_test_mixed_population <- function(total_population_size = 11860,
                                         prop_euploid = 0,
                                         prop_aneuploid_100 = 0,
                                         prop_aneuploid_90 = 0,
                                         prop_aneuploid_80 = 0,
                                         prop_aneuploid_70 = 0,
                                         prop_aneuploid_60 = 0,
                                         prop_aneuploid_50 = 0,
                                         prop_aneuploid_40 = 0,
                                         prop_aneuploid_30 = 0,
                                         prop_aneuploid_20 = 0,
                                         prop_aneuploid_10 = 0,
                                         dispersal = 0.5) {
  # Calculate the number of embryos per type based on proportions
  embryos_per_type <- c(
    euploid = total_population_size * prop_euploid,
    aneuploid_100 = total_population_size * prop_aneuploid_100,
    aneuploid_90 = total_population_size * prop_aneuploid_90,
    aneuploid_80 = total_population_size * prop_aneuploid_80,
    aneuploid_70 = total_population_size * prop_aneuploid_70,
    aneuploid_60 = total_population_size * prop_aneuploid_60,
    aneuploid_50 = total_population_size * prop_aneuploid_50,
    aneuploid_40 = total_population_size * prop_aneuploid_40,
    aneuploid_30 = total_population_size * prop_aneuploid_30,
    aneuploid_20 = total_population_size * prop_aneuploid_20,
    aneuploid_10 = total_population_size * prop_aneuploid_10
  )
  
  # Create data frame with different embryo types
  data.frame(
    n.cells = rep(200, total_population_size),
    n.chrs = rep(1, total_population_size),
    total_population_size = rep(total_population_size, total_population_size),
    prop.aneuploid = c(
      rep(1.00, embryos_per_type["aneuploid_100"]),
      rep(0.90, embryos_per_type["aneuploid_90"]),
      rep(0.80, embryos_per_type["aneuploid_80"]),
      rep(0.70, embryos_per_type["aneuploid_70"]),
      rep(0.60, embryos_per_type["aneuploid_60"]),
      rep(0.50, embryos_per_type["aneuploid_50"]),
      rep(0.40, embryos_per_type["aneuploid_40"]),
      rep(0.30, embryos_per_type["aneuploid_30"]),
      rep(0.20, embryos_per_type["aneuploid_20"]),
      rep(0.10, embryos_per_type["aneuploid_10"]),
      rep(0.00, embryos_per_type["euploid"])
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

# Function to perform Pearson's chi-squared test
perform_chi_squared_test_direct <- function(expected_counts, observed_counts) {
  # Ensure the length of counts matches
  if (length(expected_counts) != length(observed_counts)) {
    stop("The length of expected_counts must be equal to the length of observed_counts.")
  }
  
  # Calculate expected proportions from the expected counts
  expected_proportions <- expected_counts / sum(expected_counts)
  
  # Perform the chi-squared test for given proportions
  test_result <- chisq.test(x = observed_counts, p = expected_proportions)
  
  return(test_result)
}

# Load the CSV file
populations_df <- read.csv("populations_df.csv")

# Data frame to store the summary results
summary_results <- data.frame(
  PopulationID = integer(),
  Euploid_Count = integer(),
  Mosaic_Low_Count = integer(),
  Mosaic_High_Count = integer(),
  Aneuploid_Count = integer(),
  P_Value = numeric()
)

# Loop over each population
for (i in 1:nrow(populations_df)) {
  population_row <- populations_df[i, ]
  
  # Extract proportions using column names
  props <- population_row[, grepl("^prop_", names(population_row))]
  
  # Convert props to a named list to pass to the function
  props_list <- as.list(props)
  names(props_list) <- names(props)
  
  # Print population ID for tracking
  cat("Generating embryos for Population", population_row$PopulationID, "\n")
  
  # Generate the test population with specific proportions
  test_population <- do.call(create_test_mixed_population, c(list(total_population_size = 11860), props_list))
  
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
  
  # Define reference data for the chi-squared test
  reference_data <- c(
    euploid = 6512,
    mosaic_low = 799,
    mosaic_high = 462,
    aneuploid = 4088
  )
  
  # Ensure all categories are present in the observed counts
  all_categories <- names(reference_data)
  category_counts <- table(factor(classified_biopsies, levels = all_categories))
  
  # Print counts for verification
  cat("Counts of biopsies by category for Population", population_row$PopulationID, "\n")
  print(category_counts)
  
  # Ensure the observed and expected counts have the same categories
  observed_counts <- as.numeric(category_counts)
  expected_counts <- as.numeric(reference_data[all_categories])
  
  # Print observed and expected counts for verification
  cat("Observed counts for Population", population_row$PopulationID, ":", observed_counts, "\n")
  cat("Expected counts for Population", population_row$PopulationID, ":", expected_counts, "\n")
  
  # Perform chi-squared test
  chi_squared_result <- perform_chi_squared_test_direct(expected_counts, observed_counts)
  
  # Store results in a list
  population_results <- list(
    PopulationID = population_row$PopulationID,
    classification = classified_biopsies,
    category_counts = category_counts,  # Add the counts to results
    chi_squared_result = chi_squared_result
  )
  
  # Save the results to an RDS file
  saveRDS(population_results, paste0("Population_", population_row$PopulationID, "_results.RDS"))
  
  # Append the results to the summary data frame
  summary_results <- rbind(summary_results, data.frame(
    PopulationID = population_row$PopulationID,
    Euploid_Count = category_counts["euploid"],
    Mosaic_Low_Count = category_counts["mosaic_low"],
    Mosaic_High_Count = category_counts["mosaic_high"],
    Aneuploid_Count = category_counts["aneuploid"],
    P_Value = chi_squared_result$p.value
  ))
  
  # Print results for this population
  cat("Results for Population", population_row$PopulationID, "\n")
  print(population_results)
}

# Save the summary results to a CSV file
write.csv(summary_results, "summary_results.csv", row.names = FALSE)

# Print final summary results for verification
cat("Final Summary Results:\n")
print(summary_results)

# Print final message
cat("Results saved to individual RDS files and summary_results.csv")
