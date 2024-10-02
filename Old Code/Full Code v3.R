# Installing devtools to be able to access tessera.
install.packages("devtools")

# Running the install_github command to access tessera.
# Note: Packages may upgrade. Enter '1' if asked to upgrade to current packages.  
devtools::install_github("bmskinner/tessera")

install.packages("tidyverse")
library(tidyverse)

# Loading tessera for use. 
library(tessera)

makepops <- function(total_population_size = 11861,
                     euploid = 0.5, aneuploid = 0.5,
                     mosaic_high = 0.25, mosaic_low = 0.25,
                     n_cells = 5, n_chrs = 1, dispersal = 0.5) {
  
  # Define BIOPSY_SIZE based on n_cells
  BIOPSY_SIZE <- n_cells
  
  # Initialise population_id counter
  population_id_counter <- 1
  
  # Function to generate hardcoded populations
  generate_hardcoded_population <- function() {
    # Create a dataframe for hardcoded populations
    hardcoded_population <- data.frame(
      population_id = population_id_counter:(population_id_counter + 3),  # Assign a range of population_ids
      n.cells = rep(n_cells, 4),  # Number of cells in each population
      n.chrs = rep(n_chrs, 4),  # Number of chromosomes in each population
      total_population_size = rep(total_population_size, 4),  # Total population size for each
      embryo_type = c("euploid", "aneuploid", "mixed", "mixed"),  # Embryo types
      prop.euploid = c(1, 0, 0.5, 0.25),  # Proportions of euploid cells
      prop.aneuploid = c(0, 1, 0.5, 0.25),  # Proportions of aneuploid cells
      prop.mosaic_high = c(0, 0, 0, 0.25),  # Proportions of high-level mosaic cells
      prop.mosaic_low = c(0, 0, 0, 0.25),  # Proportions of low-level mosaic cells
      dispersal = rep(dispersal, 4)  # Dispersal rates
    )
    
    # Increment the population_id_counter for the next set of populations
    population_id_counter <<- population_id_counter + 4
    
    return(hardcoded_population)
  }
  
  # Function to generate specific low-level mosaic populations with fixed aneuploidy rates
  generate_fixed_mosaic_populations <- function() {
    aneuploidy_rates <- c(0.3, 0.4, 0.5)  # Defined aneuploidy rates
    
    # Generate a dataframe for each aneuploidy rate
    fixed_mosaic_populations <- lapply(aneuploidy_rates, function(rate) {
      pop <- data.frame(
        population_id = population_id_counter,  # Assign current population_id
        n.cells = n_cells,  # Number of cells in the biopsy
        n.chrs = n_chrs,  # Number of chromosomes
        total_population_size = total_population_size,  # Total population size
        embryo_type = "mosaic_low",  # Embryo type
        prop.euploid = 0,  # Proportion of euploid cells (0 in this case)
        prop.aneuploid = rate,  # Proportion of aneuploid cells
        prop.mosaic_high = 0,  # Proportion of high-level mosaic cells (0 in this case)
        prop.mosaic_low = 1,  # Proportion of low-level mosaic cells (100% in this case)
        dispersal = dispersal  # Dispersal rate
      )
      
      # Increment the population_id_counter for the next population
      population_id_counter <<- population_id_counter + 1
      
      return(pop)
    })
    
    # Combine all fixed mosaic populations into a single dataframe
    fixed_mosaic_populations <- do.call(rbind, fixed_mosaic_populations)
    
    return(fixed_mosaic_populations)
  }
  
  # Function to generate mixed populations
  generate_mixed_populations <- function() {
    mixed_populations <- data.frame(
      population_id = population_id_counter:(population_id_counter + 4),  # Assign a range of population_ids
      n.cells = rep(n_cells, 5),  # Number of cells in each population
      n.chrs = rep(n_chrs, 5),  # Number of chromosomes in each population
      total_population_size = rep(total_population_size, 5),  # Total population size for each
      embryo_type = rep("mixed", 5),  # Embryo type
      prop.euploid = seq(0.5, 0.1, by = -0.1),  # Decreasing proportion of euploid cells
      prop.aneuploid = rep(0.3, 5),  # Constant proportion of aneuploid cells
      prop.mosaic_high = rep(0.1, 5),  # Constant proportion of high-level mosaic cells
      prop.mosaic_low = seq(0.1, 0.5, by = 0.1),  # Increasing proportion of low-level mosaic cells
      dispersal = rep(dispersal, 5)  # Dispersal rates
    )
    
    # Increment the population_id_counter for the next set of populations
    population_id_counter <<- population_id_counter + 5
    
    return(mixed_populations)
  }
  
  # Combine all dataframes into one big dataframe
  combined_population <- rbind(
    generate_hardcoded_population(),  # Include hardcoded populations
    generate_fixed_mosaic_populations(),  # Include fixed mosaic populations
    generate_mixed_populations()  # Include mixed populations
  )
  
  return(combined_population)
}

# Example of running the function
populations_df <- makepops()
print(populations_df)

# Define the Embryo function, which creates an embryo with specified parameters
Embryo <- function(n.cells, n.chrs, prop.aneuploid, dispersal) {
  # Create a list representing an embryo with given properties
  embryo <- list(
    n.cells = n.cells,
    n.chrs = n.chrs,
    prop.aneuploid = prop.aneuploid,
    dispersal = dispersal
  )
  return(embryo)
}

# Define the takeBiopsy function, which simulates taking a biopsy from an embryo
takeBiopsy <- function(embryo, biopsy.size, index.cell, chromosome) {
  # Sample cells from the embryo to create a biopsy
  biopsy <- sample(1:embryo$n.cells, biopsy.size, replace = TRUE)
  
  # Calculate the number of aneuploid cells in the biopsy
  aneuploid_cells <- round(biopsy.size * embryo$prop.aneuploid)
  return(aneuploid_cells)
}

# Define a function to create an embryo and take a biopsy
create_biopsy <- function(n.cells, n.chrs, prop.aneuploid, dispersal, rng.seed) {
  # Set the random seed for reproducibility
  set.seed(rng.seed)
  
  # Create an embryo with specified parameters
  embryo <- Embryo(n.cells, n.chrs, prop.aneuploid, dispersal)
  
  # Take a biopsy from the created embryo
  biopsy <- takeBiopsy(embryo, biopsy.size = BIOPSY_SIZE, index.cell = 1, chromosome = 0)
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

# Combine biopsy results into a dataframe
combined_results <- data.frame(
  aneuploid_counts = biopsy_results$aneuploid_counts,
  population_id = biopsy_results$population_id
)

# View the combined results
print(combined_results)
nrow(combined_results)

# Load necessary libraries
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
if (!requireNamespace("stats", quietly = TRUE)) {
  install.packages("stats")
}

library(tidyr)
library(MASS)
library(stats)

# Create the clinical data dataframe
clinical_data <- data.frame(
  clinical_category = c("euploid", "mosaic_low", "mosaic_high", "aneuploid"),
  counts = c(6512, 799, 462, 4088)
)

# Print the clinical data to verify
print(clinical_data)

# Categorise biopsy results based on counts
combined_results <- combined_results %>%
  mutate(clinical_category = case_when(
    aneuploid_counts == 0 ~ "euploid",
    aneuploid_counts %in% c(1, 2) ~ "mosaic_low",
    aneuploid_counts %in% c(3, 4) ~ "mosaic_high",
    aneuploid_counts == 5 ~ "aneuploid",
    TRUE ~ NA_character_  #Handle any unexpected cases
  ))

# Print to check if categories are assigned correctly
print(head(combined_results))

makepops <- function(total_population_size = 11861,
                     euploid = 0.5, aneuploid = 0.5,
                     mosaic_high = 0.25, mosaic_low = 0.25,
                     n_cells = 5, n_chrs = 1, dispersal = 0.5) {
  
  # Define BIOPSY_SIZE based on n_cells
  BIOPSY_SIZE <- n_cells
  
  # Initialise population_id counter
  population_id_counter <- 1
  
  # Function to generate hardcoded populations
  generate_hardcoded_population <- function() {
    # Create a dataframe for hardcoded populations
    hardcoded_population <- data.frame(
      population_id = population_id_counter:(population_id_counter + 3),  # Assign a range of population_ids
      n.cells = rep(n_cells, 4),  # Number of cells in each population
      n.chrs = rep(n_chrs, 4),  # Number of chromosomes in each population
      total_population_size = rep(total_population_size, 4),  # Total population size for each
      embryo_type = c("euploid", "aneuploid", "mixed", "mixed"),  # Embryo types
      prop.euploid = c(1, 0, 0.5, 0.25),  # Proportions of euploid cells
      prop.aneuploid = c(0, 1, 0.5, 0.25),  # Proportions of aneuploid cells
      prop.mosaic_high = c(0, 0, 0, 0.25),  # Proportions of high-level mosaic cells
      prop.mosaic_low = c(0, 0, 0, 0.25),  # Proportions of low-level mosaic cells
      dispersal = rep(dispersal, 4)  # Dispersal rates
    )
    
    # Increment the population_id_counter for the next set of populations
    population_id_counter <<- population_id_counter + 4
    
    return(hardcoded_population)
  }
  
  # Function to generate specific low-level mosaic populations with fixed aneuploidy rates
  generate_fixed_mosaic_populations <- function() {
    aneuploidy_rates <- c(0.3, 0.4, 0.5)  # Defined aneuploidy rates
    
    # Generate a dataframe for each aneuploidy rate
    fixed_mosaic_populations <- lapply(aneuploidy_rates, function(rate) {
      pop <- data.frame(
        population_id = population_id_counter,  # Assign current population_id
        n.cells = n_cells,  # Number of cells in the biopsy
        n.chrs = n_chrs,  # Number of chromosomes
        total_population_size = total_population_size,  # Total population size
        embryo_type = "mosaic_low",  # Embryo type
        prop.euploid = 0,  # Proportion of euploid cells (0 in this case)
        prop.aneuploid = rate,  # Proportion of aneuploid cells
        prop.mosaic_high = 0,  # Proportion of high-level mosaic cells (0 in this case)
        prop.mosaic_low = 1,  # Proportion of low-level mosaic cells (100% in this case)
        dispersal = dispersal  # Dispersal rate
      )
      
      # Increment the population_id_counter for the next population
      population_id_counter <<- population_id_counter + 1
      
      return(pop)
    })
    
    # Combine all fixed mosaic populations into a single dataframe
    fixed_mosaic_populations <- do.call(rbind, fixed_mosaic_populations)
    
    return(fixed_mosaic_populations)
  }
  
  # Function to generate mixed populations
  generate_mixed_populations <- function() {
    mixed_populations <- data.frame(
      population_id = population_id_counter:(population_id_counter + 4),  # Assign a range of population_ids
      n.cells = rep(n_cells, 5),  # Number of cells in each population
      n.chrs = rep(n_chrs, 5),  # Number of chromosomes in each population
      total_population_size = rep(total_population_size, 5),  # Total population size for each
      embryo_type = rep("mixed", 5),  # Embryo type
      prop.euploid = seq(0.5, 0.1, by = -0.1),  # Decreasing proportion of euploid cells
      prop.aneuploid = rep(0.3, 5),  # Constant proportion of aneuploid cells
      prop.mosaic_high = rep(0.1, 5),  # Constant proportion of high-level mosaic cells
      prop.mosaic_low = seq(0.1, 0.5, by = 0.1),  # Increasing proportion of low-level mosaic cells
      dispersal = rep(dispersal, 5)  # Dispersal rates
    )
    
    # Increment the population_id_counter for the next set of populations
    population_id_counter <<- population_id_counter + 5
    
    return(mixed_populations)
  }
  
  # Combine all dataframes into one big dataframe
  combined_population <- rbind(
    generate_hardcoded_population(),  # Include hardcoded populations
    generate_fixed_mosaic_populations(),  # Include fixed mosaic populations
    generate_mixed_populations()  # Include mixed populations
  )
  
  return(combined_population)
}

# Example of running the function
populations_df <- makepops()
print(populations_df)

# Define the Embryo function, which creates an embryo with specified parameters
Embryo <- function(n.cells, n.chrs, prop.aneuploid, dispersal) {
  # Create a list representing an embryo with given properties
  embryo <- list(
    n.cells = n.cells,
    n.chrs = n.chrs,
    prop.aneuploid = prop.aneuploid,
    dispersal = dispersal
  )
  return(embryo)
}

# Define the takeBiopsy function, which simulates taking a biopsy from an embryo
takeBiopsy <- function(embryo, biopsy.size, index.cell, chromosome) {
  # Sample cells from the embryo to create a biopsy
  biopsy <- sample(1:embryo$n.cells, biopsy.size, replace = TRUE)
  
  # Calculate the number of aneuploid cells in the biopsy
  aneuploid_cells <- round(biopsy.size * embryo$prop.aneuploid)
  return(aneuploid_cells)
}

# Define a function to create an embryo and take a biopsy
create_biopsy <- function(n.cells, n.chrs, prop.aneuploid, dispersal, rng.seed) {
  # Set the random seed for reproducibility
  set.seed(rng.seed)
  
  # Create an embryo with specified parameters
  embryo <- Embryo(n.cells, n.chrs, prop.aneuploid, dispersal)
  
  # Take a biopsy from the created embryo
  biopsy <- takeBiopsy(embryo, biopsy.size = BIOPSY_SIZE, index.cell = 1, chromosome = 0)
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

# Combine biopsy results into a dataframe
combined_results <- data.frame(
  aneuploid_counts = biopsy_results$aneuploid_counts,
  population_id = biopsy_results$population_id
)

# View the combined results
print(combined_results)
nrow(combined_results)

# Load necessary libraries
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
if (!requireNamespace("stats", quietly = TRUE)) {
  install.packages("stats")
}

library(tidyr)
library(MASS)
library(stats)

# Create the clinical data dataframe
clinical_data <- data.frame(
  clinical_category = c("euploid", "mosaic_low", "mosaic_high", "aneuploid"),
  counts = c(6512, 799, 462, 4088)
)

# Print the clinical data to verify
print(clinical_data)

# Categorise biopsy results based on counts
combined_results <- combined_results %>%
  mutate(clinical_category = case_when(
    aneuploid_counts == 0 ~ "euploid",
    aneuploid_counts %in% c(1, 2) ~ "mosaic_low",
    aneuploid_counts %in% c(3, 4) ~ "mosaic_high",
    aneuploid_counts == 5 ~ "aneuploid",
    TRUE ~ NA_character_  #Handle any unexpected cases
  ))

# Print to check if categories are assigned correctly
print(head(combined_results))

# Initialize an empty vector to store p-values
p_values <- numeric(nrow(populations_df))

# Loop through each population
for (i in 1:nrow(populations_df)) {
  # Check if the population is fully euploid
  if (populations_df$embryo_type[i] == "euploid") {
    # For fully euploid populations with zero aneuploid counts, set p-value to 1
    p_values[i] <- 1
  } else {
    # Filter combined_results for the current population_id
    population_results <- combined_results[combined_results$population_id == populations_df$population_id[i], ]
    
    # Ensure there are observed counts
    if (nrow(population_results) > 0) {
      # Observed counts (number of aneuploid cells)
      observed_counts <- population_results$aneuploid_counts
      
      # Expected proportions from makepops function
      expected_props <- populations_df[i, c("prop.euploid", "prop.aneuploid", "prop.mosaic_high", "prop.mosaic_low")]
      
      # Expected counts (total biopsy size times expected proportions)
      expected_counts <- populations_df$total_population_size[i] * expected_props
      
      # Calculate chi-squared statistic
      chi_sq_stat <- sum((observed_counts - expected_counts)^2 / expected_counts)
      
      # Degrees of freedom (number of categories - 1)
      df <- length(observed_counts) - 1
      
      # Calculate p-value
      p_value <- 1 - pchisq(chi_sq_stat, df)
      
      # Store p-value in the vector
      p_values[i] <- p_value
    } else {
      # If no observed counts, handle the case (e.g., set p-value to NA)
      p_values[i] <- NA
    }
  }
}

# Combine population_id and p-values into a dataframe
population_p_values <- data.frame(
  population_id = populations_df$population_id,
  p_value = p_values
)

# Print the p-values for each population
print(population_p_values)


