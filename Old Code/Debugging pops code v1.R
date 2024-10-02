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

# Installing devtools to be able to access tessera.
install.packages("devtools")

# Running the install_github command to access tessera.
# Note: Packages may upgrade. Enter '1' if asked to upgrade to current packages.  
devtools::install_github("bmskinner/tessera")

# Loading tessera for use. 
library(tessera)

# Define the makepops function to generate populations
makepops <- function(total_population_size = 11861, n_cells = 5, n_chrs = 1, dispersal = 0.5) {
  
  # Initialise population_id counter
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
      prop.mosaic_high = c(0, 0, 0, 0),
      prop.mosaic_low = c(0, 0, 0, 0.25),
      dispersal = rep(dispersal, 4)
    )
    population_id_counter <<- population_id_counter + 4
    return(hardcoded_population)
  }
  
  # Function to generate fixed mosaic_low populations with varying aneuploidy rates
  generate_fixed_mosaic_low_populations <- function() {
    aneuploidy_rates <- c(0.3, 0.4, 0.5)
    fixed_mosaic_low_populations <- lapply(aneuploidy_rates, function(rate) {
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
    fixed_mosaic_low_populations <- do.call(rbind, fixed_mosaic_low_populations)
    return(fixed_mosaic_low_populations)
  }
  
  # Function to generate fixed mosaic_high populations with varying levels
  generate_fixed_mosaic_high_populations <- function() {
    mosaic_levels <- c(0.6, 0.7, 0.8)
    fixed_mosaic_high_populations <- lapply(mosaic_levels, function(level) {
      pop <- data.frame(
        population_id = population_id_counter,
        n.cells = n_cells,
        n.chrs = n_chrs,
        total_population_size = total_population_size,
        embryo_type = "mosaic_high",
        prop.euploid = 0,
        prop.aneuploid = 0,
        prop.mosaic_high = level,
        prop.mosaic_low = 1 - level,
        dispersal = dispersal
      )
      population_id_counter <<- population_id_counter + 1
      return(pop)
    })
    fixed_mosaic_high_populations <- do.call(rbind, fixed_mosaic_high_populations)
    return(fixed_mosaic_high_populations)
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
    generate_fixed_mosaic_low_populations(), # Fixed mosaic_low populations
    generate_fixed_mosaic_high_populations(), # Include new high mosaic populations
    generate_mixed_populations()
  )
  
  return(combined_population)
}

# Generate populations
populations_df <- makepops()

print(populations_df)

# Function to take a biopsy and count aneuploid cells
take_biopsy <- function(population_df, biopsy_size = 5) {
  set.seed(42) # For reproducibility
  
  # Function to categorise based on aneuploid count
  categorise_biopsy <- function(aneuploid_count) {
    if (aneuploid_count == 0) {
      return("Euploid")
    } else if (aneuploid_count %in% c(1, 2)) {
      return("Mosaic Low")
    } else if (aneuploid_count %in% c(3, 4)) {
      return("Mosaic High")
    } else {
      return("Aneuploid")
    }
  }
  
  # Make results list
  biopsy_results_list <- list()
  
  # Loop through each population and take biopsies for each embryo
  for (i in 1:nrow(population_df)) {
    pop <- population_df[i, ]
    num_embryos <- pop$total_population_size
    
    for (j in 1:num_embryos) {
      biopsy <- sample(
        c(rep("Euploid", round(pop$n.cells * pop$prop.euploid)),
          rep("Aneuploid", round(pop$n.cells * pop$prop.aneuploid)),
          rep("Mosaic_High", round(pop$n.cells * pop$prop.mosaic_high)),
          rep("Mosaic_Low", round(pop$n.cells * pop$prop.mosaic_low))),
        biopsy_size, replace = TRUE
      )
      
      aneuploid_count <- sum(biopsy == "Aneuploid")
      category <- categorise_biopsy(aneuploid_count)
      
      biopsy_results_list <- append(biopsy_results_list, list(
        data.frame(
          population_id = pop$population_id,
          biopsy = list(biopsy),
          aneuploid_count = aneuploid_count,
          category = category
        )
      ))
    }
  }
  
  # Combine results into a single dataframe
  biopsy_results <- bind_rows(biopsy_results_list)
  
  return(biopsy_results)
}

# Take biopsies from the population
biopsy_results <- take_biopsy(populations_df, biopsy_size = 5)

print(biopsy_results)

# Count the number of biopsies in each category for each population
count_categories <- function(biopsy_results) {
  category_counts <- biopsy_results %>%
    group_by(population_id, category) %>%
    summarise(count = n()) %>%
    spread(category, count, fill = 0) %>%
    ungroup()
  
  return(category_counts)
}

# Get category counts
category_counts <- count_categories(biopsy_results)

print(category_counts)

# Function to compare category counts with a clinical population
compare_with_clinical <- function(category_counts, clinical_counts) {
  p_values <- category_counts %>%
    rowwise() %>%
    mutate(
      p_value = chisq.test(cbind(
        c(Euploid, `Mosaic Low`, `Mosaic High`, Aneuploid),
        clinical_counts
      ))$p.value
    ) %>%
    ungroup() %>%
    select(population_id, p_value)
  
  return(p_values)
}

# Example clinical population counts (Euploid, Mosaic Low, Mosaic High, Aneuploid)
clinical_counts <- c(6512, 799, 462, 4088)

# Compare with clinical population
p_values <- compare_with_clinical(category_counts, clinical_counts)

print(p_values)

