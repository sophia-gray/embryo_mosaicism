# Load the necessary library
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("bmskinner/tessera")
library(tessera)

# Define the makepops function
makepops <- function(total_population_size = 11861, n_cells = 200, n_chrs = 1, dispersal = 0.5) {
  
  # Function to generate the population dataframe
  generate_population_df <- function(n_cells, n_chrs, total_population_size, rng_seed, embryo_type, proportion, prop_aneuploid, prop_mosaic_high, prop_mosaic_low, dispersal) {
    data.frame(
      n.cells = n_cells,
      n.chrs = n_chrs,
      total_population_size = total_population_size,
      rng.seed = rng_seed,
      embryo_type = embryo_type,
      proportion = proportion,
      prop.aneuploid = prop_aneuploid,
      prop.mosaic_high = prop_mosaic_high,
      prop.mosaic_low = prop_mosaic_low,
      dispersal = dispersal
    )
  }
  
  # Generate populations
  hardcoded_populations <- rbind(
    generate_population_df(n_cells, n_chrs, total_population_size, 42, "euploid", 1, 0, 0, 0, dispersal),
    generate_population_df(n_cells, n_chrs, total_population_size, 42, "aneuploid", 1, 1, 0, 0, dispersal),
    generate_population_df(n_cells, n_chrs, total_population_size, 42, "mixed", 1, 0.5, 0.1, 0.1, dispersal),
    generate_population_df(n_cells, n_chrs, total_population_size, 42, "mixed", 1, 0.25, 0.25, 0.25, dispersal)
  )
  
  aneuploidy_rates <- c(0.3, 0.4, 0.5)
  fixed_mosaic_populations <- do.call(rbind, lapply(aneuploidy_rates, function(rate) {
    generate_population_df(n_cells, n_chrs, total_population_size, 42, "mosaic_low", 1, rate, 0, 1, dispersal)
  }))
  
  mixed_populations <- do.call(rbind, lapply(seq(0.1, 0.5, by = 0.1), function(mosaic_low) {
    generate_population_df(n_cells, n_chrs, total_population_size, 42, "mixed", 1, 0.3, 0.1, mosaic_low, dispersal)
  }))
  
  combined_population <- rbind(hardcoded_populations, fixed_mosaic_populations, mixed_populations)
  
  # Assign unique simulation IDs
  combined_population$simulation_id <- 1:nrow(combined_population)
  
  return(combined_population)
}

# Define the make.embryos function with progress updates
make.embryos <- function(pops) {
  embryos_list <- lapply(1:nrow(pops), function(i) {
    pop <- pops[i, ]
    
    # Print progress
    cat("Generating embryos for population", i, "of", nrow(pops), "\n")
    
    embryos <- lapply(1:pop$total_population_size, function(x) {
      # Create an embryo using the parameters from the population dataframe
      tessera::Embryo(
        n.cells = pop$n.cells,
        n.chrs = pop$n.chrs,
        prop.aneuploid = pop$prop.aneuploid,
        dispersal = pop$dispersal
      )
    })
    
    return(embryos)
  })
  
  return(embryos_list)
}

# Define the biopsy.embryos function with progress updates
biopsy.embryos <- function(embryos_list, biopsy_size = 5, index_cell = 1, chromosome = 0) {
  biopsies_list <- lapply(1:length(embryos_list), function(i) {
    # Print progress
    cat("Taking biopsies for population", i, "of", length(embryos_list), "\n")
    
    embryos <- embryos_list[[i]]
    
    lapply(embryos, function(embryo) {
      tessera::takeBiopsy(embryo, biopsy.size = biopsy_size, index.cell = index_cell, chromosome = chromosome)
    })
  })
  
  return(biopsies_list)
}

# Function to classify biopsy results
classify_biopsy_results <- function(biopsies_list) {
  classified_results <- lapply(biopsies_list, function(pop_biopsies) {
    lapply(pop_biopsies, function(biopsy) {
      sapply(biopsy, function(result) {
        if (result == 0) {
          return("euploid")
        } else if (result == 1 || result == 2) {
          return("mosaic_low")
        } else if (result == 3 || result == 4) {
          return("mosaic_high")
        } else if (result == 5) {
          return("aneuploid")
        } else {
          return(NA)  # Handle unexpected values
        }
      })
    })
  })
  return(classified_results)
}

# Function to perform Chi-squared test for one population against a reference
perform_chisq_test <- function(pop_counts, ref_counts) {
  # Ensure the counts are in the same order
  all_classes <- union(names(pop_counts), names(ref_counts))
  pop_counts <- as.numeric(pop_counts[match(all_classes, names(pop_counts))])
  ref_counts <- as.numeric(ref_counts[match(all_classes, names(ref_counts))])
  
  # Perform Chi-squared test
  chi_sq_test <- chisq.test(x = pop_counts, p = ref_counts / sum(ref_counts))
  
  # Return a list with the p-value and counts
  result <- list(
    p_value = chi_sq_test$p.value,
    counts = pop_counts
  )
  
  return(result)
}

# Function to generate Chi-squared test results for all populations
test_all_populations <- function(classified_biopsies, ref_counts) {
  results <- lapply(seq_along(classified_biopsies), function(i) {
    # Get the counts for the current population
    pop_counts_table <- table(unlist(classified_biopsies[[i]]))
    pop_counts <- as.numeric(pop_counts_table)
    names(pop_counts) <- names(pop_counts_table)
    
    # Ensure all classes are present even if counts are zero
    all_classes <- names(ref_counts)
    for (class in all_classes) {
      if (!class %in% names(pop_counts)) {
        pop_counts[class] <- 0
      }
    }
    
    # Perform Chi-squared test
    test_result <- perform_chisq_test(pop_counts, ref_counts)
    
    # Create a data frame for results
    result_df <- data.frame(
      population_id = i,
      p_value = test_result$p_value,
      euploid = pop_counts["euploid"],
      mosaic_low = pop_counts["mosaic_low"],
      mosaic_high = pop_counts["mosaic_high"],
      aneuploid = pop_counts["aneuploid"]
    )
    
    return(result_df)
  })
  
  # Combine all results into a single data frame
  results_df <- do.call(rbind, results)
  
  return(results_df)
}

# Main workflow to run the complete analysis
pops <- makepops()
all_embryos <- make.embryos(pops)
all_biopsies <- biopsy.embryos(all_embryos)
classified_biopsies <- classify_biopsy_results(all_biopsies)

# Define the reference counts
ref_counts <- c(
  euploid = 6512,
  mosaic_low = 799,
  mosaic_high = 462,
  aneuploid = 4088
)

# Perform Chi-squared tests and compile results
test_results_df <- test_all_populations(classified_biopsies, ref_counts)

# Print the results
print(test_results_df)
