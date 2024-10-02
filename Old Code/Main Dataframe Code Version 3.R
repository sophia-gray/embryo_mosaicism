install.packages("dplyr")
library(dplyr)

makepops <- function(total_population_size = 11816,
                     euploid, aneuploid, mosaic_high, mosaic_low,
                     n_cells = 5, n_chrs = 1) {
  
  # Define constants
  BIOPSY_SIZE <- n_cells  # Fixed biopsy size
  MOSAIC_PROPORTIONS <- 1  # 100% low-level mosaics
  
  # Check if proportions sum to 1
  if (euploid + aneuploid + mosaic_high + mosaic_low != 1) {
    stop("Proportions must sum to 1.")
  }
  
  # Initialise population_id counter
  population_id_counter <- 1
  
  # Generate the programmatic population dataframe
  generate_programmatic_population <- function(euploid, aneuploid, mosaic_high, mosaic_low) {
    programmatic_population <- data.frame(
      population_id = population_id_counter,
      n.cells = BIOPSY_SIZE,
      n.chrs = n_chrs,
      total_population_size = total_population_size,
      embryo_type = c("euploid", "aneuploid", "mosaic_high", "mosaic_low"),
      proportion = c(euploid, aneuploid, mosaic_high, mosaic_low),
      prop.aneuploid = c(
        0, 1, 0, 0    # 50% euploid, 50% aneuploid
      ),
      dispersal = c(
        0, 0.9, 0.4, 0.5     # dispersal rates
      )
    )
    
    population_id_counter <<- population_id_counter + 1
    
    programmatic_population
  }
  
  # Generate hardcoded populations
  generate_hardcoded_population <- function() {
    hardcoded_population <- data.frame(
      population_id = population_id_counter:(population_id_counter + 7), # Adjusted for 8 populations
      n.cells = rep(BIOPSY_SIZE, 8),
      n.chrs = rep(n_chrs, 8),
      total_population_size = rep(total_population_size, 8),
      embryo_type = c("euploid", "aneuploid", "mixed", "mixed", "mosaic_high", "mosaic_low", "mixed", "mixed"),
      proportion = c(0.25, 0.25, 1, 1, 0.25, 0.25, 1, 1),
      prop.aneuploid = c(
        0, 1, NA, NA, 0, 0, NA, NA
      ),
      dispersal = c(
        0, 0.9, NA, NA, 0.4, 0.5, NA, NA
      )
    )
    
    population_id_counter <<- population_id_counter + 8  # Increase counter accordingly
    
    hardcoded_population
  }
  
  # Generate specific low-level mosaic populations with fixed aneuploidy rates
  generate_fixed_mosaic_populations <- function() {
    aneuploidy_rates <- c(0.3, 0.4, 0.5)
    
    fixed_mosaic_populations <- lapply(aneuploidy_rates, function(rate) {
      data.frame(
        population_id = population_id_counter,
        n.cells = BIOPSY_SIZE,
        n.chrs = n_chrs,
        total_population_size = total_population_size,
        embryo_type = "mosaic_low",
        proportion = MOSAIC_PROPORTIONS,
        prop.aneuploid = rate,
        dispersal = runif(1, 0.1, 0.9)
      )
    })
    
    fixed_mosaic_populations <- do.call(rbind, fixed_mosaic_populations)
    fixed_mosaic_populations$population_id <- population_id_counter:(population_id_counter + length(aneuploidy_rates) - 1)
    population_id_counter <<- population_id_counter + length(aneuploidy_rates)
    
    fixed_mosaic_populations
  }
  
  # Combine all dataframes into one big dataframe
  combined_population <- rbind(
    generate_hardcoded_population(),
    generate_fixed_mosaic_populations(),
    generate_programmatic_population(euploid, aneuploid, mosaic_high, mosaic_low)
  )
  
  # Remove rng.seed column if not needed
  combined_population <- combined_population[, !names(combined_population) %in% "rng.seed"]
  
  return(combined_population)
}

# Example usage:
populations_df <- makepops(euploid = 0.4, aneuploid = 0.1, mosaic_high = 0.2, mosaic_low = 0.3)

# Display the resulting dataframe
print(populations_df)

