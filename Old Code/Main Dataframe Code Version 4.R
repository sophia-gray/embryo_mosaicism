# Install and load necessary packages if not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Constants
BIOPSY_SIZE <- 5
n_chrs <- 1
total_population_size <- 11816

# Function to generate populations
makepops <- function(total_population_size = 11816,
                     euploid = 0.5, aneuploid = 0.5,
                     mosaic_high = 0.25, mosaic_low = 0.25,
                     n_cells = 5, n_chrs = 1, dispersal = 0.5) {
  
  # Initialise population_id counter
  population_id_counter <- 1
  
  # Function to generate the programmatic population dataframe
  generate_programmatic_population <- function(euploid, aneuploid, mosaic_high, mosaic_low) {
    programmatic_population <- data.frame(
      population_id = population_id_counter,
      n.cells = BIOPSY_SIZE,
      n.chrs = n_chrs,
      total_population_size = total_population_size,
      embryo_type = "programmatic",
      prop.euploid = euploid,
      prop.aneuploid = aneuploid,
      prop.mosaic_high = mosaic_high,
      prop.mosaic_low = mosaic_low,
      dispersal = dispersal
    )
    
    population_id_counter <<- population_id_counter + 1
    
    return(programmatic_population)
  }
  
  # Function to generate hardcoded populations
  generate_hardcoded_population <- function() {
    hardcoded_population <- data.frame(
      population_id = rep(1:4, each = 1),  # Population IDs from 1 to 4
      n.cells = rep(BIOPSY_SIZE, 4),       # Assuming BIOPSY_SIZE is defined elsewhere
      n.chrs = rep(n_chrs, 4),             # Assuming n_chrs is defined elsewhere
      total_population_size = rep(total_population_size, 4),  # Total population size
      embryo_type = c("euploid", "aneuploid", "euploid", "aneuploid"),  # Embryo types
      prop.euploid = c(1, 0, 0.5, 0.25),       # Proportions of each scenario
      prop.aneuploid = c(0, 1, 0.5, 0.25),     # Proportions of each scenario
      prop.mosaic_high = c(0, 0, 0, 0.25),     # Proportions of each scenario
      prop.mosaic_low = c(0, 0, 0, 0.25),      # Proportions of each scenario
      dispersal = rep(0.5, 4)               # Dispersal for each population
    )
    
    # Assigning "mixed" for embryo types that are not euploid or aneuploid
    hardcoded_population$embryo_type[!(hardcoded_population$embryo_type %in% c("euploid", "aneuploid"))] <- "mixed"
    
    population_id_counter <<- population_id_counter + 1
    
    return(hardcoded_population)
  }
  
  # Function to generate specific low-level mosaic populations with fixed aneuploidy rates
  generate_fixed_mosaic_populations <- function() {
    aneuploidy_rates <- c(0.3, 0.4, 0.5)
    
    fixed_mosaic_populations <- lapply(aneuploidy_rates, function(rate) {
      data.frame(
        population_id = population_id_counter,
        n.cells = BIOPSY_SIZE,
        n.chrs = n_chrs,
        total_population_size = total_population_size,
        embryo_type = "mosaic_low",
        prop.euploid = 0,
        prop.aneuploid = rate,
        prop.mosaic_high = 0,
        prop.mosaic_low = 1,  # 100% low-level mosaics
        dispersal = dispersal
      )
    })
    
    fixed_mosaic_populations <- do.call(rbind, fixed_mosaic_populations)
    population_id_counter <<- population_id_counter + 1
    
    return(fixed_mosaic_populations)
  }
  
  # Combine all dataframes into one big dataframe
  combined_population <- rbind(
    generate_hardcoded_population(),
    generate_fixed_mosaic_populations(),
    generate_programmatic_population(euploid, aneuploid, mosaic_high, mosaic_low)
  )
  
  return(combined_population)
}

# Example of running the function
populations_df <- makepops(euploid = 0.4, aneuploid = 0.1, mosaic_high = 0.2, mosaic_low = 0.3)
print(populations_df)


# embryo types on hardcoded pops arent quite right 