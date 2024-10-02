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
  
  # Function to generate a mixed population dataframe
  generate_mixed_population <- function(euploid, aneuploid, mosaic_high, mosaic_low) {
    # Create a dataframe for a single mixed population with specified proportions
    mixed_population <- data.frame(
      population_id = population_id_counter,  # Assign current population_id
      n.cells = BIOPSY_SIZE,  # Number of cells in the biopsy
      n.chrs = n_chrs,  # Number of chromosomes
      total_population_size = total_population_size,  # Total population size
      embryo_type = "mixed",  # Embryo type
      prop.euploid = euploid,  # Proportion of euploid cells
      prop.aneuploid = aneuploid,  # Proportion of aneuploid cells
      prop.mosaic_high = mosaic_high,  # Proportion of high-level mosaic cells
      prop.mosaic_low = mosaic_low,  # Proportion of low-level mosaic cells
      dispersal = dispersal  # Dispersal rate
    )
    
    # Increment the population_id_counter for the next population
    population_id_counter <<- population_id_counter + 1
    
    return(mixed_population)
  }
  
  # Function to generate hardcoded populations
  generate_hardcoded_population <- function() {
    # Create a dataframe for hardcoded populations
    hardcoded_population <- data.frame(
      population_id = population_id_counter:(population_id_counter + 3),  # Assign a range of population_ids
      n.cells = rep(BIOPSY_SIZE, 4),  # Number of cells in each population
      n.chrs = rep(n_chrs, 4),  # Number of chromosomes in each population
      total_population_size = rep(total_population_size, 4),  # Total population size for each
      embryo_type = c("euploid", "aneuploid", "mixed", "mixed"),  # Embryo types
      prop.euploid = c(1, 0, 0.5, 0.25),  # Proportions of euploid cells
      prop.aneuploid = c(0, 1, 0.5, 0.25),  # Proportions of aneuploid cells
      prop.mosaic_high = c(0, 0, 0, 0.25),  # Proportions of high-level mosaic cells
      prop.mosaic_low = c(0, 0, 0, 0.25),  # Proportions of low-level mosaic cells
      dispersal = rep(0.5, 4)  # Dispersal rates
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
        n.cells = BIOPSY_SIZE,  # Number of cells in the biopsy
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
  
  # Hardcoded mixed populations with varying proportions
  mixed_populations <- data.frame(
    population_id = population_id_counter:(population_id_counter + 4),  # Assign a range of population_ids
    n.cells = rep(BIOPSY_SIZE, 5),  # Number of cells in each population
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
  
  # Combine all dataframes into one big dataframe
  combined_population <- rbind(
    generate_hardcoded_population(),  # Include hardcoded populations
    generate_fixed_mosaic_populations(),  # Include fixed mosaic populations
    mixed_populations  # Include mixed populations
  )
  
  return(combined_population)
}

# Example of running the function
populations_df <- makepops()
print(populations_df)
