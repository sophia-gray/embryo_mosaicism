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

# Loop through each population row and create biopsy results
biopsy_results <- mapply(function(n.cells, n.chrs, prop.aneuploid, dispersal) {
  create_biopsy(n.cells, n.chrs, prop.aneuploid, dispersal, rng.seed = 42)
}, populations_df$n.cells, populations_df$n.chrs, populations_df$prop.aneuploid, populations_df$dispersal)

# Combine biopsy results into a dataframe
combined_results <- data.frame(
  aneuploid_counts = biopsy_results,
  population_id = populations_df$population_id
)

# View the combined results
print(combined_results)

# Install and load necessary packages if not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Define colors for each population ID
# Define colors for each population ID (adjust as needed)
population_colors <- c("red", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "green",
                       "purple", "brown", "pink", "blue")  # Add more colors if necessary

# Plot the combined results using ggplot2 with defined colors
ggplot(combined_results, aes(x = as.factor(population_id), y = aneuploid_counts, fill = as.factor(population_id))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = population_colors) +
  labs(x = "Population ID", y = "Aneuploid Cell Counts", fill = "Population ID") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

#This graph isn't quite right. 
