# Installing devtools to be able to access tessera.
install.packages("devtools")

# Running the install_github command to access tessera.
# Note: Packages may upgrade. Enter '1' if asked to upgrade to current packages.  
devtools::install_github("bmskinner/tessera")

# Loading tessera for use. 
library(tessera)

# Define a dataframe with all parameters for each population simulation
population_params <- data.frame(
  # Simulation IDs, used to differentiate between different population runs
  simulation_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
  
  # Number of cells in each embryo
  n.cells = c(200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200),
  
  # Number of chromosomes in each embryo
  n.chrs = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  
  # Total population size for each simulation
  total_population_size = c(10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000),
  
  # Seed for random number generation to ensure reproducibility
  rng.seed = c(42, 42, 42, 42, 43, 43, 43, 43, 44, 44, 44, 44),
  
  # Type of embryo: euploid (normal), aneuploid (abnormal), mosaic_high (high proportion abnormal), mosaic_low (low proportion abnormal)
  embryo_type = c("euploid", "aneuploid", "mosaic_high", "mosaic_low", 
                  "euploid", "aneuploid", "mosaic_high", "mosaic_low", 
                  "euploid", "aneuploid", "mosaic_high", "mosaic_low"),
  
  # Proportion of each embryo type in the population
  proportion = c(0.4, 0.4, 0.1, 0.1, 
                 0.3, 0.3, 0.2, 0.2, 
                 0.5, 0.3, 0.1, 0.1),
  
  # Proportion of aneuploid cells in each embryo
  prop.aneuploid = c(0, 1, 0.7, 0.3, 
                     0, 1, 0.7, 0.3, 
                     0, 1, 0.7, 0.3),
  
  # Dispersal parameter, indicating how aneuploid cells are spread
  dispersal = c(0, 0.9, 0.9, 0.9, 
                0, 0.9, 0.9, 0.9, 
                0, 0.9, 0.9, 0.9)
)

# View the dataframe
print(population_params)

# Loading ggplot
library(ggplot2)

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
  biopsy <- takeBiopsy(embryo, biopsy.size = 5, index.cell = 1, chromosome = 0)
  return(biopsy)
}

# Define a function to create a mixed population of embryos based on parameters using mapply
create_mixed_population <- function(simulation_params) {
  set.seed(simulation_params$rng.seed[1]) # Set the random seed for reproducibility
  total_population_size <- simulation_params$total_population_size[1] # Get the total population size for the simulation
  population <- numeric(total_population_size) # Initialise an empty population vector
  current_index <- 1 # Initialise the starting index for the population vector
  
  for (i in 1:nrow(simulation_params)) { # Loop through each row of the simulation parameters
    embryo_type <- simulation_params[i, ] # Get the parameters for the current embryo type
    num_embryos <- round(total_population_size * embryo_type$proportion) # Calculate the number of embryos of this type to create
    
    # Create the embryos and store the biopsy results in the population vector using mapply
    population[current_index:(current_index + num_embryos - 1)] <- mapply(function(x) create_biopsy(
      n.cells = embryo_type$n.cells, n.chrs = embryo_type$n.chrs, 
      prop.aneuploid = embryo_type$prop.aneuploid, 
      dispersal = embryo_type$dispersal, rng.seed = embryo_type$rng.seed + current_index + x
    ), 1:num_embryos)
    
    current_index <- current_index + num_embryos # Update the current index to the next position in the population vector
  }
  
  return(population)
}

# Split the population_params dataframe by simulation_id
simulation_groups <- split(population_params, population_params$simulation_id)

# Run simulations for each group of parameters
simulation_results <- lapply(simulation_groups, create_mixed_population)

# Combine results into a single dataframe
combined_results <- data.frame()

# Loop through each simulation result to combine into one dataframe
for (i in 1:length(simulation_results)) {
  # Create a dataframe for the current simulation result
  sim_result <- data.frame(
    aneuploid_counts = simulation_results[[i]],
    simulation_id = i
  )
  # Combine the current simulation result to the combined results dataframe
  combined_results <- rbind(combined_results, sim_result)
}

# View the combined results
head(combined_results)



