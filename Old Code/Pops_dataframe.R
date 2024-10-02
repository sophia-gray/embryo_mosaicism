# Define a dataframe with all parameters for each population simulation
population_params <- data.frame(
  simulation_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
  n.cells = c(200, 200, 200, 200, 300, 300, 300, 300, 200, 200, 200, 200),
  n.chrs = c(1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1),
  total_population_size = c(10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000),
  rng.seed = c(42, 42, 42, 42, 43, 43, 43, 43, 44, 44, 44, 44),
  embryo_type = c("euploid", "aneuploid", "mosaic_high", "mosaic_low", 
                  "euploid", "aneuploid", "mosaic_high", "mosaic_low", 
                  "euploid", "aneuploid", "mosaic_high", "mosaic_low"),
  proportion = c(0.4, 0.4, 0.1, 0.1, 
                 0.3, 0.3, 0.2, 0.2, 
                 0.5, 0.3, 0.1, 0.1),
  prop.aneuploid = c(0, 1, 0.7, 0.3, 
                     0, 1, 0.7, 0.3, 
                     0, 1, 0.7, 0.3),
  dispersal = c(0, 0.9, 0.9, 0.9, 
                0, 0.9, 0.9, 0.9, 
                0, 0.9, 0.9, 0.9)
)

# View the dataframe
print(population_params)

create_mixed_population <- function(simulation_params) {
  set.seed(simulation_params$rng.seed[1])
  total_population_size <- simulation_params$total_population_size[1]
  population <- numeric(total_population_size)
  current_index <- 1
  
  for (i in 1:nrow(simulation_params)) {
    embryo_type <- simulation_params[i, ]
    num_embryos <- round(total_population_size * embryo_type$proportion)
    
    population[current_index:(current_index + num_embryos - 1)] <- sapply(1:num_embryos, function(x) create_biopsy(
      n.cells = embryo_type$n.cells, n.chrs = embryo_type$n.chrs, 
      prop.aneuploid = embryo_type$prop.aneuploid, 
      dispersal = embryo_type$dispersal, rng.seed = embryo_type$rng.seed + current_index + x
    ))
    
    current_index <- current_index + num_embryos
  }
  
  return(population)
}

# Split the population_params dataframe by simulation_id
simulation_groups <- split(population_params, population_params$simulation_id)

# Run simulations
simulation_results <- lapply(simulation_groups, create_mixed_population)

# Combine results into a dataframe
combined_results <- data.frame()

for (i in 1:length(simulation_results)) {
  sim_result <- data.frame(
    aneuploid_counts = simulation_results[[i]],
    simulation_id = i
  )
  combined_results <- rbind(combined_results, sim_result)
}

# View the combined results
head(combined_results)

