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

# Generate the population dataframe
population_df <- makepops()

# Print the dataframe
print(population_df)
