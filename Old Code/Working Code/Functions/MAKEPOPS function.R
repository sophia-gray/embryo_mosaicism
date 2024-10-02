# Define the makepops function to generate populations
makepops <- function(total_population_size = 11861, euploid = 0, aneuploid = 0,
                     mosaic_low = 0, n_cells = 5, n_chrs = 1, dispersal = 0.5) {
  
  # Define BIOPSY_SIZE based on n_cells
  BIOPSY_SIZE <- n_cells
  
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
      prop.mosaic_high = c(0, 0, 0, 0.25),
      prop.mosaic_low = c(0, 0, 0, 0.25),
      dispersal = rep(dispersal, 4)
    )
    population_id_counter <<- population_id_counter + 4
    return(hardcoded_population)
  }
  
  # Function to generate specific low-level mosaic populations with fixed aneuploidy rates
  generate_fixed_mosaic_populations <- function() {
    aneuploidy_rates <- c(0.3, 0.4, 0.5)
    fixed_mosaic_populations <- lapply(aneuploidy_rates, function(rate) {
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
    fixed_mosaic_populations <- do.call(rbind, fixed_mosaic_populations)
    return(fixed_mosaic_populations)
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
  
  # Initialize population_id_counter
  population_id_counter <<- 1
  
  combined_population <- rbind(
    generate_hardcoded_population(),
    generate_fixed_mosaic_populations(),
    generate_mixed_populations()
  )
  
  return(combined_population)
}

# Example of running the function to generate populations
populations_df <- makepops()
print("Generated Populations:")
print(populations_df)
