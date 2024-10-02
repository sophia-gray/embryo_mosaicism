makepops <- function(total_population_size = 11816, n_replicates = 1,
                     mosaic_proportions, programmatic_proportions,
                     n_cells = 5, n_chrs = 1) {
  
  # Define constants
  ANEUPLOIDY_RANGE <- seq(0, 1, 0.1)
  DISPERSAL_RANGE <- seq(0, 1, 0.1)
  BIOPSY_SIZE <- n_cells  # Fixed biopsy size
  
  # Check if proportions sum to 1 for programmatic proportions
  if (sum(programmatic_proportions) != 1) {
    stop("Programmatic proportions must sum to 1.")
  }
  
  # Generate combinations of embryo types and biopsy sizes for replicates
  combs <- expand.grid(
    n.replicates = 1:n_replicates,
    embryo_type = names(programmatic_proportions)
  )
  
  # Generate the programmatic population dataframe
  programmatic_population <- lapply(1:nrow(combs), function(i) {
    comb <- combs[i,]
    embryo_type <- comb$embryo_type
    proportion <- programmatic_proportions[[embryo_type]]
    
    data.frame(
      simulation_id = comb$n.replicates,
      n.cells = BIOPSY_SIZE,
      n.chrs = n_chrs,
      total_population_size = total_population_size,
      rng.seed = comb$n.replicates,
      embryo_type = embryo_type,
      proportion = proportion,
      prop.aneuploid = ifelse(embryo_type == "euploid", 0, 
                              ifelse(embryo_type == "aneuploid", 1,
                                     ifelse(embryo_type == "mosaic_high", runif(1, 0.5, 0.7),
                                            ifelse(embryo_type == "mosaic_low", runif(1, 0.3, 0.5), NA)))),
      dispersal = ifelse(embryo_type == "euploid", 0, runif(1, 0.1, 0.9))
    )
  })
  
  programmatic_population <- do.call(rbind, programmatic_population)
  
  # 1. Hardcoded Populations
  hardcoded_population <- data.frame(
    simulation_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4),
    n.cells = rep(BIOPSY_SIZE, 16),
    n.chrs = rep(n_chrs, 16),
    total_population_size = rep(total_population_size, 16),
    rng.seed = rep(c(42, 43, 44, 45), each = 4),
    embryo_type = rep(c("euploid", "aneuploid", "mosaic_high", "mosaic_low"), 4),
    proportion = c(0.25, 0.25, 0.25, 0.25, 1, 0, 0, 0, 0, 1, 0, 0, 0.25, 0.25, 0.25, 0.25),
    prop.aneuploid = c(
      0, 1, 0, 0,    # 50% euploid, 50% aneuploid
      0, 0, 0, 0,    # 100% euploid
      0, 1, 0, 0,    # 100% aneuploid
      0, 1, 0.7, 0.3 # 25% each of euploid, mosaic_low, mosaic_high, aneuploid
    ),
    dispersal = c(
      0, 0.9, 0, 0,     # 50% euploid, 50% aneuploid
      0, 0, 0, 0,       # 100% euploid
      0, 0.9, 0, 0,     # 100% aneuploid
      0, 0.9, 0.9, 0.9  # 25% each of euploid, mosaic_low, mosaic_high, aneuploid
    )
  )
  
  # 2. Low-Level Mosaic Populations
  low_level_mosaic_population <- data.frame(
    simulation_id = rep(1:n_replicates, each = 4),
    n.cells = rep(BIOPSY_SIZE, 4 * n_replicates),
    n.chrs = rep(n_chrs, 4 * n_replicates),
    total_population_size = rep(total_population_size, 4 * n_replicates),
    rng.seed = rep(42:(41 + n_replicates), each = 4),
    embryo_type = rep("mosaic_low", 4 * n_replicates),
    proportion = rep(mosaic_proportions, n_replicates),
    prop.aneuploid = runif(4 * n_replicates, 0.3, 0.5),
    dispersal = runif(4 * n_replicates, 0.1, 0.9)
  )
  
  # 3. Combine all dataframes into one big dataframe
  combined_population <- rbind(hardcoded_population, low_level_mosaic_population, programmatic_population)
  
  return(combined_population)
}

# Example usage with custom proportions specified directly
mosaic_proportions <- 1  # Assume 100% low-level mosaic for this example
programmatic_proportions <- c(euploid = 0.2, aneuploid = 0.1, mosaic_high = 0.35, mosaic_low = 0.35)

combined_population_custom <- makepops(
  total_population_size = 11816, 
  n_replicates = 2, 
  mosaic_proportions = mosaic_proportions, 
  programmatic_proportions = programmatic_proportions,
  n_cells = 5, 
  n_chrs = 1
)

print(combined_population_custom)
