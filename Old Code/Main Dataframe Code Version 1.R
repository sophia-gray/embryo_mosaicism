# Make pops version 1 allows for the proportions of populations to be specified within the function - but then this will be across all the pop dataframes created so would not work.  

makepops <- function(total_population_size = 11816, n_replicates = 1, euploid_prop = 0.4, aneuploid_prop = 0.4, mosaic_high_prop = 0.1, mosaic_low_prop = 0.1, n_cells = 5, n_chrs = 1) {
  
  # Define constants
  ANEUPLOIDY_RANGE <- seq(0, 1, 0.1)
  DISPERSAL_RANGE <- seq(0, 1, 0.1)
  BIOPSY_SIZE <- n_cells  # Fixed biopsy size
  
  # Proportions list based on user inputs
  proportions_list <- list(
    c(euploid = euploid_prop, aneuploid = aneuploid_prop, mosaic_high = mosaic_high_prop, mosaic_low = mosaic_low_prop)
  )
  
  # Check if proportions sum to 1
  for (proportions in proportions_list) {
    if (sum(proportions) != 1) {
      stop("Proportions must sum to 1.")
    }
  }
  
  # Generate combinations of embryo types and biopsy sizes for replicates
  combs <- expand.grid(
    n.replicates = 1:n_replicates,
    embryo_type = names(proportions_list[[1]])
  )
  
  # Generate the programmatic population dataframe
  programmatic_population <- lapply(1:nrow(combs), function(i) {
    comb <- combs[i,]
    embryo_type <- comb$embryo_type
    proportion <- proportions_list[[1]][[embryo_type]]
    
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
  
  # 1. Hard coded Populations
  hardcoded_population <- data.frame(
    simulation_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
    n.cells = rep(BIOPSY_SIZE, 12),
    n.chrs = rep(n_chrs, 12),
    total_population_size = rep(total_population_size, 12),
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
  
  # 3. Low-Level Mosaic Populations
  low_level_mosaic_population <- data.frame(
    simulation_id = rep(1:n_replicates, each = 4),
    n.cells = rep(BIOPSY_SIZE, 4 * n_replicates),
    n.chrs = rep(n_chrs, 4 * n_replicates),
    total_population_size = rep(total_population_size, 4 * n_replicates),
    rng.seed = rep(42:(41 + n_replicates), each = 4),
    embryo_type = rep("mosaic_low", 4 * n_replicates),
    proportion = rep(1, 4 * n_replicates),  # Assume 100% low-level mosaic for this example
    prop.aneuploid = runif(4 * n_replicates, 0.3, 0.5),
    dispersal = runif(4 * n_replicates, 0.1, 0.9)
  )
  
  # 4. Combine all dataframes into one big dataframe
  combined_population <- rbind(hardcoded_population, low_level_mosaic_population, programmatic_population)
  
  return(combined_population)
}

print(combined_population)
