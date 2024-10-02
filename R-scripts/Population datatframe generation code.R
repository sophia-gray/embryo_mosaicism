# Define constants
total_population_size <- 11860
start_euploid <- 6397
end_euploid <- 6616
start_aneuploid_30 <- 914
end_aneuploid_30 <- 695
prop_aneuploid_100 <- 4088
prop_aneuploid_80 <- 462

# Number of populations to generate
num_populations <- 20  # Adjust as needed

# Create empty data frame
populations_df <- data.frame()

# Generate populations
for (i in 0:(num_populations - 1)) {
  # Calculate step size for euploid and aneuploid_30
  step_size_euploid <- (end_euploid - start_euploid) / (num_populations - 1)
  step_size_aneuploid_30 <- (start_aneuploid_30 - end_aneuploid_30) / (num_populations - 1)
  
  # Calculate current values
  prop_euploid <- round(start_euploid + i * step_size_euploid)
  prop_aneuploid_30 <- round(start_aneuploid_30 - i * step_size_aneuploid_30)
  
  # Ensure non-negative values
  prop_euploid <- max(0, prop_euploid)
  prop_aneuploid_30 <- max(0, prop_aneuploid_30)
  
  # Calculate remaining proportion for prop_aneuploid_90
  remaining_proportion <- total_population_size - (prop_euploid + prop_aneuploid_100 + prop_aneuploid_80 + prop_aneuploid_30)
  
  # Ensure remaining proportion is non-negative
  remaining_proportion <- max(0, remaining_proportion)
  
  # Append to dataframe
  populations_df <- rbind(populations_df, data.frame(
    PopulationID = i + 1,
    Population_name = paste("Population", i + 1),
    prop_euploid = prop_euploid,
    prop_aneuploid_100 = prop_aneuploid_100,
    prop_aneuploid_90 = remaining_proportion,
    prop_aneuploid_80 = prop_aneuploid_80,
    prop_aneuploid_70 = 0,
    prop_aneuploid_60 = 0,
    prop_aneuploid_50 = 0,
    prop_aneuploid_40 = 0,
    prop_aneuploid_30 = prop_aneuploid_30,
    prop_aneuploid_20 = 0,
    prop_aneuploid_10 = 0
  ))
}

# Print the dataframe
print(populations_df)


# Save to CSV
write.csv(populations_df, "populations_df_generated_0%euploid.csv", row.names = FALSE)


