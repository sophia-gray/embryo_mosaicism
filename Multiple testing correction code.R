#Multiple testing correction 

# Read in the data
data <- read.csv("Pops for multiple testing correction.csv")

# Display the first few rows to ensure it's read correctly
tail(data)

# Perform Bonferroni correction
data$Bonferroni_Corrected_PValue <- p.adjust(data$ChiSquaredPValue, method = "bonferroni")

# Save the updated data to a new CSV file
write.csv(data, "output_with_bonferroni.csv", row.names = FALSE)

# Display the first few rows of the new data
tail(data)
