# Clinical data (for reference)
clinical_data <- c(6512, 799, 499, 4088)

# Set populations (as specified)
set_populations <- list(
  c(11860, 0, 0, 0),          # Population 1
  c(0, 0, 0, 11860),          # Population 2
  c(5930, 0, 0, 5930),        # Population 3
  c(3396, 2229, 2298, 3937),  # Population 4
  c(6512, 799, 499, 4088)     # Population 5 (same as clinical data)
)

# Labels for the set populations
set_labels <- c("Pop 1", "Pop 2", "Pop 3", "Pop 4", "Pop 5 (Clinical)")

# Choose the population to visualize (Population 2)
population_index <- 2  # Population 2

# Function to run chi-squared tests for set populations
run_chi_squared_for_sets <- function(clinical, set_pops) {
  p_values <- numeric(length(set_pops))
  for (i in 1:length(set_pops)) {
    chi_vals <- matrix(c(set_pops[[i]], clinical), ncol = 4, byrow = TRUE)
    test_result <- chisq.test(chi_vals)
    p_values[i] <- test_result$p.value
  }
  return(p_values)
}

# Run chi-squared tests for the set populations
set_p_values <- run_chi_squared_for_sets(clinical_data, set_populations)

# Create a data frame for Population 2
pop_df <- data.frame(
  Category = factor(c("Euploid", "Mosaic Low", "Mosaic High", "Aneuploid"),
                    levels = c("Euploid", "Mosaic Low", "Mosaic High", "Aneuploid")),
  Count = set_populations[[population_index]]
)

# Load ggplot2 library
library(ggplot2)

# Plot the histogram for Population 2
plot2 <- ggplot(pop_df, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  theme_minimal() +
  labs(x = NULL, y = "Count") +  # Removed the x-axis label
  scale_fill_manual(values = c("Euploid" = "#E69F00", "Mosaic Low" = "#56B4E9",
                               "Mosaic High" = "#009E73", "Aneuploid" = "#F0E442"),
                    name = "Control Population 2") +  # Set legend title
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 10),  # Adjust legend title size if needed
    plot.background = element_rect(fill = "white"), # White background for the plot area
    panel.background = element_rect(fill = "white") # White background for the panel area
  )

# Display the plot
print(plot2)

# Save the plot as a PNG file with white background
ggsave(filename = paste0("histogram_control_pop2.png"), plot = plot2, bg = "white", width = 6.7, height = 6.7, dpi = 300)
