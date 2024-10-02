#Visualisation of fractioned pops 1-8% 

# Load necessary libraries
library(ggplot2)

# Data
PopulationID <- c(50, 51, 52, 53, 54, 55, 56)
Bonferroni_PValue <- c(3.98E-101, 1.94E-82, 7.71E-71, 7.33E-50, 1.39E-33, 1.58E-23, 1.29E-14)

# Create a data frame
df <- data.frame(PopulationID, Bonferroni_PValue)

# Plot
results <- ggplot(df, aes(x = as.factor(PopulationID), y = Bonferroni_PValue)) +
  geom_line(color = "#0073C2FF", size = 1) +
  geom_point(color = "#0073C2FF", size = 3) +
  scale_y_log10() +  # Use log scale on y-axis to better visualize small p-values
  labs(
    x = "Population",
    y = "Adjusted P-Value",
    title = "Bonferroni P-Values by Population ID"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 14),  # Axis labels
        axis.text = element_text(size = 12),   # Axis text
        plot.title = element_blank(),  # Title text
        legend.text = element_text(size = 12))  
print(results)

# Save the plot to a file with a white background
ggsave("plot_1-8.png", plot = results, width = 6.7, height = 6.7, dpi = 300, bg = "white")