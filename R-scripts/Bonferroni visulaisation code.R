#Visulaisation Boneforroni Code 
# Load necessary libraries
library(ggplot2)

# Load your data
data <- read.csv("output_with_bonferroni.csv")

# Create a line graph with Euploid_Count on the x-axis and Bonferroni_Corrected_PValue on the y-axis
plot <- ggplot(data, aes(x = Euploid_Count, y = Bonferroni_Corrected_PValue)) +
  geom_line(color = "#0073C2FF", size = 1) +  # Use geom_line for a line graph
  labs(x = "Euploid Biopsy Count", y = "Adjusted P-Value") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),        # Increase size of axis text
    axis.title = element_text(size = 14),       # Increase size of axis titles
    panel.grid = element_blank(),               # Remove gridlines
    plot.title = element_blank()                # Remove the title
  )
print(plot)
# Save the plot to a file with dimensions suitable for a single-column document
ggsave("bonferroni_adjusted_p_values_plot.png", plot, width = 6.7, height = 6.7, bg = "white")
