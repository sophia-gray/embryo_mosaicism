# Load necessary libraries
library(ggplot2)

# Read the data from the CSV file
df <- read.csv("summary_results_trials.csv")  # Ensure the correct file path is used

# Create the line plot
plot1 <- ggplot(df, aes(x = PopulationID, y = ChiSquaredPValue)) +
  geom_line(color = "blue") +      # Add a line for the p-values
  geom_point(color = "red") +      # Add points for each population
  labs(title = "Population ID vs. Chi-Squared p-values",
       x = "Population ID",
       y = "Chi-Squared p-value") +
  theme_minimal(base_size = 15) +  # Clean theme with larger base font size
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank())  # Remove minor gridlines

print(plot1)
# Save the plot
ggsave("line_plot_0%additonalpopstrial.png", plot1, width = 6.7, height = 6.7, dpi = 300, bg = "white")


