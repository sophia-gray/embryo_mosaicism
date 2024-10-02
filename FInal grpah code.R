# Load required packages
library(ggplot2)
library(dplyr)

# Create the data frame for the biopsy outcomes
data_biopsy <- data.frame(
  Class = c("Euploid", "Mosaic Low", "Mosaic High", "Aneuploid"),
  Outcome = c(6156, 961, 491, 4252),
  Source = "Simulated Mosaic Population"
)

data_irmet <- data.frame(
  Class = c("Euploid", "Mosaic Low", "Mosaic High", "Aneuploid"),
  Outcome = c(6152, 799, 462, 4088),
  Source = "IRMET Data"
)

# Combine the data into one data frame
data_combined <- bind_rows(data_biopsy, data_irmet)

# Ensure Class is a factor with specified levels
data_combined <- data_combined %>%
  mutate(Class = factor(Class, levels = c("Euploid", "Mosaic Low", "Mosaic High", "Aneuploid")))

# Create the plot
plot1 <- ggplot(data_combined, aes(x = Class, y = Outcome, fill = Source, color = Source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    x = NULL,           # Remove x-axis label
    y = "Number of Embryos",
    title = NULL        # Remove title
  ) +
  scale_fill_manual(values = c("Simulated Mosaic Population" = "#E69F00", "IRMET Data" = "#0073C2FF")) +
  scale_color_manual(values = c("Simulated Mosaic Population" = "#E69F00", "IRMET Data" = "#0073C2FF")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    legend.title = element_blank()  # Remove legend title
  )

# Print the plot
print(plot1)

# Save the plot
ggsave("biopsy_vs_irmet_plot_FINAL.png", plot = plot1, width = 6.7, height = 6.7, dpi = 300, bg = "white")

