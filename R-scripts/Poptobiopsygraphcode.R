# Load the ggplot2 library
library(ggplot2)
library(grid)  # For unit function

# Combine the data for Population Proportions and Biopsy Counts into a single data frame
combined_data <- data.frame(
  Category = factor(c("5% Aneuploidy", "100% Aneuploidy", "Spacer", "Euploid", "Mosaic Low", "Mosaic High", "Aneuploid"),
                    levels = c("5% Aneuploidy", "100% Aneuploidy", "Spacer", "Euploid", "Mosaic Low", "Mosaic High", "Aneuploid")),
  Count = c(7772, 4088, NA, 6281, 1424, 66, 4089),  # NA for Spacer
  Type = factor(c(rep("Embryo Counts", 2), "Spacer", rep("Biopsy Counts", 4)),
                levels = c("Embryo Counts", "Biopsy Counts", "Spacer"))
)

# Create the bar chart with a spacer and an arrow
plot_combined <- ggplot(combined_data, aes(x = Category, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, na.rm = TRUE) +
  scale_fill_manual(values = c("Embryo Counts" = "#0073C2FF", "Biopsy Counts" = "lightblue")) +
  labs(y = "Count") +  # No x-axis label
  scale_x_discrete(breaks = c("5% Aneuploidy", "100% Aneuploidy", "Euploid", "Mosaic Low", "Mosaic High", "Aneuploid")) +  # Exclude "Spacer" from the x-axis labels
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  # Remove the x-axis title
    axis.text.x = element_text(size = 14, color = "black", angle = 45, hjust = 1),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"),  # Increase y-axis text size
    axis.title.y = element_text(size = 16, color = "black"),  # Increase y-axis title size
    panel.grid = element_blank(),  # Remove gridlines
    legend.title = element_blank(),  # Remove the legend title
    legend.text = element_text(size = 12, color = "black"),  # Customize legend text size and color
    legend.position = "right"  # Position the legend on the right
  ) +
  annotate(
    geom = "segment",
    x = 2.7, xend = 3.4,  # Moved right and increased tail length
    y = 2000, yend = 2000,  # Set the y positions; adjust as needed
    arrow = arrow(type = "closed", length = unit(0.3, "inches")),  # Adjusted the arrowhead size
    color = "black",  # Changed the arrow color to black
    size = 1
  )

# Display the plot
print(plot_combined)


# Save the plot to a file
ggsave("Examplepoptobiopsy.png", plot_combined, width = 6, height = 4, bg="white")
