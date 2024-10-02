#Biopsy counts against clinical with 1-2% aneuploidy. 

# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr) # For pivot_longer

# Create the data frame
data <- data.frame(
  Population = c("Simulated Biopsy Counts", "Clinical Biopsy Counts"),
  num_aneuploid_100 = c(4260, 4088),
  num_aneuploid_80 = c(486, 462),
  num_aneuploid_30 = c(979, 799),
  num_aneuploid_02 = c(6135, 6512)
)

# Reshape data for plotting
data_long <- data %>%
  pivot_longer(
    cols = starts_with("num_aneuploid"),
    names_to = "Category",
    values_to = "Value"
  ) %>%
  mutate(
    Category = case_when(
      Category == "num_aneuploid_100" ~ "Aneuploid",
      Category == "num_aneuploid_80" ~ "Mosaic High",
      Category == "num_aneuploid_30" ~ "Mosaic Low",
      Category == "num_aneuploid_02" ~ "Euploid"
    ),
    Category = factor(Category, levels = c("Euploid", "Mosaic Low", "Mosaic High", "Aneuploid"))
  )

# Define colors
colors <- c("Simulated Biopsy Counts" = "#0073C2FF", "Clinical Biopsy Counts" = "#D55E00")

# Create bar plot
resu <- ggplot(data_long, aes(x = Category, y = Value, fill = Population)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    x = NULL, # Remove x-axis label
    y = "Number of Embryos",
    fill = NULL # Remove legend title
  ) +
  scale_x_discrete(
    labels = c("Euploid", "Mosaic Low", "Mosaic High", "Aneuploid")
  ) +
  scale_fill_manual(values = colors) + # Apply custom colors
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank(), # Remove minor gridlines
    plot.title = element_blank(),       # Remove title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14), # Increase x-axis text size
    axis.text.y = element_text(size = 14), # Increase y-axis text size
    legend.title = element_blank()       # Remove legend title
  )

# Save the combined plot to a file with a white background
ggsave("biopsycountscompared1-2.png", plot = resu, width = 6.7, height = 6.7, dpi = 300, bg = "white")
