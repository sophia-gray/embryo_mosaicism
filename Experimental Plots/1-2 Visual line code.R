# Load required packages
library(ggplot2)
library(dplyr)

# Your existing data creation and manipulation code
data <- data.frame(
  PopulationID = 57:87,
  PopulationName = paste("Population", 57:87),
  num_aneuploid_02 = seq(3020, 20, by = -100),
  num_aneuploid_01 = seq(3220, 6220, by = 100),
  Bonferroni_P_Value = c(
    2.18E-12, 3.47E-11, 1.25E-14, 5.46E-10, 2.68E-11, 1.29E-12, 
    7.69E-10, 1.54E-09, 1.26E-07, 7.10E-09, 6.14E-11, 3.84E-10, 
    8.37E-09, 9.39E-12, 2.37E-10, 1.51E-08, 2.77E-10, 6.11E-08, 
    2.77E-09, 4.74E-09, 2.11E-08, 7.41E-06, 1.49E-06, 1.71E-06, 
    5.98E-06, 6.85E-05, 3.84E-06, 5.15E-07, 6.29E-05, 7.91E-06, 
    9.80E-06
  )
)

data_long <- data %>%
  select(PopulationID, PopulationName, num_aneuploid_01, num_aneuploid_02) %>%
  pivot_longer(
    cols = starts_with("num_aneuploid"),
    names_to = "Aneuploidy_Level",
    values_to = "Num_Aneuploid"
  ) %>%
  mutate(
    Highlight = ifelse(PopulationID == 82, "Population 82", NA)
  )

data_long <- data_long %>%
  mutate(PopulationName = factor(PopulationName, levels = paste("Population", 57:87)))

line_colors <- c("num_aneuploid_01" = "#E69F00", "num_aneuploid_02" = "#0073C2FF")
highlight_color <- "black"

plot <- ggplot(data_long, aes(x = PopulationName, y = Num_Aneuploid, color = Aneuploidy_Level, group = Aneuploidy_Level)) +
  geom_line(size = 1) +
  geom_point(aes(color = ifelse(is.na(Highlight), Aneuploidy_Level, Highlight)), size = 3) +
  scale_color_manual(
    values = c(line_colors, "Population 82" = highlight_color),
    labels = c("1% Aneuploidy", "2% Aneuploidy", "Population 82")
  ) +
  scale_x_discrete(
    breaks = paste("Population", c(57, 62, 67, 72, 77, 82, 87)),
    labels = c("Population 57", "Population 62", "Population 67", "Population 72", "Population 77", "Population 82", "Population 87")
  ) +
  labs(
    x = NULL,
    y = "Number of Euploid Embryos",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(override.aes = list(size = 2)))

# Save the plot
ggsave(filename = "1-2line_plot.png", plot = plot, width = 6.7, height = 6.7, dpi = 300, bg="white")
