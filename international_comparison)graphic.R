library(readr)
library(tidyverse)
library(ggrepel)
library(colorspace)

int_data <- read_csv("international_comparisons.csv")

int_data <-int_data[order(int_data$forest_cover), ] 
int_data$jurisdiction <- factor(int_data$jurisdiction, levels = int_data$jurisdiction)

# cols <- c("seagreen1", "seagreen3", "paleturquoise","paleturquoise3",
#           "skyblue2", "skyblue2", "royalblue2", "royalblue3", 
#           "royalblue4", "purple1", "purple2", "purple3", "purple4")

ggplot(int_data, aes(x = jurisdiction, y = forest_cover, color = jurisdiction)) +

  geom_segment(aes(x = jurisdiction, 
                   xend = jurisdiction, 
                   y = 0, 
                   yend = forest_cover),
               color=ifelse(int_data$jurisdiction %in% c("Kostroma Province","Average for European Russia"), 
                            "orange", "skyblue"), 
               size=ifelse(int_data$jurisdiction %in% c("Kostroma Province","Average for European Russia"), 
                           1, .75)) +
  geom_point(color=ifelse(int_data$jurisdiction %in% c("Kostroma Province","Average for European Russia"), 
                          "orange", "blue"), 
             size=ifelse(int_data$jurisdiction %in% c("Kostroma Province","Average for European Russia"), 
                         2, 1.5)) +
    labs(title="Comparative Forest Cover, 1910", 
       caption="Data source",
       x = NULL,
       y = "Forest cover") +
  # scale_color_manual(values = cols) +

  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))