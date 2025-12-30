




# Load required libraries
library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(tmap)
library(stringr)
library(viridis)
library(scales)

# 1. Load shapefile
bd_districts <- st_read("gadm41_BGD_2.shp")  # Update with actual path
bd_districts$NAME_2 <- str_trim(bd_districts$NAME_2)

# 2. Load participant data
participants <- read_csv("District participant map.csv")  # Update with your file
participants$District <- str_trim(participants$District)

# 3. Prepare data: summary and names per district
district_counts <- participants %>%
  group_by(District) %>%
  summarise(Participants = n())

district_names_list <- participants %>%
  group_by(District) %>%
  summarise(Participant_Names = paste(Name, collapse = ", "))

# 4. Merge with shapefile
bd_districts <- bd_districts %>%
  left_join(district_counts, by = c("NAME_2" = "District")) %>%
  left_join(district_names_list, by = c("NAME_2" = "District"))

# 5. Prepare labels
bd_districts$Label_Text <- ifelse(
  is.na(bd_districts$Participant_Names),
  "",
  paste0(bd_districts$NAME_2, "\n", str_wrap(bd_districts$Participant_Names, width = 30))
)

# 6. Split participating and non-participating districts
districts_with_data <- bd_districts %>% filter(!is.na(Participants))
districts_no_data <- bd_districts %>% filter(is.na(Participants))

# 7. Assign fill colors for participant districts
district_colors <- setNames(
  viridis(nrow(districts_with_data), option = "plasma"),
  districts_with_data$NAME_2
)
districts_with_data$fill_color <- district_colors[districts_with_data$NAME_2]

# 8. Calculate brightness of each fill color for label contrast
get_brightness <- function(hex_color) {
  rgb_vals <- col2rgb(hex_color) / 255
  # Luminance formula using Rec. 709
  brightness <- 0.2126 * rgb_vals[1, ] + 0.7152 * rgb_vals[2, ] + 0.0722 * rgb_vals[3, ]
  return(brightness)
}

# Apply brightness and choose text color
brightness_vals <- sapply(districts_with_data$fill_color, get_brightness)
districts_with_data$label_color <- ifelse(brightness_vals > 0.5, "black", "white")

# 9. Plot the full map
ggplot() +
  # Base map
  geom_sf(data = districts_no_data, fill = "lightgray", color = "black") +
  
  # Participant districts
  geom_sf(data = districts_with_data, aes(fill = NAME_2), color = "black") +
  
  # Text labels
  geom_sf_text(data = districts_with_data,
               aes(label = Label_Text, color = label_color),
               size = 2, lineheight = 0.8) +
  
  # Manual fill and label color scales
  scale_fill_manual(values = district_colors, name = "District") +
  scale_color_identity() +
  
  # Title and theme
  ggtitle("N-29 FTC Family") +
  labs(caption = "Prepared by Rasel") +   # ← your credit here
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
    plot.caption = element_text(hjust = 1, size = 6, face = "italic", margin = margin(t = 10)),
    axis.title = element_blank()
  )

ggsave("n29_final.pdf", width = 12, height = 9, dpi = 1800)


