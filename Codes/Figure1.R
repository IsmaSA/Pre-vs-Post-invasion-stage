
# Figure 1 (sampling sites) ----- 
setwd("C:/Users/Propietario/Desktop/Pre_Post_invasion")
ibrary(ggplot2)
library(dplyr)
install.packages("ggpspatial")
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

data = read_xlsx("Effect_size_corrected.xlsx")
data <- data[!duplicated(data$site_id), ]

europe_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent == "Europe" | name == "Turkey" | 
           name %in% c("Morocco", "Algeria", "Tunisia", "Libya", "Egypt"))

europe_map$color <- ifelse(europe_map$name %in% c("Morocco", "Algeria", "Tunisia", "Libya", "Egypt", "Russia", "Turkey"),
                           "grey80", "Wheat")

setwd("C:/Users/Propietario/Downloads")
riversData <- st_read("rivers_europe_37253.shp")
riversData$line_width <- scales::rescale(riversData$Strahler, to = c(0.1, 1))
riversData_sf <- st_as_sf(riversData)
riversData1 <-riversData_sf %>% filter(line_width > 0.25)

my_colors <- c("Denmark" = "#e6194B",       # Red
               "UK" = "#3cb44b",            # Green
               "Germany" = "#ffa500",       # Orange
               "Belgium" = "violet",       # Blue
               "Netherlands" = "#008080",   # Teal
               "Sweden" = "#911eb4",        # Purple
               "Luxembourg" = "#46f0f0",    # Cyan
               "Hungary" = "#f032e6",       # Magenta
               "Spain" = "#00ff00",         # Lime
               "Ireland" = "seashell4",       # Light Red for Ireland
               "France" = "blue2",        # Dark Orange
               "Latvia" = "slateblue1")  

a<- ggplot() +
  geom_sf(data = europe_map, aes(fill = color)) +   scale_fill_identity()+ xlim(-11,30) + ylim(36,60)+
  geom_sf(data = riversData1, aes(size = line_width), color = "steelblue2", alpha = 0.8) +
  scale_size_continuous(name = "Strahler", range = c(0.1, 2)) +
  coord_sf() + theme_bw()+ 
  geom_point(data = data, aes(x = Longitude, y = Latitude, color = Country), size = 2) +
  scale_color_manual(values = my_colors) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", style = north_arrow_fancy_orienteering, 
                         which_north = "true", pad_x = unit(0.75, "in"),
                         pad_y = unit(0.5, "in")) + 
  theme(panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))
