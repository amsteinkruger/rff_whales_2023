# Pull in data to show countries, states, and cities.
#  Pull country polygons from rnaturalearth.
countries = 
  ne_download(scale = 10, 
              type = "countries", 
              category = "cultural", 
              returnclass = "sf") %>% 
  st_transform(crs = crs) %>% 
  select(country = ADMIN) %>% 
  filter(country == "Canada" | 
           country == "United States of America" | 
           country == "Mexico")

#  Pull state and province polygons from rnaturalearth.
states = 
  ne_download(scale = 10, 
              type = "states", 
              category = "cultural", 
              returnclass = "sf") %>% 
  st_transform(crs = crs) %>% 
  select(postal,
         admin) %>% 
  filter(postal %in% c("CA", "OR", "WA") & admin == "United States of America")

#  Pull city polygons from rnaturalearth.
cities = 
  ne_download(scale = 10, 
              type = "populated_places", 
              category = "cultural", 
              returnclass = "sf") %>% 
  st_transform(crs = crs) %>% 
  select(place = NAME,
         adm1 = ADM1NAME)

# Get reference data into layers. 
vis_countries = 
  geom_sf(data = countries, 
          fill = "grey85", 
          color = NA)

vis_states = 
  geom_sf(data = states,
          fill = "grey85",
          color = NA)

# Filter cities down to places of interest for subregional maps.
vis_cities =
  cities %>% 
  filter(place == "San Francisco" & adm1 == "California" |
         place == "Monterey" & adm1 == "California" | 
         place == "Santa Barbara" & adm1 == "California" |
         place == "Los Angeles" & adm1 == "California") %>% 
  geom_sf(data = .,
          size = 1.5,
          shape = 21,
          color = "black",
          fill = "white")

# Offset labels for places that aren't Los Angeles.
vis_names_if = 
  cities %>% 
  filter(place == "Los Angeles" & adm1 == "California") %>% 
  geom_sf_text(data = . , 
               aes(label = place),
               hjust = "center",
               nudge_x = 25000,
               nudge_y = 13000,
               size = 2.5)

# Offset labels for places that are Los Angeles.
vis_names_else =
  cities %>% 
  filter(place == "San Francisco" & adm1 == "California" |
         place == "Monterey" & adm1 == "California" | 
         place == "Santa Barbara" & adm1 == "California") %>%
  geom_sf_text(data = ., 
               aes(label = place),
               hjust = "left",
               nudge_x = 9000,
               nudge_y = 5000,
               size = 2.5)

# Set bounding box for the subregion of interest.
vis_bound =
  coord_sf(xlim = c(-300000, 275000), 
           ylim = c(-600000, 75000), 
           expand = FALSE)

# Set guides and theme.
#  Guides
vis_guide = 
  guides(fill = guide_colorbar(barwidth = 15.00,
                               barheight = 0.75,
                               title.position = "bottom",
                               # title.hjust = 0.60,
                               ticks.colour = "black",
                               ticks.linewidth = 0.25,
                               frame.colour = "black",
                               frame.linewidth = 0.25))

#  Theme
vis_theme = 
  theme_pubr() +
  theme(legend.position = "bottom",
        legend.justification = "center",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "pt"),
        panel.background = element_rect(fill = "grey50", color = NA),
        legend.margin = margin(0, 0, 0, 0, unit = "pt"),
        legend.box.margin = margin(-8, 0, 0, 0, unit = "pt"),
        legend.title.align = 0.50)

# Set two-color palette per RFF.
vis_pal_rff = c("#04273C", "#88C4F4")

# Set up alternative cities, labels for cities, and bounds for figures.
# Filter cities.
vis_cities_sup = 
  cities %>% 
  filter(place == "Seattle" & adm1 == "Washington" | 
           place == "Portland" & adm1 == "Oregon" |
           place == "San Francisco" & adm1 == "California" |
           place == "Los Angeles" & adm1 == "California" | 
           place == "San Diego" & adm1 == "California") %>% 
  geom_sf(data = .,
          size = 1.5,
          shape = 21,
          color = "black",
          fill = "white")

# Offset labels.
vis_names_sup =
  cities %>% 
  filter(place == "Seattle" & adm1 == "Washington" | 
           place == "Portland" & adm1 == "Oregon" |
           place == "San Francisco" & adm1 == "California" |
           place == "Los Angeles" & adm1 == "California" | 
           place == "San Diego" & adm1 == "California") %>% 
  geom_sf_text(data = ., 
               aes(label = place),
               hjust = "left",
               vjust = "center",
               nudge_x = 15000,
               size = 3.00)

# Set bounding.
vis_bound_sup =
  coord_sf(xlim = c(-850000, 550000), 
           ylim = c(-850000, 1175000), 
           expand = FALSE)
