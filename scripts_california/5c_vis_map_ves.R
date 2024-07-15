# Vessels
#  Transit Hours
sup_map_hou = 
  dat_in %>% 
  group_by(id,
           month, 
           year) %>%
  summarize(t = sum(t, na.rm = TRUE)) %>%
  group_by(id,
           year) %>% 
  summarize(t = mean(t, na.rm = TRUE)) %>% 
  ungroup %>%
  full_join(grid,
            by = c("id")) %>% 
  mutate(`Transit Hours` = t * 1 / 3600,
         facet = "Vessel Transit Hours",
         Label = "A") %>% 
  select(year,
         facet,
         Label,
         `Transit Hours`,
         geometry) %>% 
  st_sf %>% 
  ggplot() +
  vis_countries +
  vis_states + 
  geom_sf(aes(fill = `Transit Hours`),
          color = NA) +
  # geom_text(aes(x = 475000,
  #               y = 1100000,
  #               label = Label)) +
  labs(fill = "Hours") +
  vis_cities_sup +
  vis_names_sup +
  vis_bound_sup +
  scale_fill_viridis_c(limits = c(0, 1000),
                       breaks = c(0, 500, 1000),
                       oob = scales::squish) +
  vis_guide +
  vis_theme # +
  # facet_wrap(~ facet)

# Speed
sup_map_spe = 
  dat_in %>% 
  group_by(id) %>% 
  summarize(speed = mean(spe_m, na.rm = TRUE)) %>% 
  ungroup %>%
  full_join(grid,
            by = c("id")) %>% 
  mutate(facet = "Vessel Speeds",
         Label = "B") %>% 
  select(speed,
         facet,
         Label,
         geometry) %>% 
  st_sf %>% 
  ggplot() +
  vis_countries +
  vis_states + 
  geom_sf(aes(fill = `speed`),
          color = NA) +
  # geom_text(aes(x = 475000,
  #               y = 1100000,
  #               label = Label)) +
  vis_cities_sup +
  vis_names_sup +
  vis_bound_sup +
  scale_fill_viridis_c(limits = c(5, 25),
                       breaks = c(5, 15, 25),
                       oob = scales::squish) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(fill = "Knots") +
  vis_guide +
  vis_theme # +
  # facet_wrap(~ facet)

sup_map_ves_com = 
  ggarrange(sup_map_hou,
            sup_map_spe,
            ncol = 2)

ggsave("out/5c_sup_map_hou.png", 
       sup_map_hou, 
       bg = "transparent",
       width = 6.5, 
       dpi = 300)

ggsave("out/5c_sup_map_spe.png", 
       sup_map_spe, 
       bg = "transparent",
       width = 6.5, 
       dpi = 300)

ggsave("out/sup_map_ves_com.png", 
       sup_map_ves_com, 
       bg = "transparent",
       width = 6.5, 
       dpi = 300)
