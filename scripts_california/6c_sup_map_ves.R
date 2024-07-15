# Vessels
#  Time

vis_map_hou = 
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
  geom_text(aes(x = 250000,
                y = 50000,
                label = Label)) +
  vis_cities +
  vis_names_if +
  vis_names_else +
  vis_bound +
  scale_fill_viridis_c(limits = c(0, 800),
                       breaks = c(0, 400, 800),
                       oob = scales::squish) + 
  labs(fill = "Hours") +
  vis_guide +
  vis_theme +
  facet_wrap(~ facet)

# Speed
vis_map_spe = 
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
  geom_text(aes(x = 250000,
                y = 50000,
                label = Label)) +
  vis_cities +
  vis_names_if +
  vis_names_else +
  vis_bound +
  scale_fill_viridis_c(option = "C",
                       limits = c(5, 20),
                       breaks = c(5, 10, 15, 20),
                       oob = scales::squish) + 
  labs(fill = "Speed") +
  vis_guide +
  vis_theme +
  facet_wrap(~ facet)

vis_map_ves_com = 
  ggarrange(vis_map_hou,
            vis_map_spe,
            ncol = 2)

ggsave("./out/vis_map_ves_com.png", 
       vis_map_ves_com, 
       width = 6.5, 
       dpi = 300)
