# Distribution (Spatiotemporal) | Whales by Information Case
sup_map_wha = 
  dat_in %>% 
  select(year,
         month,
         id, 
         den_m_i, 
         den_m_p) %>% 
  group_by(year,
           month,
           id) %>% 
  summarize(den_m_i = mean(den_m_i, na.rm = TRUE),
            den_m_p = mean(den_m_p, na.rm = TRUE)) %>% 
  group_by(year,
           id) %>% 
  summarize(den_m_i = mean(den_m_i, na.rm = TRUE),
            den_m_p = mean(den_m_p, na.rm = TRUE)) %>% 
  pivot_longer(starts_with("den_m"),
               names_to = 'Information',
               values_to = 'Density') %>% 
  mutate(Information = 
           ifelse(Information == "den_m_i",
                  "ISD",
                  "RSD") %>% 
           factor(.,
                  levels = c("ISD", "RSD"),
                  labels = c("ISD", "RSD")),
         Label = ifelse(Information == "ISD", 
                        "A",
                        "B")) %>% 
  full_join(grid,
            by = "id") %>%
  drop_na(Information) %>%
  st_sf %>%
  ggplot() +
  vis_countries +
  vis_states +
  geom_sf(aes(fill = Density),
          color = NA) +
  geom_text(aes(x = 475000,
                y = 1100000,
                label = Label)) +
  vis_cities_sup +
  vis_names_sup +
  vis_bound_sup +
  scale_fill_viridis_c(limits = c(0.00, 1.50),
                       breaks = c(0.00, 0.75, 1.50)) +
  labs(fill = "Blue Whales, Annualized Mean Estimate") + 
  vis_guide +
  vis_theme +
  facet_grid(~ Information)

ggsave("out/5b_sup_map_wha.png", 
       sup_map_wha, 
       bg = "transparent",
       width = 6.5, 
       dpi = 300)
