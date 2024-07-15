# Distribution (Spatiotemporal) | Whales by Information Case
vis_map_wha = 
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
                  "Reference",
                  "Counterfactual") %>% 
           factor(.,
                  levels = c("Reference", "Counterfactual"),
                  labels = c("Reference", "Counterfactual")),
         Label = ifelse(Information == "Reference", 
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
  geom_text(aes(x = 250000,
                y = 50000,
                label = Label)) +
  vis_cities +
  vis_names_if +
  vis_names_else +
  vis_bound +
  scale_fill_viridis_c(limits = c(0.00, 1.00),
                       breaks = c(0.00, 1.00)) +# , 2.00)) +
  labs(fill = "Whales") + 
  vis_guide +
  vis_theme +
  facet_grid(~ `Information`)

ggsave("./out/vis_map_wha.png", 
       vis_map_wha, 
       width = 6.5, 
       dpi = 300)
