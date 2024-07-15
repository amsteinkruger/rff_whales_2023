#  Regional Fatal Strikes (Spatiotemporal)
vis_map_mor = 
  dat_in %>% # Snag pre-processing estimates of fatal strikes under the status quo.
  select(id, mor_m_i_ref, mor_m_p_ref) %>% 
  group_by(id) %>% 
  summarize(mor_m_i_ref = sum(mor_m_i_ref, na.rm = TRUE),
            mor_m_p_ref = sum(mor_m_p_ref, na.rm = TRUE)) %>% 
  pivot_longer(starts_with("mor_m"),
               names_to = 'Information',
               values_to = 'Fatal Strikes') %>% 
  mutate(Information = 
           ifelse(Information == "mor_m_i_ref",
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
  geom_sf(aes(fill = `Fatal Strikes`),
          color = NA) +
  geom_text(aes(x = 250000,
                y = 50000,
                label = Label)) +
  vis_cities +
  vis_names_if +
  vis_names_else +
  vis_bound +
  scale_fill_viridis_c(limits = c(0.00, 0.10),
                       breaks = c(0.00, 0.05, 0.10),
                       oob = scales::squish) +
  labs(fill = "Fatal Strikes") +
  vis_guide +
  vis_theme +
  facet_grid(~ Information)

ggsave("out/5e_vis_map_mor.png", 
       vis_map_mor, 
       bg = "transparent",
       width = 6.5,
       dpi = 300)
