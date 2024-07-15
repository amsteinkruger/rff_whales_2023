# Map locations by percent rank mortality.
vis_per =
  dat_in %>% 
  group_by(month,
           id) %>% 
  summarize(mor_m_i = sum(mor_m_i_ref), # This is where you could keep SD to get probabilistic estimates instead.
            mor_m_p = sum(mor_m_p_ref)) %>% 
  ungroup %>% 
  mutate(per_i = percent_rank(mor_m_i),
         per_p = percent_rank(mor_m_p)) %>% 
  group_by(id) %>% 
  summarize(per_i = mean(per_i),
            per_p = mean(per_p)) %>% 
  ungroup %>% 
  select(id,
         per_i,
         per_p) %>% 
  pivot_longer(cols = c(per_i, per_p),
               names_to = "Information",
               values_to = "Percent Rank") %>% 
  mutate(Information =
           ifelse(Information == "per_i",
                  "Reference",
                  "Counterfactual") %>%
           factor(levels = c("Reference", "Counterfactual"),
                  labels = c("Reference", "Counterfactual")),
         Label = ifelse(Information == "Reference", 
                        "A",
                        "B"),
         Metric = "Fatal Strikes (Percent Rank)") %>%
  left_join(grid,
            by = "id") %>% 
  st_sf %>% 
  ggplot() +
  vis_countries +
  vis_states + 
  geom_sf(aes(fill = `Percent Rank` * 100),
          color = NA) +
  geom_text(aes(x = 250000,
                y = 50000,
                label = Label)) +
  vis_cities +
  vis_names_if +
  vis_names_else +
  vis_bound +
  scale_fill_viridis(option = "D",
                     limits = c(0, 100),
                     breaks = c(0, 50, 100)) +
  theme_pubr() +
  guides(fill = guide_colorbar(title = element_blank(),
                               barwidth = 0.5,
                               barheight = 10.0,
                               direction = "vertical",
                               ticks.colour = "black",
                               ticks.linewidth = 0.25,
                               frame.colour = "black",
                               frame.linewidth = 0.25)) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        legend.margin = margin(0, 0, 0, 0, unit = "pt"),
        legend.box.margin = margin(0, 0, 0, -6, unit = "pt"),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.background = element_rect(fill = "grey50", color = NA)) +
  facet_grid(Metric ~ Information,
             switch = "y")

vis_int =
  dat_in %>% 
  group_by(month,
           id) %>% 
  summarize(mor_m_i = sum(mor_m_i_ref), # This is where you could keep SD to get probabilistic estimates instead.
            mor_m_p = sum(mor_m_p_ref)) %>% 
  ungroup %>% 
  mutate(per_i = percent_rank(mor_m_i),
         per_p = percent_rank(mor_m_p),
         bin_i = ifelse(per_i > 0.90, 1, 0),
         bin_p = ifelse(per_p > 0.90, 1, 0)) %>% # 0.90 reflects a representative extent of 10%.
  group_by(id) %>% 
  summarize(bin_i = sum(bin_i, na.rm = TRUE),
            bin_p = sum(bin_p, na.rm = TRUE)) %>% 
  select(id,
         bin_i,
         bin_p) %>% 
  pivot_longer(cols = c(bin_i, bin_p),
               names_to = "Information",
               values_to = "Intervention") %>% 
  mutate(Information =
           ifelse(Information == "bin_i",
                  "Reference",
                  "Counterfactual") %>%
           factor(levels = c("Reference", "Counterfactual"),
                  labels = c("Reference", "Counterfactual")),
         Label = ifelse(Information == "Reference",
                        "A",
                        "B"),
         Metric = "Intervention (Months)") %>%
  left_join(grid,
            by = "id") %>% 
  st_sf %>% 
  ggplot() +
  vis_countries +
  vis_states + 
  geom_sf(aes(fill = Intervention),
          color = NA) +
  geom_text(aes(x = 250000,
                y = 50000,
                label = Label)) +
  vis_cities +
  vis_names_if +
  vis_names_else +
  vis_bound +
  scale_fill_viridis(option = "cividis",
                     breaks = c(0, 6, 12))  +
  theme_pubr() +
  guides(fill = guide_colorbar(title = element_blank(),
                               barwidth = 0.5,
                               barheight = 10.0,
                               direction = "vertical",
                               ticks.colour = "black",
                               ticks.linewidth = 0.25,
                               frame.colour = "black",
                               frame.linewidth = 0.25)) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        legend.margin = margin(0, 0, 0, 0, unit = "pt"),
        # legend.box.margin = margin(0, 5, 0, -5, unit = "pt"),
        # plot.margin = unit(c(-14, 45, 0, 45), "pt"),
        panel.background = element_rect(fill = "grey50", color = NA)) + # ,
  # strip.text.x = element_blank(), 
  # strip.background.x = element_blank()) +
  facet_grid(Metric ~ Information,
             switch = "y")

vis_map_per_com = 
  ggarrange(vis_per,
            vis_int,
            nrow = 2)

ggsave("./out/vis_map_per.png", 
       vis_per, 
       width = 6.5,
       dpi = 300)

ggsave("./out/vis_map_int.png", 
       vis_int, 
       width = 6.5,
       dpi = 300)

ggsave("./out/vis_map_per_com.png", 
       vis_map_per_com, 
       width = 6.5,
       dpi = 300)
