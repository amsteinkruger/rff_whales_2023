# 2.5d plots of outcomes over a complete decision space.
#  Transform data for convenient visualization.
dat_gr = 
  dat_out %>%
  filter(ext > 0.01) %>% 
  pivot_longer(cols = c(m_i, m_p, c_i, c_p)) %>%
  separate(col = name,
           into = c("Outcome", "Information"),
           sep = "_") %>%
  mutate(Outcome = ifelse(Outcome == "m", "Fatal Strikes", "Cost"),
         Information = ifelse(Information == "i", "Reference", "Counterfactual"))

#  Visualize cost.
vis_gr_c = 
  dat_gr %>% 
  filter(Outcome == "Cost") %>% 
  ggplot() + 
  geom_raster(aes(x = int,
                  y = ext,
                  fill = value / 1000000)) + 
  scale_x_reverse(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis(option = "A",
                     direction = -1) +
  labs(x = "Policy Intensity (Speed Limit, kt)",
       y = "Policy Extensity (% US EEZ)",
       fill = "Lower-Bound Cost (US$M)") +
  facet_wrap( ~ Information) +
  guides(fill = guide_colorbar(barwidth = 15,
                               barheight = 0.5,
                               ticks = FALSE)) +
  theme_pubr()

#  Visualize fatal strikes.
vis_gr_m = 
  dat_gr %>% 
  filter(Outcome == "Fatal Strikes") %>% 
  ggplot() + 
  geom_raster(aes(x = int,
                  y = ext,
                  fill = value)) + 
  scale_x_reverse(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis(option = "A") +
  labs(x = "Policy Intensity (Speed Limit, kt)",
       y = "Policy Extensity (% US EEZ)",
       fill = "Fatal Strikes (p = 0.90)") +
  facet_wrap( ~ Information) +
  guides(fill = guide_colorbar(barwidth = 15,
                               barheight = 0.5,
                               ticks = FALSE,
                               reverse = TRUE)) +
  theme_pubr()

# Save both visualizations.
ggsave("./out/sup_gr_c.png",
       vis_gr_c,
       width = 9.0,
       height = 5,
       dpi = 300)

ggsave("./out/sup_gr_m.png",
       vis_gr_m,
       width = 9.0,
       height = 5,
       dpi = 300)
