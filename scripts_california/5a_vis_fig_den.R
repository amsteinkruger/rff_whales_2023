# Coefficients of Variation for Density by Month
vis_fig_den = 
  data %>% 
  filter(inf != "p") %>% 
  group_by(year,
           month,
           id,
           inf) %>%
  summarize(den_m = mean(den_m, na.rm = TRUE),
            den_s = mean(den_s, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Information = 
           ifelse(inf == "i",
                  "Reference",
                  "Counterfactual") %>% 
           factor(levels = c("Reference", "Counterfactual"),
                  labels = c("Reference", "Counterfactual")),
         CV = den_s / den_m,
         Month = 
           month %>% 
           factor(levels = seq(1, 12),
                  labels = c("January", 
                             "February", 
                             "March", 
                             "April", 
                             "May", 
                             "June", 
                             "July", 
                             "August", 
                             "September", 
                             "October", 
                             "November", 
                             "December")) %>% 
           fct_rev) %>% 
  ggplot(aes(x = CV,
             y = Month,
             fill = stat(x))) +
  geom_density_ridges_gradient(stat = "binline",
                               bins = 100,
                               scale = 0.85) +
  labs(x = "Coefficient of Variation",
       y = "Month") +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 1),
                     breaks = c(0, 1)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_viridis_c(limits = c(0, 1)) + 
  theme_pubr() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        panel.spacing = unit(1, "lines")) +
  vis_guide +
  facet_grid( ~ Information)

ggsave("out/5a_vis_fig_den.png", 
       vis_fig_den, 
       bg = "transparent",
       width = 6.5,
       dpi = 300)
