
# To get recent ad hoc runs (2023/10/13):
# dat_voi = read_csv("data_intermediate/dat_out_opt_alt.csv")

# VOI calculation is non-obvious.

vis_voi = 
  dat_out_opt_alt %>% 
  select(target, mor_m_p_cou, mor_m_a_cou, cos_i, cos_p, voi) %>% 
  pivot_longer(cols = c(cos_i, cos_p, voi),
               names_to = "var",
               values_to = "val") %>% 
  mutate(facet = ifelse(var == "voi", "Value of Information", "Costs"),
         label = ifelse(var == "cos_i", "ISD", ifelse(var == "cos_p", "RSD", NA)),
         label_y = ifelse(var == "cos_i", 541, 374)) %>% 
  ggplot() +
  geom_line(aes(x = target,
                y = val / 1000000,
                color = var),
            linewidth = 0.75) +
  geom_text(aes(x = 3.7,
                y = label_y,
                label = label,
                color = var),
            hjust = 0) +
  scale_x_reverse(expand = c(0.1, 0.1)) +
  scale_y_continuous(limits = c(0, 600),
                     expand = c(0, 0)) +
  scale_color_manual(values = c(vis_pal_rff, "red")) +
  labs(x = "Fatal Ship Strikes",
       y = "US$M") +
  facet_wrap(~ facet) +
  theme_pubr() +
  theme(legend.position = "none")

vis_voi_attainment = 
  dat_out_opt_alt %>% 
  select(target, mor_m_p_cou, mor_m_a_cou, cos_i, cos_p, voi) %>% 
  pivot_longer(cols = c(cos_i, cos_p, voi),
               names_to = "var",
               values_to = "val") %>% 
  mutate(target = (target - max(target)) * -1,
         facet = ifelse(var == "voi", "Value of Information", "Costs"),
         label = ifelse(var == "cos_i", "ISD", ifelse(var == "cos_p", "RSD", NA)),
         label_y = ifelse(var == "cos_i", 541, 374)) %>% 
  ggplot() +
  geom_line(aes(x = target,
                y = val / 1000000,
                color = var),
            linewidth = 0.75) +
  geom_text(aes(x = 2.4,
                y = label_y,
                label = label,
                color = var),
            hjust = 0) +
  scale_x_continuous(expand = c(0.1, 0.1),
                     breaks = seq(0, 2.5, by = 0.5)) +
  scale_y_continuous(limits = c(0, 600),
                     expand = c(0, 0)) +
  scale_color_manual(values = c(vis_pal_rff, "red")) +
  labs(x = "Averted Fatal Ship Strikes",
       y = "US$M") +
  facet_wrap(~ facet) +
  theme_pubr() +
  theme(legend.position = "none")

ggsave("out/5g_vis_voi.png", 
       vis_voi, 
       bg = "transparent",
       dpi = 300, 
       width = 6.5, 
       height = 4)

ggsave("out/5g_vis_voi_attainment.png", 
       vis_voi_attainment, 
       bg = "transparent",
       dpi = 300, 
       width = 6.5, 
       height = 4)
