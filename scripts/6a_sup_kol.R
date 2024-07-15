# Test statistical difference of aggregate distributions of model error in estimating spatial distribution of blue whales.
dat_ks_agg = 
  dat_in %>%
  select(month, id, den_m_i, den_m_p, den_s_i, den_s_p) %>%
  mutate(month_id = paste0(month, "_", id)) %>% # Add a twist to avoid an odd error.
  group_by(month, month_id) %>% # Aggregate from vessel-area-month to area-month.
  summarize(m_i = mean(den_m_i, na.rm = TRUE),
            m_p = mean(den_m_p, na.rm = TRUE),
            s_i = mean(den_s_i, na.rm = TRUE),
            s_p = mean(den_s_p, na.rm = TRUE)) %>%
  ungroup %>% 
  group_by(month) %>% # Aggregate to month.
  summarize(m_i = sum(m_i, na.rm = TRUE),
            m_p = sum(m_p, na.rm = TRUE),
            s_i = sum(s_i ^ 2, na.rm = TRUE) ^ (1 / 2),
            s_p = sum(s_p ^ 2, na.rm = TRUE) ^ (1 / 2)) %>%
  ungroup %>%
  mutate(n_i = map2(m_i, s_i, ~ qnorm(p = seq(0.01, 0.99, by = 0.01), mean = .x, sd = .y)),
         n_p = map2(m_p, s_p, ~ qnorm(p = seq(0.01, 0.99, by = 0.01), mean = .x, sd = .y)),
         test = map2(n_i, n_p, ks.test),
         test_glance = map(test, glance)) %>%
  select(m_i,
         m_p,
         s_i,
         s_p,
         test_glance) %>%
  unnest(test_glance) %>%
  select(m_i,
         m_p,
         s_i,
         s_p, 
         p.value)

dat_ks_dis =
  dat_in %>%
  select(month, id, den_m_i, den_m_p, den_s_i, den_s_p) %>%
  mutate(month_id = paste0(month, "_", id)) %>% # Add a twist to avoid an odd error.
  group_by(month, month_id) %>% # Aggregate from vessel-area-month to area-month.
  summarize(m_i = mean(den_m_i, na.rm = TRUE),
            m_p = mean(den_m_p, na.rm = TRUE),
            s_i = mean(den_s_i, na.rm = TRUE),
            s_p = mean(den_s_p, na.rm = TRUE)) %>%
  ungroup %>% 
  group_by(month) %>% # Aggregate to month.
  summarize(m_i = sum(m_i, na.rm = TRUE),
            m_p = sum(m_p, na.rm = TRUE),
            s_i = sum(s_i ^ 2, na.rm = TRUE) ^ (1 / 2),
            s_p = sum(s_p ^ 2, na.rm = TRUE) ^ (1 / 2)) %>%
  ungroup %>%
  mutate(n_i = map2(m_i, s_i, ~ qnorm(p = seq(0.01, 0.99, by = 0.01), mean = .x, sd = .y)),
         n_p = map2(m_p, s_p, ~ qnorm(p = seq(0.01, 0.99, by = 0.01), mean = .x, sd = .y)),
         test = map2(n_i, n_p, ks.test),
         test_glance = map(test, glance)) %>%
  select(month, 
         m_i,
         m_p,
         s_i,
         s_p,
         test_glance) %>%
  unnest(test_glance) %>%
  select(-method,
         -alternative) %>%
  pivot_longer(cols = c(statistic, p.value),
               names_to = "var",
               values_to = "val") %>%
  mutate(month = month %>% factor %>% fct_rev,
         var = ifelse(var == "p.value", "P-Value", "Test Statistic"))
  
vis_ks_dis = 
  dat_ks_dis %>%
  ggplot() +
  geom_density_ridges(aes(x = val,
                          y = month,
                          fill = month),
                      scale = 0.90,
                      stat = "binline",
                      bins = 45,
                      draw_baseline = FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  labs(y = "Month") +
  facet_wrap(~ var,
             scales = "free_x") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("out/sup_ks.png",
       width = 6.5,
       dpi = 300)
