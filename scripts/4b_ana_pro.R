# Process full dataset to set up optimization. 
dat_in = 
  data %>% 
  pivot_wider(names_from = inf, # Split whale distribution data over multiple columns.
              values_from = c(den_m,
                              den_s),
              values_fn = list(den_m = mean,
                               den_s = mean)) %>% # values_fn gets around an error from duplicate rows.
  drop_na(den_m_r, 
          den_m_i) %>% # Filter to areas with data in both information cases for comparison.
  rename(den_m_p = den_m_r,
         den_s_p = den_s_r) %>% 
  mutate(mor_m_i_ref = fun_mar(den_w = den_m_i / (area / 10^6), # whales / km^2
                               v_w = speed_w * kmh_ms, # m/s
                               v_v = spe_m * kt_ms, # m/s
                               l_w = length_w, # m
                               w_w = width_w, # m
                               l_v = len_v, # m
                               w_v = wid_v, # m
                               s = area, # m^2 / cell
                               t_v = t, # seconds of vessel transit
                               pro_s = pro_s, # Probability of whale depth and vessel draft coinciding.
                               pro_a = avoid, # Probability of successful avoidance by either/both the whale or/and the vessel.
                               v_v_kt = spe_m,
                               b_1_cs = b_1_cs,
                               b_2_cs = b_2_cs),
         mor_s_i_ref = fun_mar(den_w = den_s_i / (area / 10^6), # whales / km^2
                               v_w = speed_w * kmh_ms, # m/s
                               v_v = spe_m * kt_ms, # m/s
                               l_w = length_w, # m
                               w_w = width_w, # m
                               l_v = len_v, # m
                               w_v = wid_v, # m
                               s = area, # m^2 / cell
                               t_v = t, # seconds of vessel transit
                               pro_s = pro_s, # Probability of whale depth and vessel draft coinciding.
                               pro_a = avoid, # Probability of successful avoidance by either/both the whale or/and the vessel.
                               v_v_kt = spe_m,
                               b_1_cs = b_1_cs,
                               b_2_cs = b_2_cs),
         mor_m_p_ref = fun_mar(den_w = den_m_p / (area / 10^6), # whales / km^2
                               v_w = speed_w * kmh_ms, # m/s
                               v_v = spe_m * kt_ms, # m/s
                               l_w = length_w, # m
                               w_w = width_w, # m
                               l_v = len_v, # m
                               w_v = wid_v, # m
                               s = area, # m^2 / cell
                               t_v = t, # seconds of vessel transit
                               pro_s = pro_s, # Probability of whale depth and vessel draft coinciding.
                               pro_a = avoid, # Probability of successful avoidance by either/both the whale or/and the vessel.
                               v_v_kt = spe_m,
                               b_1_cs = b_1_cs,
                               b_2_cs = b_2_cs),
         mor_s_p_ref = fun_mar(den_w = den_s_p / (area / 10^6), # whales / km^2
                               v_w = speed_w * kmh_ms, # m/s
                               v_v = spe_m * kt_ms, # m/s
                               l_w = length_w, # m
                               w_w = width_w, # m
                               l_v = len_v, # m
                               w_v = wid_v, # m
                               s = area, # m^2 / cell
                               t_v = t, # seconds of vessel transit
                               pro_s = pro_s, # Probability of whale depth and vessel draft coinciding.
                               pro_a = avoid, # Probability of successful avoidance by either/both the whale or/and the vessel.
                               v_v_kt = spe_m,
                               b_1_cs = b_1_cs,
                               b_2_cs = b_2_cs))

write_csv(dat_in, "data_intermediate/dat_in.csv")

# Model mortalities and costs over full dataset to get "real" results before shifting into regression and optimization.

# Estimate outcomes the computationally-intensive way.

dat_out =
  expand_grid(int = seq(15, 5, by = -1), 
              ext = c(0.01, seq(1, 50, by = 5))) %>% # Choose parameters to test.
  mutate(out = 
           map2(.x = int, # Futures are temperamental. Check for snags before committing to large inputs.
                .y = ext, 
                .f = fun_sum,
                data = dat_in,
                quantile = quantile,
                .progress = TRUE)) %>% # Map higher-level functions over prepared data.
  unnest_wider(out) %>% # Wrangle data.
  mutate(m_i = qnorm(quantile, 
                     mor_m_i_cou,
                     mor_s_i_cou),
         m_p = qnorm(quantile,
                     mor_m_p_cou,
                     mor_s_p_cou)) %>% # Get higher-quantile values for estimated fatal strikes (instead of expected value).
  select(int,
         ext,
         m_i,
         m_p,
         c_i = cos_i,
         c_p = cos_p)

write_csv(dat_out, "data_intermediate/dat_out.csv")

# Tabulate results of full model with lags and leads to check whether mortality and cost are monotonic on intensity and extent.

tab_out = 
  dat_out %>% 
  arrange(int, ext) %>% 
  group_by(int) %>% 
  mutate(m_i_int_lead = lead(m_i),
         m_p_int_lead = lead(m_p),
         c_i_int_lead = lead(c_i),
         c_p_int_lead = lead(c_p),
         m_i_int_check = m_i_int_lead - m_i,
         m_p_int_check = m_p_int_lead - m_p,
         c_i_int_check = c_i - c_i_int_lead,
         c_p_int_check = c_p - c_p_int_lead) %>% 
  ungroup %>% 
  arrange(ext, int) %>% 
  group_by(ext) %>% 
  mutate(m_i_ext_lag = lag(m_i),
         m_p_ext_lag = lag(m_p),
         c_i_ext_lag = lag(c_i),
         c_p_ext_lag = lag(c_p),
         m_i_ext_check = m_i_ext_lag - m_i,
         m_p_ext_check = m_p_ext_lag - m_p,
         c_i_ext_check = c_i - c_i_ext_lag,
         c_p_ext_check = c_p - c_p_ext_lag)

# Visualize checks.

tab_out %>% 
  filter(ext > 0.01) %>% 
  select(int, ext, m_i_int_check, m_p_int_check, m_i_ext_check, m_p_ext_check) %>% 
  pivot_longer(cols = starts_with("m")) %>% 
  drop_na %>% # Just quick check to confirm that NAs follow a lazy pivot.
  filter(value < 0) %>% # And a quick check to see whether things are going in the wrong direction.
  ggplot() + 
  geom_raster(aes(x = int, 
                  y = ext, 
                  fill = value)) +
  scale_fill_viridis(option = "cividis") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ name)

tab_out %>% 
  filter(ext > 0.01) %>% 
  select(int, ext, c_i_int_check, c_p_int_check, c_i_ext_check, c_p_ext_check) %>% 
  pivot_longer(cols = starts_with("c")) %>% 
  drop_na %>% # Just quick check to confirm that NAs follow a lazy pivot.
  filter(value < 0) %>% # And a quick check to see whether things are going in the wrong direction.
  ggplot() + 
  geom_raster(aes(x = int, 
                  y = ext, 
                  fill = value)) +
  scale_fill_viridis(option = "cividis") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ name)

# Visualize results of full model for a quick check.
#  Mortality
dat_out %>% 
  filter(ext > 0.01) %>% 
  select(int, ext, starts_with("m")) %>% 
  pivot_longer(cols = starts_with("m")) %>% 
  ggplot() + 
  geom_raster(aes(x = int, 
                  y = ext, 
                  fill = value)) +
  scale_fill_viridis(option = "cividis") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ name)

#  Cost
dat_out %>% 
  filter(ext > 0.01) %>% 
  select(int, ext, starts_with("c")) %>% 
  pivot_longer(cols = starts_with("c")) %>% 
  ggplot() + 
  geom_raster(aes(x = int, 
                  y = ext, 
                  fill = value)) +
  scale_fill_viridis(option = "cividis") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ name)

# Get some useful values set before optimizing parameters.
#  Get costs of all kept transits under the status quo.
cos_0 = sum(dat_in$c_min * dat_in$t)

#  Get fatal strikes at P = 0.90 under the status quo.
mor_i_0 = qnorm(quantile, sum(dat_in$mor_m_i_ref), sum(dat_in$mor_s_i_ref ^ 2) ^ (1/2))
mor_p_0 = qnorm(quantile, sum(dat_in$mor_m_p_ref), sum(dat_in$mor_s_p_ref ^ 2) ^ (1/2))

# Choose target outcomes.

target = 
  bind_rows(tibble(target = seq(7.5, 12.0, by = 0.10)) %>% mutate(inf = "i"), 
            tibble(target = seq(5.0, 8.5, by = 0.10)) %>% mutate(inf = "p"))

# Fit linear model over target outcomes.
dat_opt <- 
  dat_out %>% # Get data from full biophysical + economic model.
  # filter(int %in% seq(5, 15)) %>% 
  nest(data = everything()) %>% 
  mutate(mod_m_i = map(.x = data, # Fit linear models to both variables and information cases.
                       .f = lm,
                       formula = m_i ~ int + ext + I(int * ext) + I(int^2) + I(ext^2)), #  + I(int^2 * ext^2)
         mod_c_i = map(.x = data,
                       .f = lm,
                       formula = c_i ~ int + ext + I(int * ext) + I(int^2) + I(ext^2)), #  + I(int^2 * ext^2)
         mod_m_p = map(.x = data,
                       .f = lm,
                       formula = m_p ~ int + ext + I(int * ext) + I(int^2) + I(ext^2)), #  + I(int^2 * ext^2)
         mod_c_p = map(.x = data,
                       .f = lm,
                       formula = c_p ~ int + ext + I(int * ext) + I(int^2) + I(ext^2))) %>%  #  + I(int^2 * ext^2)
  pivot_longer(cols = starts_with("mod"),
               names_to = c("var", "inf"),
               names_pattern = "mod_(.)_(.)",
               values_to = "mod") %>% 
  mutate(tidy = map(.x = mod, 
                    .f = tidy)) %>% # Get linear models into a more useful format.
  unnest(tidy) %>% 
  mutate(term = ifelse(term == "(Intercept)", "intercept", term)) %>% 
  select(var, inf, term, estimate) %>% 
  pivot_wider(names_from = term,
              values_from = estimate) %>% 
  rename(intext = 6,
         int2 = 7,
         ext2 = 8) %>% # Clean up variable names.
  pivot_wider(names_from = var,
              values_from = c(intercept, int, ext, intext, int2, ext2)) %>% # intext2
  right_join(target, ., by = "inf") %>% # Tag on targets.
  rowwise %>% 
  mutate(opt = fun_opt_out(inf = inf, # Optimize linear models over target outcomes.
                           target = target, 
                           intercept_m = intercept_m, 
                           int_m = int_m, 
                           ext_m = ext_m, 
                           int2_m = int2_m,
                           ext2_m = ext2_m,
                           intext_m = intext_m,
                           intercept_c = intercept_c, 
                           int_c = int_c, 
                           ext_c = ext_c,
                           int2_c = int2_c,
                           ext2_c = ext2_c,
                           intext_c = intext_c)) %>% 
  ungroup %>% 
  unnest_wider(opt, names_sep = "_") %>% 
  rename(int = opt_1,
         ext = opt_2) %>% 
  rowwise %>% 
  mutate(mor_check = fun_mor_reg(int = int, # Check mortality outcomes of linear models against underlying biophsical + economic model.
                                 ext = ext,
                                 intercept_m = intercept_m, 
                                 int_m = int_m, 
                                 ext_m = ext_m, 
                                 int2_m = int2_m,
                                 ext2_m = ext2_m,
                                 intext_m = intext_m),
         # intext2_m = intext2_m),
         cos_check = fun_cos_reg(int = int, # Check cost outcomes of linear models against underlying biophsical + economic model.
                                 ext = ext,
                                 intercept_c = intercept_c, 
                                 int_c = int_c, 
                                 ext_c = ext_c,
                                 int2_c = int2_c,
                                 ext2_c = ext2_c,
                                 intext_c = intext_c)) %>% # ,
  # intext2_c = intext2_c)) %>% 
  ungroup

# Set aside parameters in vectors for later.
dat_opt_pars = 
  dat_opt %>% 
  select(inf, ends_with("m"), ends_with("c")) %>%
  distinct

# Finish working over dat_opt.
dat_opt = 
  dat_opt %>%
  select(inf,
         target,
         int,
         ext,
         ends_with("check"))

# Visualize results of optimization for a quick check.

dat_opt %>% ggplot() + geom_point(aes(x = mor_check, y = cos_check, color = inf), alpha = 0.50)
dat_opt %>% ggplot() + geom_point(aes(x = int, y = ext, color = inf), alpha = 0.50) + facet_wrap(~inf)

dat_out %>% 
  filter(ext > 0.01) %>% 
  mutate(m_i_lm = 
           fun_mor_reg(int = int, 
                       ext = ext, 
                       intercept_m = dat_opt_pars$intercept_m[[1]],
                       int_m = dat_opt_pars$int_m[[1]],
                       int2_m = dat_opt_pars$int2_m[[1]],
                       ext_m = dat_opt_pars$ext_m[[1]],
                       ext2_m = dat_opt_pars$ext2_m[[1]],
                       intext_m = dat_opt_pars$intext_m[[1]]),
         m_p_lm = 
           fun_mor_reg(int = int, 
                       ext = ext, 
                       intercept_m = dat_opt_pars$intercept_m[[2]],
                       int_m = dat_opt_pars$int_m[[2]],
                       int2_m = dat_opt_pars$int2_m[[2]],
                       ext_m = dat_opt_pars$ext_m[[2]],
                       ext2_m = dat_opt_pars$ext2_m[[2]],
                       intext_m = dat_opt_pars$intext_m[[2]]),
         c_i_lm = 
           fun_cos_reg(int = int, 
                       ext = ext, 
                       intercept_c = dat_opt_pars$intercept_c[[1]],
                       int_c = dat_opt_pars$int_c[[1]],
                       int2_c = dat_opt_pars$int2_c[[1]],
                       ext_c = dat_opt_pars$ext_c[[1]],
                       ext2_c = dat_opt_pars$ext2_c[[1]],
                       intext_c = dat_opt_pars$intext_c[[1]]),
         c_p_lm = 
           fun_cos_reg(int = int, 
                       ext = ext, 
                       intercept_c = dat_opt_pars$intercept_c[[2]],
                       int_c = dat_opt_pars$int_c[[2]],
                       int2_c = dat_opt_pars$int2_c[[2]],
                       ext_c = dat_opt_pars$ext_c[[2]],
                       ext2_c = dat_opt_pars$ext2_c[[2]],
                       intext_c = dat_opt_pars$intext_c[[2]])) %>% 
  select(int, ext, m_i_lm, m_p_lm) %>% 
  pivot_longer(cols = c(m_i_lm, m_p_lm)) %>% 
  ggplot() +
  geom_raster(aes(x = int, 
                  y = ext, 
                  fill = value)) +
  scale_fill_viridis(option = "cividis") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ name)

dat_out %>% 
  filter(ext > 0.01) %>% 
  mutate(m_i_lm = 
           fun_mor_reg(int = int, 
                       ext = ext, 
                       intercept_m = dat_opt_pars$intercept_m[[1]],
                       int_m = dat_opt_pars$int_m[[1]],
                       int2_m = dat_opt_pars$int2_m[[1]],
                       ext_m = dat_opt_pars$ext_m[[1]],
                       ext2_m = dat_opt_pars$ext2_m[[1]],
                       intext_m = dat_opt_pars$intext_m[[1]]),
         m_p_lm = 
           fun_mor_reg(int = int, 
                       ext = ext, 
                       intercept_m = dat_opt_pars$intercept_m[[2]],
                       int_m = dat_opt_pars$int_m[[2]],
                       int2_m = dat_opt_pars$int2_m[[2]],
                       ext_m = dat_opt_pars$ext_m[[2]],
                       ext2_m = dat_opt_pars$ext2_m[[2]],
                       intext_m = dat_opt_pars$intext_m[[2]]),
         c_i_lm = 
           fun_cos_reg(int = int, 
                       ext = ext, 
                       intercept_c = dat_opt_pars$intercept_c[[1]],
                       int_c = dat_opt_pars$int_c[[1]],
                       int2_c = dat_opt_pars$int2_c[[1]],
                       ext_c = dat_opt_pars$ext_c[[1]],
                       ext2_c = dat_opt_pars$ext2_c[[1]],
                       intext_c = dat_opt_pars$intext_c[[1]]),
         c_p_lm = 
           fun_cos_reg(int = int, 
                       ext = ext, 
                       intercept_c = dat_opt_pars$intercept_c[[2]],
                       int_c = dat_opt_pars$int_c[[2]],
                       int2_c = dat_opt_pars$int2_c[[2]],
                       ext_c = dat_opt_pars$ext_c[[2]],
                       ext2_c = dat_opt_pars$ext2_c[[2]],
                       intext_c = dat_opt_pars$intext_c[[2]])) %>% 
  select(int, ext, c_i_lm, c_p_lm) %>% 
  pivot_longer(cols = c(c_i_lm, c_p_lm)) %>% 
  ggplot() +
  geom_raster(aes(x = int, 
                  y = ext, 
                  fill = value)) +
  scale_fill_viridis(option = "cividis") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ name)

# Use results of optimization to get mortalities and costs

# There's some funk with long/wide dataframes here -- dat_opt inputs are long, fun_sum/fun_sum_alt outputs are wide.

dat_out_opt_alt = 
  dat_opt %>% 
  # filter(target %in% seq(5.5, 5.7, by = 0.10)) %>%
  mutate(out_alt = 
           map2(.x = int, 
                .y = target, # Remember to check this against mor_check -- otherwise results could get weird without obvious indications. 
                .f = fun_sum_alt,
                data = dat_in,
                quantile = quantile,
                .progress = TRUE)) %>% 
  unnest_wider(out_alt) %>% 
  filter(inf == "p") %>% 
  select(target, mor_m_i_cou, mor_m_p_cou, mor_m_a_cou, ext_i, ext_p, cos_i, cos_p) %>%
  mutate(voi = cos_i - cos_p)

write_csv(dat_out_opt_alt, "data_intermediate/dat_out_opt_alt.csv")

dat_voi = dat_out_opt_alt

dat_voi %>% 
  select(target, ext_i, ext_p, cos_i, cos_p, voi) %>% 
  mutate(ext_i = ext_i * 100, 
         ext_p = ext_p * 100, 
         cos_i = cos_i / 1000000 * cpi, 
         cos_p = cos_p / 1000000 * cpi, 
         voi = voi / 1000000 * cpi, 
         ext_i = ext_i %>% round(2), 
         ext_p = ext_p %>% round(2), 
         cos_i = cos_i %>% round(2), 
         cos_p = cos_p %>% round(2), 
         voi = voi %>% round(2)) %>% 
  write_csv("out/dat_tab.csv")

dat_out_opt_alt %>% 
  ggplot() + 
  geom_point(aes(x = mor_m_i_cou, y = cos_i), color = "black") + 
  geom_point(aes(x = mor_m_p_cou, y = cos_p), color = "blue") + 
  geom_point(aes(x = mor_m_a_cou, y = cos_i), color = "red") +
  labs(x = "Whale Strikes", y = "Costs (US$2012)")

dat_out_opt_alt %>% 
  ggplot() + 
  geom_point(aes(x = target, y = voi))
