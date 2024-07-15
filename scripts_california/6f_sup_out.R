# Save intermediate and final data products to drive for reference.
#  Save geodata.
grid %>% select(id) %>% st_transform(crs = "+proj=longlat") %>% st_write("out/dat.shp")

#  Save post-processing, pre-analysis data.
data %>% write_csv("out/dat_initial.csv")

#  Save mid-analysis data. This is the initial output of the biophysical model without model policies.
dat_in %>% write_csv("out/dat_intermediate.csv")

#  Save mid-analysis data. This is the later output of the biophysical model with model policies.
dat_out %>% write_csv("out/dat_output.csv")

#  Save mid-analysis data. This is the output of the linear model minimizing cost subject to target rates of fatal strikes.
dat_opt %>% select(-message) %>% write_csv("out/dat_opt.csv")

#  Save post-analysis data. This is the output of the linear model summarized to set up value of information.
dat_voi %>% write_csv("out/dat_voi.csv")

#  Save supplemental statistical tests: Kolmogorov-Smirnov tests of distributions of error by month across information cases.
dat_ks_agg %>% write_csv("out/dat_ks.csv")

#  Save supplemental statistical output: polynomial regressions approximating biophysical and economic models for optimization.
dat_mod %>% 
  select(var, inf, mod, tidy) %>% 
  unnest(tidy) %>% 
  mutate(r2_adj = map(mod, ~ (summary(.)$adj.r.squared))) %>% 
  unnest(r2_adj) %>% 
  select(-mod) %>% 
  write_csv("out/dat_reg.csv")
