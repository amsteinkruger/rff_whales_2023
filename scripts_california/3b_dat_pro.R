# Process data describing distributions of blue whales and positions, speeds of vessels.
# Read in RSDI (Hazen et al. 2017 / WhaleWatch).
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets directory to the current directory
# setwd('../../../..') # relative paths to move directory to the root project directory
# setwd('L:/Project-Valuables_WhaleWatch/v_ww_CA')

rsd = 
  lapply(list.files("data/rsd",
                    full.names=TRUE),
         read_csv) %>%
  bind_rows %>% 
  dplyr::select(la = latitude, # Tweaks for just 2017.
                lo = longitude,
                year,
                month,
                den_rsd_mid = density,
                den_rsd_std = sddens)

# Break out sums of densities by region and month to rescale with transparent computation.
#  This comes back around to rescale in situ data, too.
rsd_sum =
  rsd %>%
  filter(year == 2017) %>% # 2017's a nice year. (Regional sums are almost identical for 2016-2019.)
  group_by(month) %>%
  summarize(den_sum = sum(den_rsd_mid, na.rm = TRUE)) %>%
  ungroup

# Scale.
rsd =
  rsd %>%
  left_join(rsd_sum,
            by = "month") %>% # Tag on sums by region and month.
  group_by(year,
           month) %>%
  mutate(den_rsd_mid_normd = den_rsd_mid / sum(den_rsd_mid, na.rm = TRUE) * den_sum, # Scale means of densities.
         den_rsd_std_normd = den_rsd_std * den_rsd_mid_normd / den_rsd_mid) %>% # Scale standard deviations of densities (?).
  ungroup %>%
  select(la,
         lo,
         year,
         month,
         den_rsd_mid = den_rsd_mid_normd,
         den_rsd_std = den_rsd_std_normd)

# Set extent.
#  Box.
lonbox = c(min(rsd$lo) + 0.125,
           max(rsd$lo) + 0.125,
           max(rsd$lo) + 0.125,
           min(rsd$lo) + 0.125,
           min(rsd$lo) + 0.125)
latbox = c(max(rsd$la) + 0.125,
           max(rsd$la) + 0.125,
           min(rsd$la) + 0.125,
           min(rsd$la) + 0.125,
           max(rsd$la) + 0.125)

#  Transform area to CRS.
box =
  cbind(lonbox, latbox) %>%
  list %>%
  st_polygon %>%
  st_sfc(crs = "+proj=longlat")

# Set CA boundary at 42nd parallel
lonca = c(-129.40,
          -122.00,
          -122.00,
          -129.40,
          -129.40)
latca = c(49.20,
          49.20,
          41.99,
          41.99,
          49.20)

#  Transform area to CRS.
ca_parallel =
  cbind(lonca, latca) %>%
  list %>%
  st_polygon %>%
  st_sfc(crs = 4326) %>% 
  st_transform(crs = st_crs(crs))

# Yoink coastal states to trim inland waters off the exclusive economic zone.
sta =
  st_read("data/sta/CoastalStates.gdb") %>%
  filter(StateUSPS == "CA" | StateUSPS == "OR") %>%
  select %>%
  st_union %>%
  st_transform(crs = crs)

ca_waters =
  st_read("data/eez/FederalAndStateWaters.gdb") %>%
  filter(Jurisdicti == "California" | Jurisdicti == "Federal") %>%
  select %>%
  st_transform(crs = crs) %>%
  st_intersection(box %>%
                    st_make_grid(cellsize = c(0.25,
                                              0.25),
                                 what = "polygons") %>% # Grid RSD.
                    st_union %>% # Combine areas for easier computing.
                    st_sf %>%
                    st_transform(crs = crs) %>%
                    st_union) %>% # Drop polygons outside the area of interest.
  st_difference(sta) %>% # Drop inland jurisdictions.
  st_difference(ca_parallel) %>% # Drop everything above CA parallel.
  st_union %>% # Combine jurisdictions off the contiguous Pacific Coast States.
  st_simplify(dTolerance = 1000) %>% # Clean edges for easier computing. dTolerance is in meters.
  st_sf

# Wrangle WhaleWatch onto polygon grid, then bound by CA waters
rsd =
  rsd %>%
  st_as_sf(coords = c("lo", "la"),
           crs = "+proj=longlat") %>%
  st_transform(crs = crs) %>%
  st_join(.,
          box %>%
            st_make_grid(cellsize = c(0.25,
                                      0.25),
                         what = "polygons") %>% # Grid RSD.
            st_sf %>%
            mutate(id = seq(1, nrow(.)),
                   inf = "r") %>% # Get a unique numeric identifier for each polygon.
            st_transform(crs = crs) %>% 
            st_intersection(ca_waters %>% st_buffer((25000^2 + 25000^2)^(1 / 2))) %>% # Intersect w/ EEZ + buffer to keep RSD points within polygons.
            mutate(area_old = st_area(.) %>% as.numeric), # Preserve area w/ buffer on EEZ for transformation w/ unbuffered area below.
          join = st_within,
          left = FALSE) %>%
  st_set_geometry(NULL) %>% # Get rid of point geometries for replacement w/ polygons.
  rename(den_mid = den_rsd_mid,
         den_std = den_rsd_std) %>% 
  ungroup %>% 
  full_join(box %>%
              st_make_grid(cellsize = c(0.25,
                                        0.25),
                           what = "polygons") %>% # Grid RSD.
              st_sf %>%
              mutate(id = seq(1, nrow(.))) %>% # Get a unique numeric identifier for each polygon.
              st_transform(crs = crs) %>% 
              st_intersection(ca_waters),
            by = "id") %>% # Get polygon geometries.
  st_sf %>% 
  st_intersection(ca_waters) %>% # Trim buffer off. The buffer kept st_within from losing points outside EEZ describing areas within EEZ.
  mutate(area_new = st_area(.) %>% as.numeric,
         den_mid = den_mid * (area_new / area_old) * (10^9 / area_new),
         den_std = den_std * (area_new / area_old) * (10^9 / area_new)) %>% # Unitize blue whale densities by square kilometers.
  select(-starts_with("area")) %>%
  st_set_geometry(NULL)

# Define grids for interpolation by data extent and EEZ.
#  Wrangle and bound RSD by EEZ.
grid_rsd = 
  box %>%
  st_make_grid(cellsize = c(0.25,
                            0.25),
               what = "polygons") %>% # Grid RSD.
  st_sf %>%
  st_transform(crs = crs) %>% 
  mutate(id = seq(1, nrow(.))) %>% # Get a unique numeric identifier for each polygon.
  st_intersection(ca_waters)

#  Same, but with a buffer to get the right ISD polygons for areal weighting. (Vignette?)
grid_rsd_buf = 
  box %>%
  st_make_grid(cellsize = c(0.25,
                            0.25),
               what = "polygons") %>% # Grid RSD.
  st_sf %>%
  st_transform(crs = crs) %>% 
  mutate(id = seq(1, nrow(.))) %>% # Get a unique numeric identifier for each polygon.
  st_intersection(ca_waters %>% st_buffer(50000))

#  Wrangle and bound ISD by EEZ.
grid_isd =
  st_read("data/isd/serdp_swfsc_cce.shp") %>% # Read in-situ data (Becker et al. 2012).
  st_transform(crs = crs) %>% 
  select %>% 
  mutate(id = seq(1, nrow(.)),
         area_old = st_area(.) %>% as.numeric) %>% 
  st_intersection(ca_waters) %>%
  st_simplify(dTolerance = 1000) %>% 
  mutate(area_new = st_area(.) %>% as.numeric)

# Build out a list of months and years to format ISD for join w/ regional abundances and normalization w/ RSD.
isd =
  data.frame(year = rep(min(rsd$year, na.rm = TRUE):max(rsd$year, na.rm = TRUE), each = 12),
             month = rep(1:12, length(min(rsd$year, na.rm = TRUE):max(rsd$year, na.rm = TRUE)))) %>% # Build list of m*y.
  group_by(year,
           month) %>%
  nest %>%
  mutate(data = # Fill list w/ ISD, with the full dataset appearing for each m*y.
           st_read("data/isd/serdp_swfsc_cce.shp") %>% # Read in-situ data (Becker et al. 2012).
           select(den_isd_mid = Bmu_i_u_d,
                  den_isd_std = Bmu_i_u_e) %>% 
           st_transform(crs = crs) %>% 
           mutate(id = seq(1, nrow(.))) %>% 
           st_set_geometry(NULL) %>%
           right_join(.,
                      grid_isd,
                      by = "id") %>% # Use a non-spatial join to avoid shenanigans with polygons.
           select(-geometry,
                  -starts_with("area)")) %>%
           list) %>% # Squish data into convenient format for join.
  unnest(data) %>% # Explode data.
  ungroup %>%
  select(year,
         month,
         id,
         den_isd_mid,
         den_isd_std) %>% # Keep useful columns.
  full_join(grid_isd) %>% # Get geometries back.
  mutate(den_isd_mid = den_isd_mid * (area_new / area_old) * (10^9 / area_new),
         den_isd_std = den_isd_std * (area_new / area_old) * (10^9 / area_new)) %>% 
  data.frame %>% 
  st_sf %>% 
  select(year, 
         month, 
         den_isd_mid,
         den_isd_std) %>% # Keep useful columns. id would be nice, but it precludes interpolation.
  group_by(year,
           month) %>% # Exclude columns from interpolation.
  nest %>% # Squish data.
  mutate(data = 
           data %>% 
           map(.f = st_interpolate_aw,
               to = grid_rsd_buf, # Intersect w/ EEZ + buffer to keep RSD points within polygons.
               extensive = FALSE) %>% # Interpolate.
           map(.f = st_intersection,
               y = ca_waters) %>% 
           map(.f = st_join,
               y = grid_rsd,
               join = st_equals) %>% 
           map(.f = select,
               id,
               den_isd_mid,
               den_isd_std)) %>% # Get rid of interpolation output.
  unnest(data) %>% 
  ungroup %>% 
  select(-geometry) %>% # Get rid of geometries.
  rename(den_mid = den_isd_mid,
         den_std = den_isd_std) %>% # Get names set for bind_rows.
  mutate(inf = "i") # Annotate data for bind_rows.

# Clarify that grid_rsd is the only grid of interest through the rest of the analysis.
grid = grid_rsd %>% mutate(area = st_area(.) %>% as.numeric)

# Export for future reference.
grid %>% select(id) %>% write_sf("data_intermediate/grid.shp")

# Combine data and find a posterior distribution for locations and times with available data.
data = 
  bind_rows(isd, rsd) %>% # Squish data together.
  filter(year == 2017) %>% # Get rid of periods outside period of interest.
  pivot_wider(names_from = inf,
              values_from = starts_with("den")) %>% # Widen data for normalization and posterior calculation.
  group_by(year,
           month) %>% # Group data for normalization.
  mutate(den_mid_i_nor = den_mid_i / sum(den_mid_i, na.rm = TRUE) * sum(den_mid_r, na.rm = TRUE),
         den_std_i_nor = den_std_i * (den_mid_i_nor / den_mid_i)) %>% # Normalize data
  ungroup %>% 
  select(year,
         month,
         id,
         den_mid_i = den_mid_i_nor,
         den_std_i = den_std_i_nor,
         den_mid_r,
         den_std_r) %>% # Clean out unnormalized ISD.
  mutate(den_mid_p = ifelse(is.na(den_mid_i),
                            NA,
                            ifelse(is.na(den_mid_r),
                                   den_mid_i,
                                   ((den_std_i ^ 2) ^ -1 + (den_std_r ^ 2) ^ -1) ^ -1 * (den_mid_i / den_std_i ^ 2 + den_mid_r / den_std_r ^ 2))),
         den_std_p = ifelse(is.na(den_std_i),
                            NA,
                            ifelse(is.na(den_std_r),
                                   den_std_i,
                                   (((den_std_i ^ 2) ^ -1 + (den_std_r ^ 2) ^ -1) ^ -1 + (den_std_r ^ 2)) ^ (1 / 2)))) %>% # Get posterior predictive.
  group_by(year,
           month) %>% # Group data for another round of normalization.
  mutate(den_mid_p_nor = den_mid_p / sum(den_mid_p, na.rm = TRUE) * sum(den_mid_r, na.rm = TRUE),
         den_std_p_nor = den_std_p * (den_mid_p_nor / den_mid_p)) %>% # Normalize posterior.
  ungroup %>%
  select(year,
         month,
         id,
         den_mid_i,
         den_std_i,
         den_mid_r,
         den_std_r,
         den_mid_p = den_mid_p_nor,
         den_std_p = den_std_p_nor) %>% # Clean out unnormalized ISD.
  pivot_longer(cols = starts_with("den"),
               names_to = c("sta",
                            "inf"),
               names_pattern = "den_(.*)_(.*)",
               values_to = "den") %>% 
  pivot_wider(names_from = sta,
              values_from = den) %>% 
  rename(den_mid = mid,
         den_std = std) %>% 
  left_join(grid,
            by = "id") %>% 
  mutate(den_mid = den_mid * (area / 10^9), 
         den_std = den_std * (area / 10^9)) %>% 
  select(-geometry,
         -area)

# Clean out the environment.
rm(box,
   latbox,
   lonbox,
   ca_parallel,
   sta,
   rsd_sum,
   grid_isd,
   grid_rsd,
   grid_rsd_buf,
   isd,
   rsd)

# Read, wrangle, and transform Automatic Identification System data.

#  First, read, filter, and select from daily AIS reports.
#   Estimated at 2h for the full 2017 dataset, 2023/11/3. Interruptions might be caused by vroom.

ais_filter = 
  list.files("data/ais", full.names = TRUE) %>%
  # magrittr::extract(1:365) %>% # To resume processing after an interruption, clean up files and change this index.
  map(ais_fun_filter,
      bound_lat_upper = 49,
      bound_lon_upper = -115,
      bound_lon_lower = -140)

#  Second, read filtered daily AIS reports, join onto the grid, and turn points into duration.

#   Estimated at 18h for the full 2017 dataset, 2023/11/6.

ais =
  list.files("data_intermediate/ais", full.names = TRUE) %>%
  tibble(file = .) %>% 
  # filter(str_sub(file, -9, -8) %in% c("09", "10", "11", "12")) %>% # To subset files for processing, change this index to other _MM_DD values.
  mutate(year = file %>% str_sub(-14, -11),
         month = file %>% str_sub(-9, -8)) %>% 
  group_by(year, month) %>% 
  nest(.key = "dat") %>% 
  mutate(dat = 
           map(.x = dat,
               .f = ~ ais_fun_outer(ais_files = .x, ais_file = file, grid_which = grid))) %>% 
  unnest %>% 
  ungroup %>% 
  mutate(year = year %>% as.numeric,
         month = month %>% as.numeric)

gc()

# Export for future reference.
write_csv(ais, "data_intermediate/ais.csv")

# Import for present reference.
# ais = read_csv("data_intermediate/ais.csv")

# Try a Band-Aid for nondistinct (but non-duplicate?) AIS observations, post-processing.
ais = 
  ais %>% 
  group_by(id, month, year, vessel, type) %>% 
  summarize(speed_m = weighted.mean(x = speed_m, w = time, na.rm = TRUE),
            speed_s = weighted.mean(x = speed_s, w = time, na.rm = TRUE), # This must be bad statistics but speed_s isn't used for anything.
            length = mean(length),
            width = mean(width),
            draft = mean(draft),
            time = sum(time)) %>% 
  ungroup

# Read codes and costs for vessels by type. The costs file includes a crosswalk.
ais_codes = 
  "data/cos/ais_codes_cadastre_20180523.csv" %>% 
  read_delim(delim = ";") %>% 
  select(group_coarse = 1,
         group_fine = 5,
         type = 3)

ais_costs = 
  "data/cos/costs_nathan_2012.csv" %>% 
  read_delim(delim = ";") %>% 
  select(group = 1,
         cost_min = 2,
         cost_max = 3)

# Join codes and costs to AIS data.
ais_join =
  ais %>%
  mutate(type = ifelse(is.na(type), 0, type)) %>% 
  left_join(ais_codes,
            by = "type") %>%
  left_join(ais_costs,
            by = c("group_coarse" = "group")) %>%
  mutate(cost_min = cost_min / 3600,
         cost_max = cost_max / 3600) # Convert hour costs to second costs.

# Join data.
data = 
  data %>% 
  filter(inf != "p") %>%
  left_join(ais_join,
            by = c("id",
                   "year",
                   "month"),
            relationship = "many-to-many") %>%
  left_join(grid %>% st_set_geometry(NULL),
            by = "id") %>% 
  select(inf = inf, 
         year = year,
         month = month,
         id = id,
         area = area,
         den_m = den_mid, 
         den_s = den_std, 
         group_coarse,
         group_fine,
         spe_m = speed_m, 
         len_v = length,
         wid_v = width,
         dra_v = draft,
         t = time,
         c_min = cost_min,
         c_max = cost_max) %>% 
  group_by(year,
           month,
           id,
           group_fine) %>% # Group data to impute at highest available resolution.
  mutate(spe_m = ifelse(is.na(spe_m),
                        mean(spe_m, na.rm = TRUE),
                        spe_m),
         len_v = ifelse(is.na(len_v),
                        mean(len_v, na.rm = TRUE),
                        len_v),
         wid_v = ifelse(is.na(wid_v),
                        mean(wid_v, na.rm = TRUE),
                        wid_v),
         dra_v = ifelse(is.na(dra_v),
                        mean(dra_v, na.rm = TRUE),
                        dra_v)) %>% # Impute limiting parameters.
  group_by(group_coarse) %>% # Group data to impute at second-to-lowest resolution.
  mutate(len_v = ifelse(is.na(len_v),
                        mean(len_v, na.rm = TRUE),
                        len_v),
         wid_v = ifelse(is.na(wid_v),
                        mean(wid_v, na.rm = TRUE),
                        wid_v),
         dra_v = ifelse(is.na(dra_v),
                        mean(dra_v, na.rm = TRUE),
                        dra_v),
         c_min = ifelse(is.na(c_min),
                        mean(c_min, na.rm = TRUE),
                        c_min)) %>% # Impute limiting parameters.
  ungroup %>% 
  drop_na(spe_m,
          len_v,
          wid_v,
          dra_v,
          c_min,
          c_max,
          t) %>% 
  mutate(dra_v = ifelse(dra_v <= 0, 1, dra_v), # Wrangle drafts to probabilities of vessel draft and whale depth coinciding.
         depth = ifelse(ceiling(dra_v) * depth > 31, 31, ceiling(dra_v) * depth)) %>% # 31m is the upper end of blue whales' time-at-depth.
  left_join(read_csv("data/div/depth.csv"), by = "depth") %>% # (m) From Rockwood et al. (2017).
  rename(pro_s = probability) %>% 
  select(-depth)

write.csv(data, "data_intermediate/data.csv")

# Tidy up the environment.
rm(eez,
   ca_waters,
   ais,
   ais_join,
   ais_codes,
   ais_costs,
   depth)
