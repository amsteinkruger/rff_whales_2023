# Define a filter function to reduce daily AIS files on variables, latitude, and longitude, and then bind reports by month for further processing.
ais_fun_filter = function(ais_file, bound_lat_upper, bound_lon_upper, bound_lon_lower){
  
  ais_file %>% 
    read_csv %>% 
    select(vessel = MMSI,
           time = BaseDateTime,
           LAT,
           LON,
           speed = SOG,
           type = VesselType,
           length = Length,
           width = Width,
           draft = Draft) %>% 
    filter(LAT < bound_lat_upper) %>% 
    filter(LON < bound_lon_upper) %>% 
    filter(LON > bound_lon_lower) %>% 
    filter(speed > 1.00) %T>% 
    write_csv(paste0("data_intermediate/ais/", str_sub(ais_file, 10, -5), ".csv")) %T>% 
    gc() %>% 
    nrow
  
}

# Define an inner function to join AIS reports nested by vessel, day, month, and year to polygons specified by ecological data.
ais_fun_inner = function(ais_nest,
                         grid_which){
  
  ais_nest %>% 
    st_as_sf(coords = c("LON",
                        "LAT"), 
             crs = "+proj=longlat") %>% # Let R know about spatial coordinates.
    st_transform(crs = crs) %>% # Watch out for this external reference.
    st_join(.,
            waters, # Watch out for this external reference.
            join = st_within,
            left = FALSE) %>% # Intersect points with area of interest. Throw out points outside area of interest. 
    st_join(.,
            grid_which,
            join = st_within,
            left = FALSE) %>% # Intersect points with grid of cell polygons. Throw out points outside area of interest. 
    st_set_geometry(NULL)
  
}

# Define an outer function to read, process, and summarize AIS reports.
ais_fun_outer = function(ais_files,
                         ais_file,
                         grid_which){
  ais_files %>% 
    mutate(dat = map(.x = file, .f = read_csv)) %>% 
    unnest %>% 
    arrange(vessel, time) %>% 
    group_by(vessel) %>% 
    mutate(time = ifelse(as.numeric(as.duration(time - lag(time))) < 600,
                         as.numeric(as.duration(time - lag(time))),
                         0)) %>%
    nest %>% 
    mutate(out = data %>% map(ais_fun_inner, grid_which = grid_which)) %>% # Join AIS data to grid by vessel.
    select(-data) %>% # Drop input.
    unnest(out) %>% 
    group_by(id,
             vessel,
             type) %>% 
    summarize(speed_m = mean(speed, na.rm = TRUE), # Find mean speed over ground in knots for vessels.
              speed_s = sd(speed, na.rm = TRUE), # Find standard deviation of speed.
              length = mean(length, na.rm = TRUE),
              width = mean(width, na.rm = TRUE),
              draft = mean(draft, na.rm = TRUE),
              time = sum(time, na.rm = TRUE)) %>% # Find sum of vessel-seconds in each location and time.
    ungroup

}
