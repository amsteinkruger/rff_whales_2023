# Set parameters.
length_w = 20.9 # (m) From Gilpatrick and Perryman (2008) by way of Rockwood et al. (2017).
width_w = 2.96 # (m) From Goldbogen et al. (2011) by way of Rockwood et al. (2017).
speed_w = 2.64  # (km/h) From Bailey et al. (2009) by way of Rockwood et al. (2017) SM2. Add SD.
b_1_cs = -1.905 # Parameter from Conn and Silber (2013), used in Martin et al. (2016) formulation. 
b_2_cs = 0.217 # Parameter from Conn and Silber (2013), used in Martin et al. (2016) formulation.
depth = 2 # Parameter operationalizes proportion of draft enabling strike. So, strikes occur at (depth)*100% of draft.
avoid = 0 # Parameter operationalizes assumption of nonavoidance, as in third model from Rockwood et al. (2017).

kmh_ms = 0.277778 # Factor converting kilometers per hour to meters per second.
kt_ms = 0.514444 # Factor converting knots to meters per second.

criterion = 2.3 # Potential biological removal rate (PBR) in SAR 2018.

quantile = 0.90 # Quantile of interest for exceedance assessment.

crs = "+init=epsg:3309" # Set coordinate reference system to California Teale-Albers.

cpi = 1.42 # Factor for USD2012 > USD2023. From BLS CPI Inflation Calculator. https://data.bls.gov/cgi-bin/cpicalc.pl

# Get futures set up.
#  Let some workers rest.
# workers = availableWorkers() %>% length * 0.5

#  Make a plan.
# plan(multisession, 
#      workers = workers)
