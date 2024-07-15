#  Follow Martin et al. (2016) in estimating encounters, strikes, and fatal strikes.
#   Function estimating encounters.
fun_encounter = function(v_w, # Speed of whale in meters per second.
                         v_v, # Speed of vessel in meters per second.
                         l_w, # Length of vessel in meters.
                         w_w, # Width of vessel in meters.
                         l_v, # Length of vessel in meters.
                         w_v, # Width of vessel in meters.
                         s,   # Space in square meters.
                         t_v, # Time in vessel-seconds.
                         d_w) # Density of whales in animals per cell.
  
{
  
  r = (l_w ^ 2 + w_w ^ 2) ^ (1 / 2) + (l_v ^ 2 + w_v ^ 2) ^ (1 / 2) # Radius for encounters.
  
  fun_j = Vectorize(function(v_w, v_v)
    
  {
    
    a = (2 * v_w * v_v) / (v_w ^ 2 + v_v ^ 2)
    
    fun_theta = function(theta, a) ((1 - a * cos(theta)) / (2 * pi)) ^ (1 / 2)
    
    integrate(fun_theta, 
              lower = 0,
              upper = 2 * pi,
              a = a)$value
    
  })
  
  (2 * r / s) * (v_w ^ 2 + v_v ^ 2) ^ (1 / 2) * fun_j(v_w, v_v) * t_v * d_w # Encounters per location per timestep.
  
}

#  Function estimating strikes.
fun_strike = function(pro_s, # Probability of whale depth and vessel draft coinciding.
                      pro_a) # Probability of successful avoidance by either/both the whale or/and the vessel.
  
{pro_s * (1 - pro_a)} # Probability of a strike conditional on encounter.

#  Function estimating mortality.
fun_mortality = function(encounter, # Encounters.
                         strike,    # Probability of strike conditional on encounter.
                         v_v_kt,    # Speed of vessel in knots.
                         b_1_cs,    # Parameter from Conn and Silber (2013). (Given by Martin et al. (2016) as -1.905 for this formulation.)
                         b_2_cs)    # Parameter from Conn and Silber (2013). (Given by Martin et al. (2016) as 0.217 for this formulation.)
  
{(1 / (1 + exp(-b_1_cs - b_2_cs * v_v_kt))) * strike * encounter} # Mortality.

#  Wrapper for encounters, strikes, and mortality.
fun_mar = function(den_w, # Density of whales in animals per cell. Mind names.
                   v_w, # Speed of whale in meters per second.
                   v_v, # Speed of vessel in meters per second.
                   l_w, # Length of whale in meters.
                   w_w, # Width of whale in meters.
                   l_v, # Length of vessel in meters.
                   w_v, # Width of vessel in meters.
                   s,   # Space in square meters.
                   t_v, # Time in vessel-seconds.
                   pro_s, # Probability of whale depth and vessel draft coinciding.
                   pro_a, # Probability of successful avoidance by either/both the whale or/and the vessel.
                   v_v_kt, # Speed of vessel in knots.
                   b_1_cs, # Parameter from Conn and Silber (2013). (Given by Martin et al. (2016) as -1.905 for this formulation.
                   b_2_cs) # Parameter from Conn and Silber (2013). (Given by Martin et al. (2016) as 0.217 for this formulation.
{
  # Arguments are explicit for idiot-proofing.
  
  encounters = fun_encounter(d_w = den_w, # Density of whales in animals per cell. Mind names.
                             v_w = v_w, # Speed of whale in meters per second.
                             v_v = v_v, # Speed of vessel in meters per second.
                             l_w = l_w, # Length of vessel in meters.
                             w_w = w_w, # Width of vessel in meters.
                             l_v = l_v, # Length of vessel in meters.
                             w_v = w_v, # Width of vessel in meters.
                             s = s,   # Space in square meters.
                             t_v = t_v) # Time in vessel-seconds.) # Density of whales in animals per cell. Mind names.
  
  encounters = ifelse(encounters < 0, # Insert a floor to avoid negative encounters.
                      0,
                      encounters)
  
  strikes = fun_strike(pro_s, # Probability of whale depth and vessel draft coinciding.
                       pro_a) # Probability of successful avoidance by either/both the whale or/and the vessel.
  
  mortality = fun_mortality(encounter = encounters, # Encounters.
                            strike = strikes,    # Probability of strike conditional on encounter.
                            v_v_kt = v_v_kt,     # Speed of vessel in knots.
                            b_1_cs = b_1_cs,     # Parameter.
                            b_2_cs = b_2_cs)     # Parameter.
  
  return(mortality)
  
}

#  Estimate fatal strikes under a counterfactual policy with explicit intensity (vessel speed limit) and extensity (when and where).
fun_mor = function(data,
                   quantile,
                   intensity, 
                   extensity)
                   # target)
                   # cost)
  
{
  data %>%
    mutate(spe_cou = ifelse(spe_m > intensity, intensity, spe_m),
           mor_m_i_cou = fun_mar(den_w = den_m_i / (area / 10^6), # whales / km^2
                                 v_w = speed_w * kmh_ms, # m/s
                                 v_v = spe_cou * kt_ms, # m/s
                                 l_w = length_w, # m
                                 w_w = width_w, # m
                                 l_v = len_v, # m
                                 w_v = wid_v, # m
                                 s = area, # m^2 / cell
                                 t_v = t * spe_m / spe_cou, # seconds of vessel transit
                                 pro_s = pro_s, # Probability of whale depth and vessel draft coinciding.
                                 pro_a = avoid, # Probability of successful avoidance by either/both the whale or/and the vessel.
                                 v_v_kt = spe_cou,
                                 b_1_cs = b_1_cs,
                                 b_2_cs = b_2_cs),
           mor_s_i_cou = fun_mar(den_w = den_s_i / (area / 10^6), # whales / km^2
                                 v_w = speed_w * kmh_ms, # m/s
                                 v_v = spe_cou * kt_ms, # m/s
                                 l_w = length_w, # m
                                 w_w = width_w, # m
                                 l_v = len_v, # m
                                 w_v = wid_v, # m
                                 s = area, # m^2 / cell
                                 t_v = t * spe_m / spe_cou, # seconds of vessel transit
                                 pro_s = pro_s, # Probability of whale depth and vessel draft coinciding.
                                 pro_a = avoid, # Probability of successful avoidance by either/both the whale or/and the vessel.
                                 v_v_kt = spe_cou,
                                 b_1_cs = b_1_cs,
                                 b_2_cs = b_2_cs),
           mor_m_p_cou = fun_mar(den_w = den_m_p / (area / 10^6), # whales / km^2 (?)
                                 v_w = speed_w * kmh_ms, # m/s
                                 v_v = spe_cou * kt_ms, # m/s
                                 l_w = length_w, # m
                                 w_w = width_w, # m
                                 l_v = len_v, # m
                                 w_v = wid_v, # m
                                 s = area, # m^2 / cell
                                 t_v = t * spe_m / spe_cou, # seconds of vessel transit
                                 pro_s = pro_s, # Probability of whale depth and vessel draft coinciding.
                                 pro_a = avoid, # Probability of successful avoidance by either/both the whale or/and the vessel.
                                 v_v_kt = spe_cou,
                                 b_1_cs = b_1_cs,
                                 b_2_cs = b_2_cs),
           mor_s_p_cou = fun_mar(den_w = den_s_p / (area / 10^6), # whales / km^2 (?)
                                 v_w = speed_w * kmh_ms, # m/s
                                 v_v = spe_cou * kt_ms, # m/s
                                 l_w = length_w, # m
                                 w_w = width_w, # m
                                 l_v = len_v, # m
                                 w_v = wid_v, # m
                                 s = area, # m^2 / cell
                                 t_v = t * spe_m / spe_cou, # seconds of vessel transit
                                 pro_s = pro_s, # Probability of whale depth and vessel draft coinciding.
                                 pro_a = avoid, # Probability of successful avoidance by either/both the whale or/and the vessel.
                                 v_v_kt = spe_cou,
                                 b_1_cs = b_1_cs,
                                 b_2_cs = b_2_cs),
           cos_min_ref = c_min * t,
           cos_min_cou = ifelse(spe_m > spe_cou, cos_min_ref * (spe_m / intensity), cos_min_ref),
           cos_min_dif = cos_min_cou - cos_min_ref) %>% # Sneak in a convenient cost calculation.
    group_by(year,
             month,
             id) %>% 
    summarize(mor_m_i_ref = sum(mor_m_i_ref, na.rm = TRUE),
              mor_s_i_ref = sum(mor_s_i_ref ^ 2, na.rm = TRUE) ^ (1 / 2), 
              mor_m_p_ref = sum(mor_m_p_ref, na.rm = TRUE),
              mor_s_p_ref = sum(mor_s_p_ref ^ 2, na.rm = TRUE) ^ (1 / 2),
              mor_m_i_cou = sum(mor_m_i_cou, na.rm = TRUE),
              mor_s_i_cou = sum(mor_s_i_cou ^ 2, na.rm = TRUE) ^ (1 / 2),
              mor_m_p_cou = sum(mor_m_p_cou, na.rm = TRUE),
              mor_s_p_cou = sum(mor_s_p_cou ^ 2, na.rm = TRUE) ^ (1 / 2),
              cos_min_dif = sum(cos_min_dif, na.rm = TRUE)) %>%
    ungroup %>%
    # Check for oddball negative values.
    mutate(mor_m_i_cou = ifelse(mor_m_i_cou > mor_m_i_ref, mor_m_i_ref, mor_m_i_cou),
           mor_m_p_cou = ifelse(mor_m_p_cou > mor_m_p_ref, mor_m_p_ref, mor_m_p_cou)) %>% 
    # Pick places and times.
    mutate(mar_q_i_90_ref = qnorm(quantile, mor_m_i_ref, mor_s_i_ref),
           mar_q_i_90_cou = qnorm(quantile, mor_m_i_cou, mor_s_i_cou),
           mar_q_i_90_dif = mar_q_i_90_ref - mar_q_i_90_cou,
           mar_i = mar_q_i_90_dif / (cos_min_dif + 1), 
           mar_q_p_90_ref = qnorm(quantile, mor_m_p_ref, mor_s_p_ref),
           mar_q_p_90_cou = qnorm(quantile, mor_m_p_cou, mor_s_p_cou),
           mar_q_p_90_dif = mar_q_p_90_ref - mar_q_p_90_cou,
           mar_p = mar_q_p_90_dif / (cos_min_dif + 1)) %>% 
    arrange(desc(mar_i)) %>% 
    mutate(ext = row_number() / n(), # This is sensitive to order, so it needs to be overwritten with reordering.
           bin_i = ifelse(ext < (extensity / 100), 1, 0)) %>% # This line works for inputs intensity and extensity.
           # mor_q_i_dif_sum = cumsum(mar_q_i_90_dif), # This line works for inputs intensity and target.
           # bin_i = ifelse(mor_q_i_dif_sum < target, 1, 0)) %>% # This line works for inputs intensity and target.
    arrange(desc(mar_p)) %>% 
    mutate(ext = row_number() / n(), # This is sensitive to order, so it needs to be overwritten with reordering.
           bin_p = ifelse(ext < (extensity / 100), 1, 0)) %>% # This line works for inputs intensity and extensity.
           # mor_q_p_dif_sum = cumsum(mar_q_p_90_dif), # This line works for inputs intensity and target.
           # bin_p = ifelse(mor_q_p_dif_sum < target, 1, 0)) %>% # This line works for inputs intensity and target.
    arrange(year, month, id) %>% 
    mutate(mor_m_i_out = mor_m_i_ref * abs(bin_i - 1) + mor_m_i_cou * bin_i,
           mor_m_p_out = mor_m_p_ref * abs(bin_p - 1) + mor_m_p_cou * bin_p,
           mor_s_i_out = mor_s_i_ref * abs(bin_i - 1) + mor_s_i_cou * bin_i,
           mor_s_p_out = mor_s_p_ref * abs(bin_p - 1) + mor_s_p_cou * bin_p,
           cos_i = cos_min_dif * bin_i,
           cos_p = cos_min_dif * bin_p) %>% 
    return
  
}

#  Find distributions of outcomes under the status quo and counterfactual policies.
fun_agg = function(data)
  
{
  data %>% 
    summarize(mor_m_i_ref = sum(mor_m_i_ref, na.rm = TRUE),
              mor_s_i_ref = sum(mor_s_i_ref ^ 2, na.rm = TRUE) ^ (1/2),
              mor_m_p_ref = sum(mor_m_p_ref, na.rm = TRUE),
              mor_s_p_ref = sum(mor_s_p_ref ^ 2, na.rm = TRUE) ^ (1/2),
              mor_m_i_cou = sum(mor_m_i_out, na.rm = TRUE),
              mor_s_i_cou = sum(mor_s_i_out ^ 2, na.rm = TRUE) ^ (1/2),
              mor_m_p_cou = sum(mor_m_p_out, na.rm = TRUE),
              mor_s_p_cou = sum(mor_s_p_out ^ 2, na.rm = TRUE) ^ (1/2),
              ext_i = sum(bin_i) / n(),
              ext_p = sum(bin_p) / n(),
              cos_i = sum(cos_i, na.rm = TRUE),
              cos_p = sum(cos_p, na.rm = TRUE))
  
}

#  Combine functions for convenience.
fun_sum = function(data, quantile, intensity, extensity) # target
  
{
  
  fun_mor(data = data, quantile = quantile, intensity = intensity, extensity = extensity) %>% fun_agg(data = .) %>% return # target = target
  
}

# Describe functions to optimize over estimated outcomes.
#  Describe outer function.

# Alternative specification of the outer function using nloptr.
#  See https://nlopt.readthedocs.io/en/latest/NLopt_Algorithms/ for algorithm references in detail.
#  The algorithm specified at writing (2023/5/8) is a re-implementation of Subplex, a variant of Nelder-Mead, which might be familiar from Excel's solver.
#  i.e. this algorithm has a lower overhead than some alternatives and accomodates my layman's intuition in terms of inputs and outputs.
fun_opt_out = function(inf, 
                       target, 
                       intercept_m, 
                       int_m, 
                       int2_m,
                       ext_m, 
                       ext2_m,
                       intext_m, 
                       intercept_c, 
                       int_c, 
                       int2_c,
                       ext_c, 
                       ext2_c,
                       intext_c) 

{

  nloptr(x0 = c(5.00, 0.00), # Try positions close to the lowest possible speed limit and lowest possible extent.
         eval_f = fun_opt_inn,
         opts = list("algorithm" = "NLOPT_LN_SBPLX",
                     "maxeval" = 1000,
                     "xtol_rel" = 1.0e-3),
         lb = c(5.00, 0.00),
         ub = c(5.00, 50),
         inf = inf,
         target = target,
         intercept_m = intercept_m,
         int_m = int_m, 
         int2_m = int2_m,
         ext_m = ext_m, 
         ext2_m = ext2_m,
         intext_m = intext_m, 
         intercept_c = intercept_c, 
         int_c = int_c, 
         int2_c = int2_c,
         ext_c = ext_c, 
         ext2_c = ext2_c,
         intext_c = intext_c) %>% 
    magrittr::extract("solution")
  
}

#  Describe inner function. This must match regression specifications in the next script (4b).
fun_opt_inn = function(par, 
                       inf, 
                       target, 
                       intercept_m, 
                       int_m, 
                       int2_m,
                       ext_m, 
                       ext2_m,
                       intext_m, 
                       intercept_c, 
                       int_c, 
                       int2_c,
                       ext_c, 
                       ext2_c,
                       intext_c) 

{
  
  # Split test parameter values.
  int_try = par[1]
  ext_try = par[2]
  
  
  # Estimate outcomes of test parameters.
  mor = intercept_m + int_m * int_try + ext_m * ext_try + intext_m * int_try * ext_try + int2_m * int_try^2 + ext2_m * ext_try^2 #  + intext2_m * int_try * ext_try
  cos = intercept_c + int_c * int_try + ext_c * ext_try + intext_c * int_try * ext_try + int2_c * int_try^2 + ext2_c * ext_try^2 # + intext2_c * int_try * ext_try
  
  # Get reference parameters from the environment. (These could be arguments.)
  mor_0 = ifelse(inf == "i", mor_i_0, mor_p_0)
  cos_0 = cos_0
  
  # Get a value to minimize.
  out = abs(1 - mor / target) + abs(1 - cos / cos_0)
  
  return(out)
  
}

# convenience functions
fun_mor_reg = function(int,
                       ext,
                       intercept_m, 
                       int_m, 
                       int2_m,
                       ext_m, 
                       ext2_m,
                       intext_m)

{
  
  intercept_m + int_m * int + ext_m * ext + intext_m * int * ext + int2_m * int^2 + ext2_m * ext^2 # + intext2_m * int * ext
  
}

fun_cos_reg = function(int,
                       ext,
                       intercept_c, 
                       int_c, 
                       int2_c,
                       ext_c, 
                       ext2_c,
                       intext_c)

{
  
  intercept_c + int_c * int + ext_c * ext + intext_c * int * ext + int2_c * int^2 + ext2_c * ext^2 # + intext2_c * int * ext
  
}

# Extensions for "model truth" runs, where decisions under the reference case return mortality outcomes informed by the counterfactual case.
#  So, plug intensity and extensity from reference case runs into just the counterfactual's data.
fun_mor_alt = function(data,
                       quantile,
                       intensity, 
                       target)
  
{
  data %>%
    mutate(spe_cou = ifelse(spe_m > intensity, intensity, spe_m),
           mor_m_i_cou = fun_mar(den_w = den_m_i / (area / 10^6), # whales / km^2
                                 v_w = speed_w * kmh_ms, # m/s
                                 v_v = spe_cou * kt_ms, # m/s
                                 l_w = length_w, # m
                                 w_w = width_w, # m
                                 l_v = len_v, # m
                                 w_v = wid_v, # m
                                 s = area, # m^2 / cell
                                 t_v = t * spe_m / spe_cou, # seconds of vessel transit
                                 pro_s = pro_s, # Probability of whale depth and vessel draft coinciding.
                                 pro_a = avoid, # Probability of successful avoidance by either/both the whale or/and the vessel.
                                 v_v_kt = spe_cou,
                                 b_1_cs = b_1_cs,
                                 b_2_cs = b_2_cs),
           mor_s_i_cou = fun_mar(den_w = den_s_i / (area / 10^6), # whales / km^2
                                 v_w = speed_w * kmh_ms, # m/s
                                 v_v = spe_cou * kt_ms, # m/s
                                 l_w = length_w, # m
                                 w_w = width_w, # m
                                 l_v = len_v, # m
                                 w_v = wid_v, # m
                                 s = area, # m^2 / cell
                                 t_v = t * spe_m / spe_cou, # seconds of vessel transit
                                 pro_s = pro_s, # Probability of whale depth and vessel draft coinciding.
                                 pro_a = avoid, # Probability of successful avoidance by either/both the whale or/and the vessel.
                                 v_v_kt = spe_cou,
                                 b_1_cs = b_1_cs,
                                 b_2_cs = b_2_cs),
           mor_m_p_cou = fun_mar(den_w = den_m_p / (area / 10^6), # whales / km^2 (?)
                                 v_w = speed_w * kmh_ms, # m/s
                                 v_v = spe_cou * kt_ms, # m/s
                                 l_w = length_w, # m
                                 w_w = width_w, # m
                                 l_v = len_v, # m
                                 w_v = wid_v, # m
                                 s = area, # m^2 / cell
                                 t_v = t * spe_m / spe_cou, # seconds of vessel transit
                                 pro_s = pro_s, # Probability of whale depth and vessel draft coinciding.
                                 pro_a = avoid, # Probability of successful avoidance by either/both the whale or/and the vessel.
                                 v_v_kt = spe_cou,
                                 b_1_cs = b_1_cs,
                                 b_2_cs = b_2_cs),
           mor_s_p_cou = fun_mar(den_w = den_s_p / (area / 10^6), # whales / km^2 (?)
                                 v_w = speed_w * kmh_ms, # m/s
                                 v_v = spe_cou * kt_ms, # m/s
                                 l_w = length_w, # m
                                 w_w = width_w, # m
                                 l_v = len_v, # m
                                 w_v = wid_v, # m
                                 s = area, # m^2 / cell
                                 t_v = t * spe_m / spe_cou, # seconds of vessel transit
                                 pro_s = pro_s, # Probability of whale depth and vessel draft coinciding.
                                 pro_a = avoid, # Probability of successful avoidance by either/both the whale or/and the vessel.
                                 v_v_kt = spe_cou,
                                 b_1_cs = b_1_cs,
                                 b_2_cs = b_2_cs),
           cos_min_ref = c_min * t,
           cos_min_cou = ifelse(spe_m > spe_cou, c_min * t * (spe_m / intensity), c_min * t),
           cos_min_dif = cos_min_cou - cos_min_ref) %>% # Sneak in a convenient cost calculation.
    group_by(year,
             month,
             id) %>% 
    summarize(mor_m_i_ref = sum(mor_m_i_ref, na.rm = TRUE),
              mor_s_i_ref = sum(mor_s_i_ref ^ 2, na.rm = TRUE) ^ (1 / 2), 
              mor_m_p_ref = sum(mor_m_p_ref, na.rm = TRUE),
              mor_s_p_ref = sum(mor_s_p_ref ^ 2, na.rm = TRUE) ^ (1 / 2),
              mor_m_i_cou = sum(mor_m_i_cou, na.rm = TRUE),
              mor_s_i_cou = sum(mor_s_i_cou ^ 2, na.rm = TRUE) ^ (1 / 2),
              mor_m_p_cou = sum(mor_m_p_cou, na.rm = TRUE),
              mor_s_p_cou = sum(mor_s_p_cou ^ 2, na.rm = TRUE) ^ (1 / 2),
              cos_min_dif = sum(cos_min_dif, na.rm = TRUE)) %>%
    ungroup %>%
    # Check for oddball negative values.
    mutate(mor_m_i_cou = ifelse(mor_m_i_cou > mor_m_i_ref, mor_m_i_ref, mor_m_i_cou),
           mor_m_p_cou = ifelse(mor_m_p_cou > mor_m_p_ref, mor_m_p_ref, mor_m_p_cou)) %>% 
    # Pick places and times.
    mutate(mar_q_i_90_ref = qnorm(quantile, mor_m_i_ref, mor_s_i_ref),
           mar_q_i_90_cou = qnorm(quantile, mor_m_i_cou, mor_s_i_cou),
           mar_q_i_90_dif = mar_q_i_90_ref - mar_q_i_90_cou,
           mar_i = mar_q_i_90_dif / (cos_min_dif + 1), 
           mar_q_p_90_ref = qnorm(quantile, mor_m_p_ref, mor_s_p_ref),
           mar_q_p_90_cou = qnorm(quantile, mor_m_p_cou, mor_s_p_cou),
           mar_q_p_90_dif = mar_q_p_90_ref - mar_q_p_90_cou,
           mar_p = mar_q_p_90_dif / (cos_min_dif + 1)) %>% 
    
    # NB: this section has heinous nomenclature to get the right outputs aligned on mortalities.
    
    arrange(desc(mar_i)) %>%
    mutate(ext = row_number() / n(), # This is sensitive to order, so it needs to be overwritten with reordering.
           mor_q_i_dif_sum = cumsum(mar_q_p_90_dif), # This line works for inputs intensity and target.
           bin_i = ifelse(mor_q_i_dif_sum < (sum(mar_q_p_90_ref) - target), 1, 0)) %>% # This line works for inputs intensity and target.
    arrange(desc(mar_p)) %>%
    mutate(ext = row_number() / n(), # This is sensitive to order, so it needs to be overwritten with reordering.
           mor_q_p_dif_sum = cumsum(mar_q_p_90_dif), # This line works for inputs intensity and target.
           bin_p = ifelse(mor_q_p_dif_sum < (sum(mar_q_p_90_ref) - target), 1, 0)) %>% # This line works for inputs intensity and target.
    
    # End heinous section.
    
    arrange(year, month, id) %>%
    mutate(mor_m_i_out = mor_m_i_ref * abs(bin_i - 1) + mor_m_i_cou * bin_i,
           mor_m_p_out = mor_m_p_ref * abs(bin_p - 1) + mor_m_p_cou * bin_p,
           mor_m_a_out = mor_m_p_ref * abs(bin_i - 1) + mor_m_p_cou * bin_i,
           mor_s_i_out = mor_s_i_ref * abs(bin_i - 1) + mor_s_i_cou * bin_i,
           mor_s_p_out = mor_s_p_ref * abs(bin_p - 1) + mor_s_p_cou * bin_p,
           mor_s_a_out = mor_s_p_ref * abs(bin_i - 1) + mor_s_p_cou * bin_i,
           cos_i = cos_min_dif * bin_i,
           cos_p = cos_min_dif * bin_p,
           cos_a = cos_min_dif * bin_i) %>%
    return

}

#  Find distributions of outcomes under the status quo and counterfactual policies.
fun_agg_alt = function(data)
  
{
  data %>% 
    summarize(mor_m_i_ref = sum(mor_m_i_ref, na.rm = TRUE),
              mor_s_i_ref = sum(mor_s_i_ref ^ 2, na.rm = TRUE) ^ (1/2),
              mor_m_p_ref = sum(mor_m_p_ref, na.rm = TRUE),
              mor_s_p_ref = sum(mor_s_p_ref ^ 2, na.rm = TRUE) ^ (1/2),
              mor_m_i_cou = sum(mor_m_i_out, na.rm = TRUE),
              mor_s_i_cou = sum(mor_s_i_out ^ 2, na.rm = TRUE) ^ (1/2),
              mor_m_p_cou = sum(mor_m_p_out, na.rm = TRUE),
              mor_s_p_cou = sum(mor_s_p_out ^ 2, na.rm = TRUE) ^ (1/2),
              mor_m_a_cou = sum(mor_m_a_out, na.rm = TRUE),
              mor_s_a_cou = sum(mor_s_a_out ^ 2, na.rm = TRUE) ^ (1/2),
              cos_i = sum(cos_i, na.rm = TRUE),
              cos_p = sum(cos_p, na.rm = TRUE))
  
}

#  Combine functions for convenience.
fun_sum_alt = function(data, quantile, intensity, target) # extensity
  
{
  
  fun_mor_alt(data = data, quantile = quantile, intensity = intensity, target = target) %>% fun_agg_alt(data = .) %>% return
  # fun_mor_alt(data = data, quantile = quantile, intensity = intensity, extensity = extensity) %>% fun_agg_alt(data = .) %>% return
  
}
