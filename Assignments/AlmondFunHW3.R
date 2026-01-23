# Almond Yield Anomaly Model Function
# Based on Lobell et al. (in press) equation from Table 2
#
# INPUTS:
#   clim: data frame with columns:
#     - day: day of month
#     - month: month (1-12)
#     - year: year
#     - wy: water year
#     - tmax_c: maximum daily temperature (degrees C)
#     - tmin_c: minimum daily temperature (degrees C)
#     - precip: daily precipitation (mm)
#
# OUTPUTS:
#   data frame with columns:
#     - year: year
#     - yield_anomaly: almond yield anomaly (ton/acre)

almond_yield_anomaly <- function(clim) {
  
  library(dplyr)
  
  # Aggregate daily climate data to monthly
  monthly_clim <- clim %>%
    group_by(year, month) %>%
    summarise(
      Tn = mean(tmin_c, na.rm = TRUE),  # Monthly mean min temp
      P = sum(precip, na.rm = TRUE),    # Monthly total precip
      .groups = 'drop'
    )
  
  # Extract February minimum temperature (month 2)
  feb_tmin <- monthly_clim %>%
    filter(month == 2) %>%
    select(year, Tn_2 = Tn)
  
  # Extract January precipitation (month 1)
  jan_precip <- monthly_clim %>%
    filter(month == 1) %>%
    select(year, P_1 = P)
  
  # Merge climate variables by year
  yield_data <- feb_tmin %>%
    left_join(jan_precip, by = "year")
  
  # Apply Lobell et al. model equation for almonds
  # Y = -0.015*Tn,2 - 0.0046*Tn,2^2 - 0.07*P1 + 0.0043*P1^2 + 0.28
  yield_data <- yield_data %>%
    mutate(
      yield_anomaly = -0.015 * Tn_2 - 
        0.0046 * Tn_2^2 - 
        0.07 * P_1 + 
        0.0043 * P_1^2 + 
        0.28
    )
  
  return(yield_data)
}