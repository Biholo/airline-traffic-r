# Data Processing Logic
# Extracted from Projet - Groupe 2.Rmd

library(dplyr)
library(lubridate)

prepare_flight_data <- function(flights, airlines, airports) {
  
  # --- 1. Helper Functions ---
  
  # Convertit HHMM en datetime
  to_datetime <- function(year, month, day, time_col) {
    time_str <- ifelse(is.na(time_col), NA, sprintf("%04d", time_col))
    hour <- ifelse(is.na(time_str), NA_integer_, as.integer(substr(time_str, 1, 2)))
    minute <- ifelse(is.na(time_str), NA_integer_, as.integer(substr(time_str, 3, 4)))
    lubridate::make_datetime(
      year  = year,
      month = month,
      day   = day,
      hour  = hour,
      min   = minute,
      sec   = 0
    )
  }
  
  # Convertir hhmm (ex: 524, 1330) en minutes depuis minuit
  hhmm_to_minutes <- function(time) {
    time <- as.numeric(time)
    hour <- floor(time / 100)
    minute <- time %% 100
    out <- hour * 60 + minute
    out[is.na(time)] <- NA
    return(out)
  }
  
  # --- 2. Main Processing ---
  
  # Step A: Basic Datetime Construction
  flights2 <- flights %>%
    mutate(
      sched_dep_datetime = to_datetime(year, month, day, sched_dep_time),
      dep_datetime       = to_datetime(year, month, day, dep_time),
      arr_datetime       = to_datetime(year, month, day, arr_time),
      sched_arr_datetime = to_datetime(year, month, day, sched_arr_time),
      date = as_date(sched_dep_datetime)
    )
  
  # Step B: Advanced Delay Calculation (Handling Midnight)
  df2 <- flights2 %>%
    mutate(
      arr_time_min   = ifelse(is.na(arr_time), NA_real_, 
                              hhmm_to_minutes(arr_time)),
      sched_arr_min  = ifelse(is.na(sched_arr_time), NA_real_, 
                              hhmm_to_minutes(sched_arr_time)),
      dep_time_min   = ifelse(is.na(dep_time), NA_real_, 
                              hhmm_to_minutes(dep_time)),
      sched_dep_min  = ifelse(is.na(sched_dep_time), NA_real_, 
                              hhmm_to_minutes(sched_dep_time))
    ) %>%
    rowwise() %>%
    mutate(
      arr_delay_calc = if(any(is.na(c(arr_time_min, sched_arr_min)))) NA_real_ else {
        delta <- arr_time_min - sched_arr_min
        if(delta < -720) delta <- delta + 1440
        if(delta > 720) delta <- delta - 1440
        delta
      },
      dep_delay_calc = if(!is.na(dep_delay)) dep_delay 
      else if(any(is.na(c(dep_time_min, sched_dep_min)))) NA_real_ else {
        delta2 <- dep_time_min - sched_dep_min
        if(delta2 < -720) delta2 <- delta2 + 1440
        if(delta2 > 720) delta2 <- delta2 - 1440
        delta2
      }
    ) %>%
    ungroup()
  
  # Replace original delay columns with calculated ones if needed
  df2 <- df2 %>%
    rename(arr_delay_raw = arr_delay, dep_delay_raw = dep_delay) %>%
    mutate(
      arr_delay = arr_delay_calc,
      dep_delay = dep_delay_calc
    ) %>%
    select(-arr_time_min, -sched_arr_min, -dep_time_min, -sched_dep_min, 
           -arr_delay_calc, -dep_delay_calc)
  
  # Join Airline Info
  if(!("carrier" %in% names(airlines))) {
    # Ensure airlines has 'carrier' column
    # Assuming airlines.json structure might vary, but usually it has carrier code as key or field
    # If airlines is a dataframe with code/name
    if("carrier" %in% names(airlines)) {
       # Good
    } else if ("code" %in% names(airlines)) {
       airlines <- airlines %>% rename(carrier = code)
    } else {
       # Fallback if names are just columns 1 and 2
       names(airlines) <- c("carrier", "name")[1:ncol(airlines)]
    }
  }
  
  df2 <- df2 %>% left_join(airlines, by = "carrier")
  
  # Step C: Master Table with Indicators
  df_master <- df2 %>%
    mutate(
      delayed_arr = ifelse(is.na(arr_delay), NA, arr_delay > 0),
      delayed_dep = ifelse(is.na(dep_delay), NA, dep_delay > 0),
      early_arr   = ifelse(is.na(arr_delay), NA, arr_delay < 0),
      early_dep   = ifelse(is.na(dep_delay), NA, dep_delay < 0),
      gain_vol    = ifelse(is.na(dep_delay) | is.na(arr_delay), NA, dep_delay - arr_delay),
      # gain_per_hour = gain en minutes divisé par air_time en heures
      gain_per_hour = ifelse(is.na(gain_vol) | is.na(air_time) | 
                               air_time <= 0, NA, gain_vol / (air_time/60)),
      rattrape    = ifelse(is.na(arr_delay) | is.na(dep_delay), NA, arr_delay < dep_delay)
    )
  
  return(df_master)
}

filter_flight_data <- function(df_master) {
  # Filtrage des Valeurs Aberrantes (Phase 2I)
  df_filtered <- df_master %>%
    filter(!is.na(arr_delay), !is.na(dep_delay),
           arr_delay >= -30, arr_delay <= 1440,  # tolérer quelques cas extrêmes
           dep_delay >= -30, dep_delay <= 1440,
           !is.na(distance), distance > 0,
           !is.na(air_time), air_time > 0)
  
  return(df_filtered)
}
