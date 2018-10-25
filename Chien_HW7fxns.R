# Nathaniel Chien
# April 7th, 2016
# Hydrologic Modeling - ERE 645
# Homework 7
# FUNCTIONS

# List of Functions
#   - nwis_process(data_withcomments): process raw data downloaded from nwis website
# 


# Function to process data generated from NWIS website
# strip strings from top of file and output a data frame
# with streamflow and dates
nwis_process = function(data_withcomments){
  # Read input file from NWIS website and strip the comment lines
  i = 1
  while(toString(data_withcomments[i,1]) != "USGS"){
    i = i + 1 
  }
  nwis_data = data_withcomments[i:nrow(data_withcomments),1:ncol(data_withcomments)]
  row.names(nwis_data) = 1:nrow(nwis_data)
  # Format data file for use in main algorithm
  nwis_data$flow_cfs = as.numeric(as.character(nwis_data$V4))
  # Format dates
  dates = as.Date(nwis_data[,3], "%m/%d/%Y")
  nwis_data$year = c(as.numeric(format(dates, format = "%Y")))
  nwis_data$month = c(as.numeric(format(dates, format = "%m")))
  nwis_data$day = c(as.numeric(format(dates, format = "%d")))
  # Prepare output file
  output = data.frame(cbind(nwis_data$flow_cfs,nwis_data$year, nwis_data$month, nwis_data$day))
  colnames(output) = c("flow_cfs","year","month","day")
  return(output)
}


# Function to add the water year to a data frame of NWIS USGS data
# Start increment at 1 on Oct 1st
nwis_wateryr = function(nwis_data){
  # initiate empty vector to store water years
  water_yr = vector(mode="integer",length=nrow(nwis_data))
  # Iterate through nwis_data to determine water year for each year
  for (i in seq(1,nrow(nwis_data))){
    # Assign first water year
    if (i == 1){
      water_yr[i] = nwis_data[i,2] + 1
    }
    # Assign subsequent water years
    else{
      # If move to a new water year increment
      if ((nwis_data[i,3] != nwis_data[i-1,3]) & (nwis_data[i,3] == 10)){
        water_yr[i] = water_yr[i-1] + 1
      }
      else{
        water_yr[i] = water_yr[i-1]
      }
    }
  }
  nwis_data$water_year = water_yr
  return(nwis_data)
}


# Function to add the water year to the NOAA climate data frame
# Start increment at 1 on Oct 1st
climate_wateryr = function(climate_data){
  # initiate empty vector to store water years
  water_yr = vector(mode="integer",length=nrow(climate_data))
  # Iterate through nwis_data to determine water year for each year
  for (i in seq(1,nrow(climate_data))){
    # Assign first water year
    if (i == 1){
      water_yr[i] = climate_data$year[i] + 1
    }
    # Assign subsequent water years
    else{
      # If move to a new water year increment
      if ((climate_data$month[i] != climate_data$month[i-1]) & (climate_data$month[i] == 10)){
        water_yr[i] = water_yr[i-1] + 1
      }
      else{
        water_yr[i] = water_yr[i-1]
      }
    }
  }
  climate_data$water_year = water_yr
  return(climate_data)
}


# Function to process NOAA cliamte data for missing values
# Requires the use of a properly formatted nwis_data file for reference
#     Need a data frame with formate c(flow,year,month,day,water year)
rid_issues = function(climate_data,nwis_data){
  # Load precipitation and temperature columns
  clean_data = data.frame(climate_data$PRCP,climate_data$TMAX,climate_data$TMIN)
  # Load and format dates
  dates = as.Date(as.character(climate_data[,3]),format="%Y%m%d")
  clean_data$year = c(as.numeric(format(dates, format = "%Y")))
  clean_data$month = c(as.numeric(format(dates, format = "%m")))
  clean_data$day = c(as.numeric(format(dates, format = "%d")))
  # Find missing days (assume if first or last data point, could manually replace easily)
  for (i in seq(1,nrow(clean_data))){
    # Determine if the current day index is the same for the climate and nwis dataframes 
    if (clean_data$day[i] != nwis_data$day[i]){
      newrow = matrix(-9999,1,6)
      colnames(newrow) = colnames(clean_data)
      clean_data = rbind(clean_data[1:i-1,],newrow,clean_data[-(1:i-1),])
      row.names(clean_data) = 1:nrow(clean_data)
      # Set dates of new row
      clean_data$year[i] = nwis_data$year[i]
      clean_data$month[i] = nwis_data$month[i]
      clean_data$day[i] = nwis_data$day[i]
    }
  }
  # Replace data entry errors
  # May need to add more statements here for discoveries of future errors
  for (i in seq(1,nrow(clean_data))){
    if (clean_data$climate_data.TMAX[i] < clean_data$climate_data.TMIN[i]){
      clean_data$climate_data.TMAX[i] = -9999
    }
  }
  # Replace Precipitation missing precip values with 0
  for (i in seq(1,nrow(clean_data))){
    if (clean_data$climate_data.PRCP[i] == -9999){
      clean_data$climate_data.PRCP[i] = 0
    }
  }
  # Fill in missing temperature values
  for (i in seq(1,nrow(clean_data))){
    if (clean_data$climate_data.TMAX[i] == -9999){
      count = 1
      indicate = FALSE
      while (indicate == TRUE) {
        if (clean_data$climate_data.TMAX[i+1] != -9999){
          indicate = TRUE
        }
        else {
          count = count + 1
        }
      }
      clean_data$climate_data.TMAX[i] = ((clean_data$climate_data.TMAX[i+count] - clean_data$climate_data.TMAX[i-1]) / (count + 1)) + clean_data$climate_data.TMAX[i-1]
    }
    if (clean_data$climate_data.TMIN[i] == -9999){
      count = 1
      indicate = FALSE
      while (indicate == TRUE) {
        if (clean_data$climate_data.TMIN[i+1] != -9999){
          indicate = TRUE
        }
        else {
          count = count + 1
        }
      }
      clean_data$climate_data.TMIN[i] = ((clean_data$climate_data.TMIN[i+count] - clean_data$climate_data.TMIN[i-1]) / (count + 1)) + clean_data$climate_data.TMIN[i-1]
    }
  }
  colnames(clean_data) = c("prec","tmax","tmin","year","month","day")
  return(clean_data)
}


# Function to estimate ET using the following equation
# Equation: Evap = 0.021*N^2*e_st/(T+273)
# where N is the number of daylight hours
#       e_st is the saturated water vapor pressure
#       T is the average daily temperature, assumed to 
#          be the average of the maximum and minimum 
#          daily temperatures
evap_calc = function(climate_data){
  # Initialize storage variables
  daylight_hrs = c(9.3,10.4,11.7,13.1,14.3,15.0,14.6,13.6,12.3,10.9,9.7,9.0)
  avg_temp = vector(mode="double",length=nrow(climate_data))
  e_st = vector(mode="double",length=nrow(climate_data))
  evap = vector(mode="double",length=nrow(climate_data))
  
  # Calculate average daily temperature and sat water vapor pressure
  for (i in seq(1,nrow(climate_data))){
    avg_temp[i] = (climate_data$tmax[i] + climate_data$tmin[i]) / 2
    # convert to celsius
    avg_temp[i] = (avg_temp[i] - 32) * 5 / 9
    e_st[i] = 6.108 * exp(17.27 * avg_temp[i] / (237.3 + avg_temp[i]))
  }
  
  # Calculate evaporation
  for (i in seq(1,nrow(climate_data))){
    if (avg_temp[i] <= 0){
      evap[i] = 0
    }
    else {
      evap[i] = 0.021 * (daylight_hrs[climate_data$month[i]])^2 * e_st[i] / (avg_temp[i] + 273)
    }
    # Convert units from cm/day to ft/s
    evap[i] = evap[i] / 30.48 / 24 / 60 / 60
  }
  climate_data$avg_temp_c = c(avg_temp)
  climate_data$evap_fts = c(evap)
  return(climate_data)
}


# Convert Precipitation from in/day to ft/s
prec_convert= function(climate_data){
  for (i in seq(1,nrow(climate_data))){
    climate_data$prec[i] = climate_data$prec[i] / 12 / 24 / 60 / 60
  }
  colnames(climate_data)[1] = c("prec_fts")
  return(climate_data)
}

# Function to determine whether precipitation is rain or snow
prec_det = function(climate_data,kmelt){
  snow = vector(mode="double",length=nrow(climate_data))
  rain = vector(mode="double",length=nrow(climate_data))
  melt = vector(mode="double",length=nrow(climate_data))
  for (i in seq(1,nrow(climate_data))){
    # First set the initial conditions
    if (i == 1){
      snow[i] = snowacc_init
      melt[i] = 0
      rain[i] = climate_data$prec_fts[i]
      # Calculate snow for day 2 based on initial conditions
      if (climate_data$avg_temp_c[i] <= 0){
        snow[i+1] = snow[i] + climate_data$prec_fts[i]
      }
      else{
        snow[i+1] = snow[i] - melt[i]
      }
    }
    # Determine whether precipitation is snow or rain by avg daily temp
    # For below freezing conditions
    else if ((climate_data$avg_temp_c[i] <= 0) & (nrow(climate_data) != i)){
      snow[i+1] = snow[i] + climate_data$prec_fts[i]
      rain[i] = 0
      melt[i] = 0
    }
    # For above freezing conditions
    else if ((climate_data$avg_temp_c[i] > 0) & (nrow(climate_data) != i)){
      rain[i] = climate_data$prec_fts[i]
      melt[i] = min(snow[i],(kmelt*climate_data$avg_temp_c[i]))
      snow[i+1] = snow[i] - melt[i]
    }
    # Calculate rain and melt conditions for last day
    else{
      if (climate_data$avg_temp_c[i] <= 0){
        rain[i] = 0
        melt[i] = 0
      }
      else{
        rain[i] = climate_data$prec_fts[i]
        melt[i] = min(snow[i],(kmelt*climate_data$avg_temp_c[i]))
      }
    }
  }
  climate_data$snow = snow
  climate_data$rain = rain
  climate_data$melt = melt
  return(climate_data)
}


# Determine the 5-day antecedant moisture (ft/day)
# To make the model more efficient, the algorithm constantly updates the
# moisture values of the preceeding 5 days
ant_moist = function(climate_data, am5_init){
  moist = vector(mode="double",length = nrow(climate_data))
  for (i in seq(1,nrow(climate_data))){
    # Manually determine am5 for the first 5 days
    if (i == 1){
      moist[i] = am5_init
    }
    else if (i == 2){
      moist[i]=(climate_data$rain[i-1] + climate_data$melt[i-1])
    }
    else if (i == 3){
      moist[i]=(climate_data$rain[i-1] + climate_data$melt[i-1]) + (climate_data$rain[i-2] + climate_data$melt[i-2])
    }
    else if (i == 4){
      moist[i]=(climate_data$rain[i-1] + climate_data$melt[i-1]) + (climate_data$rain[i-2] + climate_data$melt[i-2]) + 
        (climate_data$rain[i-3] + climate_data$melt[i-3])
    }
    else if (i == 5){
      moist[i]=(climate_data$rain[i-1] + climate_data$melt[i-1]) + (climate_data$rain[i-2] + climate_data$melt[i-2]) + 
        (climate_data$rain[i-3] + climate_data$melt[i-3]) + (climate_data$rain[i-4] + climate_data$melt[i-4])
    }
    # Determine the antecedant moisture for every other day
    else{
      moist[i]=(climate_data$rain[i-1] + climate_data$melt[i-1]) + (climate_data$rain[i-2] + climate_data$melt[i-2]) + 
        (climate_data$rain[i-3] + climate_data$melt[i-3]) + (climate_data$rain[i-4] + climate_data$melt[i-4]) + 
        (climate_data$rain[i-5] + climate_data$melt[i-5])
    }
  }
  # Convert from ft/s to ft/day because the Haith CN method requires ft/day
  moist = moist * 60 * 60 * 24
  return(moist)
}


# Determine the CN from Haith's continuous CN method
haith = function(climate_data,cn2){
  # Calculate area-weighted curve numbers
  cn1 = cn2 / (2.334 - 0.01334 * cn2)
  cn3 = cn2 / (0.4036 + 0.0059 * cn2)
  # Initiate storage vector for curve numbers
  dailyCN = vector(mode="double",length=nrow(climate_data))
  # Iterate through and determine cn for every day
  for (i in seq(1,nrow(climate_data))){
    # determine if growing season
    if (4 < climate_data$month[i] & climate_data$month[i] < 11){
      am1 = 0.11811          # 3.6 cm/day to feet/day
      am2 = 0.173885         # 5.3 cm/day to feet/day             
    }
    # dormant season
    else{
      am1 = 0.0426509        # 1.3 cm/day to feet/day
      am2 = 0.0918635        # 2.8 cm/day to feet/day
    }
    # Determine curve number for different antecedant moisture conditions
    # If there is snow melt then the cn should be for the wettest conditions 
    if (climate_data$melt[i] > 0){
      cn = cn3
    }
    else if (climate_data$am5[i] < am1){
      cn = cn1 + (cn2 - cn1) / am1 * climate_data$am5[i]
    }
    else if (climate_data$am5[i] < am2){
      cn = cn2 + (cn3 - cn2)/(am2 - am1)*(climate_data$am5[i] - am1)
    }
    else{
      cn = cn3
    }
    dailyCN[i] = cn
  }
  return(dailyCN)
}


# Determine the surface runoff using Surface Runoff = 1000 /cn - 10 (inches/day)
surface_runoff = function(climate_data){
  surf_run = vector(mode="double",length=nrow(climate_data))
  for (i in seq(1,nrow(climate_data))){
    # Determine the melt and rain component
    water = climate_data$rain[i] + climate_data$melt[i]
    # calculate S and convert to ft/s from in/day
    s = (1000 / climate_data$cn[i] - 10) / 12 / 24 / 60 / 60
    # If there is no melt/rain -> no surface runoff
    if (water <= 0.2 * s){
      surf_run[i] = 0
    }
    # Calculate surface runoff using SCS method
    else{
      surf_run[i] = (water - 0.2 * s)^2 / (water + 0.8 * s)
    }
  }
  return(surf_run)
}

# Determine the actual ET from the potential ET (calculated with Hamon's method)
unsat = function(climate_data,fcap){
  # Set a vector of crop cover coefficients, determined from data table
  ku = c(0.78,0.82,0.82,0.79,0.89,0.91,0.93,0.98,1.03,0.97,0.72,0.61)
  # Set storage vectors
  storage = vector(mode="double",length=nrow(climate_data))
  perc = vector(mode="double",length=nrow(climate_data))
  for (i in seq(1,nrow(climate_data))){
    if (i==1){
      storage[i] = fcap
    }
    # determine other parameters needed for storage
    actual_ET = min((ku[climate_data$month]*climate_data$evap_fts[i]),storage[i])
    store_temp = storage[i] + climate_data$infilt[i] - actual_ET
    # Percolation only occurs when field capacity of the unsaturated zone is exceeded.
    if (store_temp <= fcap){
      perc[i] = 0
      store_next = store_temp
    }
    else{
      perc[i] = store_temp - fcap
      store_next = fcap
    }
    # Determine storage for other days
    if (i != nrow(climate_data)){
      storage[i + 1] = store_next
    }
  }
  climate_data$storage = storage
  climate_data$perc = perc
  return(climate_data)
}

# Determine saturated zone discharge
groundwater_Q = function(climate_data,kb){
  subsurf_Q = vector(mode="double",length=nrow(climate_data))
  sat_store = vector(mode="double",length=nrow(climate_data))
  for (i in seq(1,nrow(climate_data))){
    if (i == 1){
      sat_store[i] = satstore_init
    }
    subsurf_Q[i] = (1 - kb) * sat_store[i]
    if (i != nrow(climate_data)){
      sat_store[i+1] = sat_store[i] + climate_data$perc[i] - subsurf_Q[i]
    }
  }
  return(subsurf_Q)
}

# Calculate the average streamflow of the NOAA data for every month
month_Q = function(climate_data){
  # Calculate monthly average for each water year
  sets_wateryrs = split(climate_data,climate_data$water_year)
  monthly_avg = vector(mode="list",length=length(sets_wateryrs))
  for (i in seq(1,length(sets_wateryrs))){
    monthly_avg[i] = aggregate(lapply(sets_wateryrs[i],"[[",20),lapply(sets_wateryrs[i],"[[",5),mean)[2]
  }
  # Format output table for readability
  months_name = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  output = data.frame(monthly_avg)
  colnames(output) = seq(1941,1960)
  output = cbind(months_name,output)
  # Calculate average
  temp_month_avg = vector(mode="double",length=12)
  for (i in seq(1,12)){
    temp_month_avg[i] = 0
    for (j in seq(2,ncol(output))){
      temp_month_avg[i] = temp_month_avg[i] + output[i,j]
    }
    temp_month_avg[i] = temp_month_avg[i] / 20
  }
  output$avg = temp_month_avg
  return(output)
}

# Calculate the avg streamflow of the USGS nwis data for every month
month_Q_nwis = function(nwis_data){
  # Calculate monthly average for each water year
  sets_wateryrs = split(nwis_data,nwis_data$water_year)
  monthly_avg = vector(mode="list",length=length(sets_wateryrs))
  for (i in seq(1,length(sets_wateryrs))){
    monthly_avg[i] = aggregate(lapply(sets_wateryrs[i],"[[",1),lapply(sets_wateryrs[i],"[[",3),mean)[2]
  }
  # Format output table for readability
  months_name = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  output = data.frame(monthly_avg)
  colnames(output) = seq(1941,1960)
  output = cbind(months_name,output)
  # Calculate average
  temp_month_avg = vector(mode="double",length=12)
  for (i in seq(1,12)){
    temp_month_avg[i] = 0
    for (j in seq(2,ncol(output))){
      temp_month_avg[i] = temp_month_avg[i] + output[i,j]
    }
    temp_month_avg[i] = temp_month_avg[i] / 20
  }
  output$avg = temp_month_avg
  return(output)
}

# Calculate the bias for an array of predicted and observed values
bias = function(pred,obs){
  sum_temp = 0 
  for (i in seq(1,length(pred))){
    sum_temp = sum_temp + (pred[i] - obs[i])
  }
  output = sum_temp / length(pred)
  return(output)
}

# Calculate the NSE for an array of predicted and observed values
nse = function(pred,obs){
  num = 0
  denom = 0
  mean_obs = mean(obs)
  for (i in seq(1,length(pred))){
    num = num + (pred[i] - obs[i])^2
    denom = denom + (obs[i] - mean_obs)^2
  }
  output = 1 - (num/denom)
  return(output)
}

