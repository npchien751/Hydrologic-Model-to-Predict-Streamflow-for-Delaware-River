# Nathaniel Chien
# March 24th, 2016
# Hydrologic Modeling - ERE 645
# Homework 7
# MAIN SCRIPT

# Goal: Create a hydrologic model to predict streamflow 
#       for the Deleware River in Hale Eddy, NY.
#       Calculate objective function statistics to determine
#       general model performance.
# Model: Q = P - E


# Variable Descriptions
# Data Frames

# Functions: See accessory file "Chien_HW7_fxns.R"


# Clear all variables
rm(list=ls(all=T))

# Load functions I've created
source("Chien_HW7fxns.R")

# Read input file from NWIS website and strip the comment lines
nwis_data = nwis_process(read.csv(file="nwis_data.csv", header=FALSE))

# Add the water year to the nwis_data data frame
nwis_data = nwis_wateryr(nwis_data)

# Read metereological data file and process with self-made function
climate_data = read.csv(file="metereological_data.csv", header=TRUE)
climate_data = rid_issues(climate_data,nwis_data)

# Function to estimate potential ET
climate_data = evap_calc(climate_data)

# Convert precip from in/day to ft/s
climate_data = prec_convert(climate_data)

# Add appropriate water year to climate data frame
climate_data = climate_wateryr(climate_data)

#
#
# Begin calculating different components of water budget
#
#
# Set constants
kb = 0.924              # Baseflow Recession Coefficient
fcap = 10               # initial unsaturated storage is at field capacity of 10 cm
cn2 = 65                # Curve number for average moisture conditions
am5_init = 0            # Initial antecedant moisture coniditions
snowacc_init = 0        # Initial snow accumulation
satstore_init = 0.265   # Initial saturated zone storage in cm
gw_store = NA           # No deep groundwater storage
kmelt = 0.45            # Snow melt constant from B.A. Stewart et al. (1975)
# Change constants to from cm/day to feet/sec
fcap = fcap * 0.0328084 / 24 / 60 / 60
satstore_init = satstore_init * 0.0328084 / 24 / 60 / 60

# Monthly average observed streamflow
monthAvgObs = month_Q_nwis(nwis_data)
monthObsData = vector(mode='double',length=240)
monthPredData = vector(mode='double',length=240)
for (i in seq(2,ncol(monthAvgObs)-1)){
  for (j in seq(1,12)){
    monthObsData[j + 12*(i-2)] = monthAvgObs[j,i]
  }
}

# CALIBRATION
calibrationMatrix = matrix(NA,length(seq(.8,1,.01))*length(seq(50,90,1)),3)
count = 1
for (i in seq(0.8,1,.01)){ # Iterate through Kbs 
  kb = i
  for (j in seq(50,90,1)){ # Iterate through cn2s
    cn2 = j
    # SURFACE RUNOFF
    # Determine snowmelt vs. rain (ft/s)
    climate_data = prec_det(climate_data,kmelt)
    # Determine antecedant moisture in ft/day
    climate_data$am5 = ant_moist(climate_data,am5_init)
    # Use Haiths Continuous curve number method to determine curve number for every day
    climate_data$cn = haith(climate_data,cn2)
    # Determine the surface runoff (ft/s) as a function of cn
    # Still using Haiths continuous curve number method
    climate_data$surf_run = surface_runoff(climate_data)
    # INFILTRATION, SOIL MOISTURE, ET, PERCOLATION
    # Determine daily infilitration (ft/s)
    climate_data$infilt = climate_data$rain + climate_data$melt - climate_data$surf_run
    # Determine the unsaturated zone water storage
    climate_data = unsat(climate_data,fcap)
    # GROUNDWATER!!!!!!!!!!!
    # Subsurface discharge calculation
    climate_data$gw_Q = groundwater_Q(climate_data,kb)
    # Calculate DISCHARGE!!!!!!!!!!!!!
    # Convert to cfs by multiplying by drainge area of watershed
    drainage_area = 595 * 5280 * 5280 # Convert 595 miles^2 to ft^2
    climate_data$pred_Q = (climate_data$surf_run + climate_data$gw_Q) * drainage_area
    
    
    #
    # Populate storage matrix with nse values to determine optimum
    #
    monthAvgPred = month_Q(climate_data)
    # Turn monthly data into a list n=240
    for (a in seq(2,ncol(monthAvgPred)-1)){
      for (b in seq(1,12)){
        monthPredData[b + 12*(a-2)] = monthAvgPred[b,a]
      }
    }
    # Update Calibration Matrix
    calibrationMatrix[count,1] = i
    calibrationMatrix[count,2] = j
    calibrationMatrix[count,3] = nse(monthPredData,monthObsData)
    count = count + 1
  }
}

