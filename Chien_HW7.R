# Nathaniel Chien
# March 24th, 2016
# Hydrologic Modeling - ERE 645
# Homework 7
# MAIN SCRIPT

# Goal: Create a hydrologic model to predict streamflow 
#       for the Delaware River in Hale Eddy, NY.
#       Calculate objective function statistics to determine
#       general model performance.
# Model: Q = P - E


# Variable Descriptions
# Data Frames

# Functions: See accessory file "Chien_HW7_fxns.R"


# Clear all variables
rm(list=ls(all=T))

# Start timer
ptm <- proc.time()

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

#
# SURFACE RUNOFF
#
# Determine snowmelt vs. rain (ft/s)
climate_data = prec_det(climate_data,kmelt)

# Determine antecedant moisture in ft/day
climate_data$am5 = ant_moist(climate_data,am5_init)

# Use Haiths Continuous curve number method to determine curve number for every day
climate_data$cn = haith(climate_data,cn2)

# Determine the surface runoff (ft/s) as a function of cn
# Still using Haiths continuous curve number method
climate_data$surf_run = surface_runoff(climate_data)

#
# INFILTRATION, SOIL MOISTURE, ET, PERCOLATION
#
#
# Determine daily infilitration (ft/s)
climate_data$infilt = climate_data$rain + climate_data$melt - climate_data$surf_run

# Determine the unsaturated zone water storage
climate_data = unsat(climate_data,fcap)

#
# GROUNDWATER!!!!!!!!!!!
#
#
# Subsurface discharge calculation
climate_data$gw_Q = groundwater_Q(climate_data,kb)

# 
# Calculate DISCHARGE!!!!!!!!!!!!!
#
# 
# Convert to cfs by multiplying by drainge area of watershed
drainage_area = 595 * 5280 * 5280 # Convert 595 miles^2 to ft^2
climate_data$pred_Q = (climate_data$surf_run + climate_data$gw_Q) * drainage_area


#
# Prepare Output for Grading!!!!!!!!!!!!!!!!
#
#
# 

# Determine annual average daily streamflow
annAvgPred = aggregate(climate_data$pred_Q,list(climate_data$water_year),mean)
annAvgObs = aggregate(nwis_data$flow_cfs,list(nwis_data$water_year),mean)
annAvg = cbind(annAvgPred,annAvgObs$x)
colnames(annAvg) = c("Water Year","Pred","Obs")

# Determine monthly average streamflow
monthAvgPred = month_Q(climate_data)
monthAvgObs = month_Q_nwis(nwis_data)

# Turn monthly values into a 240 length long array of values so can input into a bias function
monthPredData = vector(mode='double',length=240)
monthObsData = vector(mode='double',length=240)
for (i in seq(2,ncol(monthAvgPred)-1)){
  for (j in seq(1,12)){
    monthPredData[j + 12*(i-2)] = monthAvgPred[j,i]
    monthObsData[j + 12*(i-2)] = monthAvgObs[j,i]
  }
}

# Calculate the bias of annual and monthly average streamflow
bias_ann = bias(annAvgPred$x,annAvgObs$x)
bias_month = bias(monthPredData,monthObsData)

# Nash Sutcliffe Efficiency for annual average daily and monthly average streamflows
nse_ann = nse(annAvgPred$x,annAvgObs$x)
nse_month = nse(monthPredData,monthObsData)

#
# CALIBRATION!!!!!!!!!!!
#
#
# Calibrate Kb manually in a separate script ("Chien_HW7Calibrate.R")
kb_opt = 0.95
cn2_opt = 50
# Use the optimal Kb and cn2 to recalculate climate data
# SURFACE RUNOFF
# Determine snowmelt vs. rain (ft/s)
climate_data2 = prec_det(climate_data,kmelt)
# Determine antecedant moisture in ft/day
climate_data2$am5 = ant_moist(climate_data2,am5_init)
# Use Haiths Continuous curve number method to determine curve number for every day
climate_data2$cn = haith(climate_data2,cn2_opt)
# Determine the surface runoff (ft/s) as a function of cn
# Still using Haiths continuous curve number method
climate_data2$surf_run = surface_runoff(climate_data2)
# INFILTRATION, SOIL MOISTURE, ET, PERCOLATION
# Determine daily infilitration (ft/s)
climate_data2$infilt = climate_data2$rain + climate_data2$melt - climate_data2$surf_run
# Determine the unsaturated zone water storage
climate_data2 = unsat(climate_data2,fcap)
# GROUNDWATER!!!!!!!!!!!
# Subsurface discharge calculation
climate_data2$gw_Q = groundwater_Q(climate_data2,kb_opt)
# Calculate DISCHARGE!!!!!!!!!!!!!
# Convert to cfs by multiplying by drainge area of watershed
climate_data2$pred_Q = (climate_data2$surf_run + climate_data2$gw_Q) * drainage_area

# Calculate Objective Functions
monthAvgPredOPT = month_Q(climate_data)
monthPredDataOPT = vector(mode='double',length=240)
for (i in seq(2,ncol(monthAvgPredOPT)-1)){
  for (j in seq(1,12)){
    monthPredDataOPT[j + 12*(i-2)] = monthAvgPredOPT[j,i]
  }
}
# Determine annual average daily streamflow
annAvgPredOPT = aggregate(climate_data2$pred_Q,list(climate_data2$water_year),mean)
annAvgObsOPT = aggregate(nwis_data$flow_cfs,list(nwis_data$water_year),mean)
annAvgOPT = cbind(annAvgPredOPT,annAvgObsOPT$x)
colnames(annAvgOPT) = c("Water Year","Pred","Obs")

# Calculate the bias of annual and monthly average streamflow
bias_annOPT = bias(annAvgPredOPT$x,annAvgObs$x)
bias_monthOPT = bias(monthPredDataOPT,monthObsData)

# Nash Sutcliffe Efficiency for annual average daily and monthly average streamflows
nse_annOPT = nse(annAvgPredOPT$x,annAvgObs$x)
nse_monthOPT = nse(monthPredDataOPT,monthObsData)

#
# Scatter Plots!!!!!!!!!!!!
#
#
# Scatter plot showing pred vs. obs annual avg daily streamflow vs. water year
plot(cbind(seq(1941,1960)),annAvg$Pred,col="red",pch=16,
     main="Predicted with Default params (red), predictions with optimal params (blue) and Observed (black) Streamflow by Water Year",
     xlab="Water Year",ylab="Streamflow (cfs)",ylim=range(700,1800))
points(cbind(seq(1941,1960)),annAvgOPT$Pred,col="blue",pch=16,xlab="Water Year",ylab="Streamflow (cfs)")
points(cbind(seq(1941,1960)),annAvg$Obs,col="black",pch=16,xlab="Water Year",ylab="Streamflow (cfs)")

# Scatter plot showing pred vs. obs avg monthly streamflow
month_list = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
plot(cbind(seq(1,12)),monthAvgPred$avg,col="red",pch=16,
     main="Predicted (red) and Observed (black) Avg Monthly Streamflow",
     xlab="Month",ylab="Streamflow (cfs)",xaxt="n",ylim=range(0,2500))
points(cbind(seq(1,12)),monthAvgObs$avg,col="black",pch=16,xlab="Month",ylab="Streamflow (cfs)")
axis(1,at=1:12,labels=month_list)

# Hydrograph for observed and predicted streamflow for water year 1960
xObs = nwis_data$flow_cfs[6940:7305]
xPred = climate_data$pred_Q[6940:7305]
plot(seq(1,366),xPred,col="red",type="l",xlab="Time (days)",ylab="Streamflow (cfs)",
     main="Predicted (red) and Observed (black) Daily Streamflow for 1960 water year")
lines(seq(1,366),xObs,col="black")

# End time
time <- proc.time() - ptm

# 
# Format output table with statistics and everything else
#
# Suppress warnings related to file output 
options(warn=-1)
# Output for HW7
write("Nathaniel Chien","Chien_HW7.out",append=FALSE)
write("Output for HW7","Chien_HW7.out",append=TRUE)
write("ERE 645 - Hydrologic Modeling","Chien_HW7.out",append=TRUE)
write("April 19th, 2017","Chien_HW7.out",append=TRUE)
write("__________________________________________________________________________________________________","Chien_HW7.out",append=TRUE)
write("For a Kb of 0.924 and a cn2 of 65, my model had the following objective function values:","Chien_HW7.out",append=TRUE)
write("The Bias for annual average daily streamflow is:","Chien_HW7.out",append=TRUE)
write(bias_ann,"Chien_HW7.out",append=TRUE)
write("The Bias for monthly average daily streamflow is:","Chien_HW7.out",append=TRUE)
write(bias_month,"Chien_HW7.out",append=TRUE)
write("The NSE for annual average daily streamflow is:","Chien_HW7.out",append=TRUE)
write(nse_ann,"Chien_HW7.out",append=TRUE)
write("The NSE for monthly average daily streamflow is:","Chien_HW7.out",append=TRUE)
write(nse_month,"Chien_HW7.out",append=TRUE)
write("__________________________________________________________________________________________________","Chien_HW7.out",append=TRUE)
write("After calibration, the new optimal kb and cn2 are 0.95 and 50, respectively","Chien_HW7.out",append=TRUE)
write("","Chien_HW7.out",append=TRUE)
write("For these parameter values, the Bias for annual average daily streamflow is:","Chien_HW7.out",append=TRUE)
write(bias_annOPT,"Chien_HW7.out",append=TRUE)
write("For these parameter values, the Bias for monthly average daily streamflow is:","Chien_HW7.out",append=TRUE)
write(bias_monthOPT,"Chien_HW7.out",append=TRUE)
write("For these parameter values, the NSE for annual average daily streamflow is:","Chien_HW7.out",append=TRUE)
write(nse_annOPT,"Chien_HW7.out",append=TRUE)
write("For these parameter values, the NSE for monthly average daily streamflow is:","Chien_HW7.out",append=TRUE)
write(nse_monthOPT,"Chien_HW7.out",append=TRUE)
write("__________________________________________________________________________________________________","Chien_HW7.out",append=TRUE)
write("The time to run all the code (except for calibration and output table is:","Chien_HW7.out",append=TRUE)
write(time[3],"Chien_HW7.out",append=TRUE)

# Turn warnings back on...
options(warn=0)
