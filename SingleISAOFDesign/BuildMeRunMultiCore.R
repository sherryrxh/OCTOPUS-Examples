
#### Description ################################################################################################
#   This project was created utilizing the OCTOPUS package located at https://kwathen.github.io/OCTOPUS/ .
#   There are several ReadMe comments in the document to help understand the next steps.
#   When this project was created from the template if an option was not supplied then there
#   may be variables with the prefix TEMP_ which will need to be updated.
################################################################################################### #

# It is a good practice to clear your environment before building your simulation/design object then
# then clean it again before you run simulations with only the minimum variables need to avoid potential
# misuse of variables
remove( list=ls() )

# ReadMe - If needed, install the latest copy of OCTOPUS using the remotes package
#remotes::install_github( "kwathen/OCTOPUS")

library( "OCTOPUS" )

# ReadMe - Useful statements for running on a grid such as linux based grid
if(interactive() || Sys.getenv("SGE_TASK_ID") == "") {
  #The SGE_TASK_ID is used if you are running on a linux based grid
    Sys.setenv(SGE_TASK_ID=1)
}

source( "Functions.R")           # Contains a function to delete any previous results
CleanSimulationDirectories( )   # only call when you want to erase previous results

gdConvWeeksToMonths <- 12/52     # Global variable to convert weeks to months, the g is for global as it may be used


source( "TrialDesign.R")
source( "SimulationDesign.R")
source( "TrialDesignFunctions.R")

dQtyMonthsFU       <- 1
mQtyPatientsPerArm <- matrix( c( 192, 192 ), nrow=1, ncol = 2 )
mMinQtyPats       <- cbind( floor(apply( mQtyPatientsPerArm , 1, sum )/2),  apply( mQtyPatientsPerArm , 1, sum ) )
vMinFUTime        <- rep( dQtyMonthsFU, ncol( mMinQtyPats) )
dQtyMonthsBtwIA   <- 0


vISAStartTimes     <- c(  0 )
nQtyReps           <- 313 # How many replications to simulate each scenario
vPValueCutoffForFutility <- c( 0.9, 0.048 )
vPValueCutoffForSuccess  <- c( 0.048, 0.048 )



cTrialDesign <- SetupTrialDesign( strAnalysisModel   = "TTestOneSided",
                                  strBorrowing       = "AllControls",
                                  mPatientsPerArm    = mQtyPatientsPerArm,
                                  mMinQtyPat         = mMinQtyPats,
                                  vMinFUTime         = vMinFUTime,
                                  dQtyMonthsFU       = dQtyMonthsFU,
                                  dQtyMonthsBtwIA    = dQtyMonthsBtwIA,
                                  vPValueCutoffForFutility = vPValueCutoffForFutility,
                                  
                                  vPValueCutoffForSuccess = vPValueCutoffForSuccess )

cSimulation  <- SetupSimulations( cTrialDesign,
                                  nQtyReps                  = nQtyReps,
                                  strSimPatientOutcomeClass = "Normal",
                                  vISAStartTimes            = vISAStartTimes,
                                  nDesign                   = 1)

#Save the design file because we will need it in the RMarkdown file for processing simulation results
save( cTrialDesign, file="cTrialDesign.RData" )

# Additional Designs ####



# Design Option 2 ####

# Example 2 (Design Option 3): Add interim analysis ( IA ) where the IA is performed at half the patients.
# At the IA if the posterior probability that the difference between treatment and control is is greater than MAV is greater than 0.99
# then a Go decision is reached, if it is less than 0.01 a No Go decision is reached, otherwise the trial continues to the end.
# At the end of the trial if the posterior probability that the difference between treatment and control is is greater than MAV is greater than 0.8
# then a Go decision is reached, if it is less than 0.1 a No Go decision

mQtyPatientsPerArm <- matrix( c( 293,293 ), nrow=1, ncol = 2 )
mMinQtyPats       <- cbind( floor(apply( mQtyPatientsPerArm , 1, sum )/3),floor(2*apply( mQtyPatientsPerArm , 1, sum )/3),  apply( mQtyPatientsPerArm , 1, sum ) )
vMinFUTime        <- rep( dQtyMonthsFU, ncol( mMinQtyPats) )
dQtyMonthsBtwIA   <- 0

vPValueCutoffForFutility <- c( 0.9,   0.048, 0.045 )
vPValueCutoffForSuccess  <- c( 0.001, 0.016, 0.045 )

cTrialDesign2 <- SetupTrialDesign( strAnalysisModel   = "TTestOneSided",
                                   strBorrowing       = "AllControls",
                                   mPatientsPerArm    = mQtyPatientsPerArm,
                                   mMinQtyPat         = mMinQtyPats,
                                   vMinFUTime         = vMinFUTime,
                                   dQtyMonthsBtwIA    = dQtyMonthsBtwIA,
                                   vPValueCutoffForFutility = vPValueCutoffForFutility,
                                   
                                   vPValueCutoffForSuccess = vPValueCutoffForSuccess
                                   
)

cSimulation2 <- SetupSimulations( cTrialDesign2,
                                  nQtyReps                  = nQtyReps,
                                  strSimPatientOutcomeClass = "Normal",
                                  vISAStartTimes            = vISAStartTimes,
                                  nDesign                   = 2 )

cSimulation$SimDesigns[[2]] <- cSimulation2$SimDesigns[[1]]

save( cTrialDesign2, file = "cTrialDesign2.RData" )


#Often it is good to keep the design objects for utilizing in a report

lTrialDesigns <- list( cTrialDesign1 = cTrialDesign, cTrialDesign2 = cTrialDesign2 )
save( lTrialDesigns, file="lTrialDesigns.RData")

# End of multiple design options - stop deleting or commenting out here if not utilizing example for multiple designs.

#  As a general best practice, it is good to remove all objects in the global environment just to make sure they are not inadvertently used.
#  The only object that is needed is the cSimulation object and gDebug, gnPrintDetail.
rm( list=(ls()[ls()!="cSimulation" ]))



# Declare global variable (prefix with g to make it clear)
gDebug        <- FALSE   # Can be useful to set if( gDebug ) statements when developing new functions
gnPrintDetail <- 0       # Higher number cause more printing to be done during the simulation.  A value of 0 prints almost nothing and should be used when running
# large scale simulations.
bDebug2 <- FALSE
# Files specific for this project that were added and are not available in OCTOPUS.
# These files create new generic functions that are utilized during the simulation.
source( 'RunAnalysis.TTestOneSided.R' )
source( 'SimPatientOutcomes.Normal.R' )  # This will add the new outcome
source( "BinaryFunctions.R" )


#################################################################################################### .
# Setup of parallel processing                                                                  ####
#################################################################################################### .

library( "foreach")
library( "parallel" )
library( "doParallel" )
library( "foreach")
library( "utils" )
library( "iterators" )
library( "doSNOW" )
library( "snow" )
source( "RunParallelSimulations.R" ) # This file has a version of simulations that utilize more cores

# Use 1 less than the number of cores available
nQtyCores  <- 8

# The nStartIndex and nEndIndex are used to index the simulations and hence the output files see the RunParallelSimulations.R file
# for more details
RunParallelSimulations( nStartIndex = 1, nEndIndex = nQtyCores,  nQtyCores, cSimulation )

# The next option will run the parallel simulations but with a visual update on the % complete

#RunParallelSimulationsWithUpdate( nStartIndex = 1, nEndIndex = nQtyCores,  nQtyCores, cSimulation )


# Post Process ####
# Create .RData sets of the simulation results
# simsCombined.Rdata - This will have the all results about the platform and decisions made for each ISA
# simsISAX.RData will have additional info about ISA X
# simsMain.RData contain decisions that are made for the platform/ISA

dfTmp <- OCTOPUS::BuildSimulationResultsDataSet( )   # Assigning to dfTmp but the important outputs are saved as .RData

# Now you could knit the SimulationReport.rmd
