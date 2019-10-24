  

remove( list=ls() )

#install.packages("OCTOPUS_1.3.0.4.tar.gz", repos = NULL, type = "source", lib="RLibrary")

library( "OCTOPUS", lib.loc = "RLibrary" )

#This is just for running on the grid
if (interactive() || Sys.getenv("SGE_TASK_ID") == "") {
    Sys.setenv(SGE_TASK_ID=1)
}

source( "Functions.R")  # Contains a function to delete any previous results 
CleanSimulationDirectories( )   #only call when you want to erase previous results

#library( nlme)
#library( coin)

gdConvWeeksToMonths <- 12/52    #Global variable to convert weeks to months

################################################################################################### #
#   Source the setup files                                                                       ####
################################################################################################### #
source( "TrialDesign.R")
source( "SimulationDesign.R")
source( "TrialDesignFunctions.R")

cTrialDesign <- SetupTrialDesign( )
cSimulation  <- SetupSimulations( cTrialDesign, nQtyReps=5 )

#Save the design file because we will need it in the RMarkdown file for processing simulation results
save( cTrialDesign, file="cTrialDesign.RData" )


#  As a general best practice it is good to remove all objects in the global environment just to make sure they are not inadvertently used.
#  The only object that is needed is the cSimulation object.
rm( list=(ls()[ls()!="cSimulation" ]))
gdConvWeeksToMonths <- 12/52    #Global variable to convert weeks to months

gDebug <- FALSE

#Files specific for specific example
source( "RunAnalysis.BetaBinomial.R")
source( "SimPatientOutcomes.Binary.R")  # This will add the new outcome 
source( "BinaryFunctions.R" )
# In this case study we utilize the MVNWithCovarite patient simulator from the base package
gnPrintDetail <-0
RunSimulation( cSimulation )



# If running on a single instance could just increase the nQtyReps above and use code as is up to the RunSimulation() line.  
# However, to "simulate" running this on the grid and getting multiple output files, combining them 
# then creating an R markdown document 

# Due to a bug in the BuildSimulationResultsDataSet( ) we need to run at lease nSGETask = 2
vSGETasks <- 2:200  # This will give us 500 reps (50 * 5)

gDebug <- FALSE
for ( i in 1:length( vSGETasks ) )
{
    nSGETask <- vSGETasks[ i ]
    Sys.setenv(SGE_TASK_ID= nSGETask )
    print( paste( "Simulating task ", i, " of ", length( vSGETasks ), "..."))
    RunSimulation( cSimulation )
}
