RunParallelSimulations <- function( nQtyCores, cSimulation ) 
{
    tryCatch( 
    {
        myCluster2 <- makeCluster( nQtyCores )
        registerDoParallel( myCluster2 )
        
        dStartTime <- Sys.time()
        mResults <- foreach( i = 1:nQtyCores, .combine= rbind) %dopar%{
            # load any libraries you need
            #library( doParallel )
            
            #This chunk of code will be run for each instance of the loop so it identifies stuff you need such as new functions ect
            # Need to define any functions or variables you want to use in the function you call.   
            # Typically I would be sourcing files here if I needed to or I load the custom R package I have with my simulation code in it. 
            RunSimulationsOnCore <- function( nTrialID, cSimulation ) 
            {
                gDebug <- FALSE
                Sys.setenv(SGE_TASK_ID= nTrialID )
                gnPrintDetail <- 1       # Higher number cause more printing to be done during the simulation.  A value of 0 prints almost nothing and should be used when running
                # large scale simulations.
                
                # Files specific for this project that were added and are not available in OCTOPUS.
                # These files create new generic functions that are utilized during the simulation.
                # These files and OCTOPUS are loaded here because it needs to be done for each core.
                library( OCTOPUS )
                source( 'RunAnalysis.CategoricalAnalysis.R' )
                source( 'SimPatientOutcomes.Categorical.R' )  # This will add the new outcome
                source( "BinaryFunctions.R" )
                RunSimulation( cSimulation )
                
            }
            
            RunSimulationsOnCore( nTrialID = i, cSimulation ) 
        }
        dEndTime <- Sys.time()
        print( paste( "It took ", difftime(dEndTime,  dStartTime, units = "mins"), " minutes to run the simulation. "))
        
    },
    error = function( e ){
        message( "An error occured:\n", e )
    },
    warning = function(w){
        message("A warning occured:\n", w)
    },
    finally = {
        message("Stopping cluster")
        stopCluster( myCluster2  )
    })
}