
################################################################################################### #
#   Description - This file will add a new patient outcome where patient outcomes are binary
#
#   TODO: Test this function.
################################################################################################### #

SimPatientOutcomes.Normal <- function(  cSimOutcomes, cISADesign, dfPatCovISA )
{
    print( "SimPatientOutcomes.NormalWithPointMass")
    if( XRDebug == TRUE )
        browser()
    
    #print( "Executing SimPatientOutcomes.Normal ...")
    if( !is.null(  dfPatCovISA  ) )
        stop( "SimPatientOutcomes.Binary is not designed to incorporate patient covariates and  dfPatCovISA  is not NULL.")
    
    
    mOutcome        <- NULL
    
    vMeans          <- cSimOutcomes$vTrueMean
    vQtyPats        <- cISADesign$vQtyPats
    dStdDev         <- cSimOutcomes$dTrueStdDev
    vPointVal <- cSimOutcomes$vPointVal
    vPointPct <- cSimOutcomes$vPointPct
    
    vPatTrt         <- rep( cISADesign$vTrtLab, vQtyPats )
    iArm            <- 1
    for( iArm in 1:length( vQtyPats ) )
    {
        #Sim x% Point Mass
        ids <- 1:vQtyPats[ iArm ]
        N.Point <- floor(vQtyPats[ iArm ]*vPointPct[iArm])
        id.Point <- sample(ids, N.Point)
        vPatOut <- rnorm( vQtyPats[ iArm ], vMeans[ iArm ], dStdDev )
        vPatOut[id.Point] <- vPointVal[iArm]

        
        #print( paste( "iArm ", iArm, " Prob Resp ", vProbResponse[ iArm ], " # Pats ", vQtyPats[ iArm ], " % resp ", sum(vPatientOutcomes)/vQtyPats[iArm]))
        mOutcome         <- rbind( mOutcome, matrix( vPatOut , ncol = 1) )
    }
    
    
    lSimDataRet <- structure( list( mSimOut1 = mOutcome, vObsTime1 = cISADesign$cISAAnalysis$vAnalysis[[1]]$vObsTime ), class= class(cSimOutcomes) )
    
    
    lSimDataRet$nQtyOut  <- 1#length( cSimOutcomes )
    lSimDataRet$vPatTrt  <- vPatTrt
    
    return( lSimDataRet )
    
}

