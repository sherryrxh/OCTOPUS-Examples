##### File Description ######################################################################################################
#   This file is to create the new RunAanlysis that calculates based on a beta-binomial
#   Inputs:
#     cAnalysis - The class( cAnalysis ) determines the specific version of RunAnalysis that is called. It contains
#                 the details about the analysis such as the priors, MAV, TV, decsiion cutoff boundaries. 
#     lDataAna  - The data that is used int he analysis.  Typically contains vISA (the ISA for the patient),
#                 vTrt (treatment for each patient), vOut (the outcome for each patient)
#     nISAAnalysisIndx - index of the analysis used for changing boundaries)
#     bIsFinaISAAnalysis - TRUE or FALSE, often we change the value of the cutoff at the final analysis for an ISA
#     cRandomizer - The randomizer, mainly used for cases with covariates
#
#############################################################################################################################.
RunAnalysis.BetaBinomial <- function( cAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer )
{
    #print( paste( "RunAnalysis.BetaBinomial "))
  
   # Prior are in cAnalysis$vPriorA and cAnalysis$vPriorB
  
    vISA <- lDataAna$vISA
    vTrt <- lDataAna$vTrt
    vOut <- lDataAna$vOut
    
    nISA <- vISA[1]
    # Using the is.na because it could be that a patient has not had the outcome observed at the time of the analysis
    vISA <- vISA[ !is.na( vOut ) ]
    vTrt <- vTrt[ !is.na( vOut ) ]
    vOut <- vOut[ !is.na( vOut ) ]
    
    # Set the Posterior parameters to the prior parameters
    dPostACtrl <- cAnalysis$vPriorA[ 1 ]
    dPostBCtrl <- cAnalysis$vPriorB[ 1 ]
    dPostATrt  <- cAnalysis$vPriorA[ 2 ]
    dPostBTrt  <- cAnalysis$vPriorB[ 2 ]
    
    
    # If the weight is 1 then doing full borrowing, if weight = 0 then no borrowing
    dWeightPatientsOutsideISA <- cAnalysis$dWeightNonISAPatients
    
    
    #Compute Posterior Paramters - Control treatment 
    nNCtrlInISA          <- length( vTrt[ vTrt == 1 & vISA == nISA ] )  # Number of patient on Control in the current ISA
    nNCtrlNotInISA       <- length( vTrt[ vTrt == 1 & vISA != nISA ] )  # Number of patient on Control in OTHER ISAs
    
    nQtyRespCtrlInISA    <- sum( vOut[ vTrt == 1 & vISA == nISA ] )     # Number of responses on Control in current ISA
    nQtyRespCtrlNotInISA <- sum( vOut[ vTrt == 1 & vISA != nISA ] )     # Number of responses on Control in OTHER ISAs
    
    # Update Posterior for Control ####
    # Posterior A = Prior A + Number of responders in ISA + Weight * Number of responders NOT in ISA
    dPostACtrl   <- dPostACtrl + nQtyRespCtrlInISA + dWeightPatientsOutsideISA * nQtyRespCtrlNotInISA 
    
    # Posterior B = Prior B + Number of non-responders in ISA + Weight * Number of non-responders NOT in ISA
    dPostBCtrl   <- dPostBCtrl +  ( nNCtrlInISA - nQtyRespCtrlInISA ) 
    dPostBCtrl   <- dPostBCtrl + dWeightPatientsOutsideISA * ( nNCtrlNotInISA - nQtyRespCtrlNotInISA )     
   
     
    # Compute Posterior Paramters - Treatment ##### 
    # Note: Only the control data is "borrowed" from other ISAs so no need to change anything for the treatment
    nNTrt        <- length( vTrt[ vTrt != 1 ] )
    nQtyRespTrt  <- sum( vOut[ vTrt != 1] )
    dPostATrt    <- dPostATrt + nQtyRespTrt
    dPostBTrt    <- dPostBTrt + nNTrt - nQtyRespTrt
    
    # Want to calcuate Pr( Q_T > Q_C + MAV | Data ) - need to compute posterior parameters
    # ProbX1GrX2PlusDelta
    if( is.na( dPostACtrl) | is.na( dPostBCtrl ) | is.na( dPostATrt ) | is.na( dPostBTrt ) )
    {
      # This is for debugging and should not be hit
      browser()
    }
    #print( paste( "Number of patinets: ", length( vTrt )," Q_C ~ Beta( ",dPostACtrl, dPostBCtrl , "), Q_T ~ Beta( ", dPostATrt, ", ", dPostBTrt, ")"))
    #
   
    
    dPrGrtMAV  <- ProbX1GrX2PlusDelta( dPostATrt,  dPostBTrt,
                                       dPostACtrl, dPostBCtrl,
                                       cAnalysis$dMAV )  
   
    #print( paste( "Pr( Q_Trt > Q_Ctrl + MAV ) = ", dPrGrtMAV ))
    lCutoff    <- GetBayesianCutoffs( cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis )
    
    
    lCalcs     <- list( dPrGrtMAV      = dPrGrtMAV,
                        dPUpperCutoff  = lCutoff$dPUpperCutoff,
                        dPLowerCutoff  = lCutoff$dPLowerCutoff )
    
    lRet       <- MakeDecisionBasedOnPostProb(cAnalysis, lCalcs )
   
    lRet$dPostACtrl <- dPostACtrl
    lRet$dPostBCtrl <- dPostBCtrl
    lRet$dPostATrt  <- dPostATrt
    lRet$dPostBTrt  <- dPostBTrt
    lRet$cRandomizer <- cRandomizer  # Needed because the main code will pull the randomzier off just incase this function were to close a covariate group
    return( lRet )
    
    
}






 
