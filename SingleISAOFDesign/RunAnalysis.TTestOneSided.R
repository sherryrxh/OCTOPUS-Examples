##### File Description ######################################################################################################
#   This file is to create the new RunAanlysis that calculates based on a beta-binomial
#   Inputs:
#     cAnalysis - The class( cAnalysis ) determines the specific version of RunAnalysis that is called. It contains
#                 the details about the analysis such as the priors, MAV, TV, decision cut-off boundaries.
#     lDataAna  - The data that is used int he analysis.  Typically contains vISA (the ISA for the patient),
#                 vTrt (treatment for each patient), vOut (the outcome for each patient)
#     nISAAnalysisIndx - index of the analysis used for changing boundaries)
#     bIsFinaISAAnalysis - TRUE or FALSE, often we change the value of the cut-off at the final analysis for an ISA
#     cRandomizer - The randomizer, mainly used for cases with covariates
#
#############################################################################################################################.
RunAnalysis.TTestOneSided <- function( cAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer )
{
    
    lCI      <- GetCILimits(  cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis )
    dLowerCI <- lCI$dLowerCI
    dUpperCI <- lCI$dUpperCI
    #if( dLowerCI  == 1 - dUpperCI )  #Symmetrical CI so only need to do the test once to get the desired CI
    #{
        dCILevel <- 1.0 - dLowerCI
        
        
        lTTest  <- t.test(lDataAna$vOut ~ as.factor(lDataAna$vTrt), alternative = c("greater"),mu = 0, exact = NULL, correct = TRUE,  conf.int = TRUE, conf.level = dCILevel)
        Means   <- lTTest["estimate"]$estimate[[1]] - lTTest["estimate"]$estimate[[2]] #pbo- trt
        dLower  <- lTTest["conf.int"]$conf.int[[1]]
        dUpper  <- lTTest["conf.int"]$conf.int[[2]]
    # }
    # else
    # {
    #     #The desired CI is not symmetrical so we need to do the test twice
    #     
    #     #Get the CI for the Lower Limit
    #     dCILevel <- 1 - 2*dLowerCI
    #     lTTest  <- t.test(lDataAna$vOut ~ as.factor(lDataAna$vTrt), alternative = c("greater"),mu = 0, exact = NULL, correct = TRUE,  conf.int = TRUE, conf.level = dCILevel)
    #     Means   <- lTTest["estimate"]$estimate[[1]] - lTTest["estimate"]$estimate[[2]] #pbo- trt
    #     dLower  <- lTTest["conf.int"]$conf.int[[1]]
    #     
    #     
    #     dCILevel <- 1 - 2*(1 - dUpperCI)
    #     lTTest  <- t.test(lDataAna$vOut ~ as.factor(lDataAna$vTrt), alternative = c("greater"),mu = 0, exact = NULL, correct = TRUE,  conf.int = TRUE, conf.level = dCILevel)
    #     dUpper  <- lTTest["conf.int"]$conf.int[[2]]
    #     
    # }
    
    lRet <- MakeDecisionBasedOnCI( dLower, dUpper, cAnalysis )
    lRet$cRandomizer <- cRandomizer
    
    return( lRet )
    


}







