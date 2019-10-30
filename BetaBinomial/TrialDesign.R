##### File Description ######################################################################################################
#   This file will setup a trial design that will have the following charteristics
#   3 ISAs - each with control (C) and treatment (T)
#   Binary outcome with probability of response Q_C and Q_T for control and treatment respectively
#   Beta-Binomial analysis model
#   Priors are Q_C ~ Beta( vPriorA[1], vPriorB[1] ) and Q_T ~ Beta( vPriorA[2], vPriorB[2])
#
#############################################################################################################################.

SetupTrialDesign <- function()
{
    dConvWeeksToMonths <- 12/52
    
    # Options for borrowing "NoBorrowing" or "AllControls"
    dWeightNonISAPatients <- 0   # weights of inter-ISA control borrowing, 1 = full borrow, 0=no borrow
    strBorrow             <- "AllControls"
    strModel              <- "BetaBinomial"
    bIncreaseParam        <- TRUE
    dMAV                  <- 0.1
    
    #Decision boundaries - Interim 
    vPUpper               <- c( 1.1, 1.1 )    #If Pr( delta > MAV | data ) > vUpper  --> Go
    vPLower               <- c( -0.1, -0.1 )      #If Pr( delta > MAV | data ) < vPLower  --> NO Go
    
    #Decision boundaries - Final analysis
    dFinalPUpper          <- 0.8           #If Pr( delta > MAV | data ) > dFinalPUpper  --> Go
    dFinalPLower          <- 0.2           #If Pr( delta > MAV | data ) < dFinalPLower  --> No Go
        
    vQtyPats              <- c( 72, 72 )  #number of patients in c(Control, Treatment)
        
    vObsTimeInMonths      <- c( 6 )  # patients have outcome observed at 6 months
    
    #Setup the analysis times
    vMinQtyPats           <- c( 72,144 )   # Start the analysis when 25 patients have 6 months of data
    vMinFUTime            <- c( 6,6)
    dQtyMonthsBtwIA       <- 0 
    
    
    ########################################################################.
    #  ISA 1 Information                                                ####
    ########################################################################.
    
    # Prior parameters for Control, Treatment
    vPriorA   <- c( 0.3, 0.3 )
    vPriorB   <- c( 0.7, 0.7 )
    
    cISA1Info <- CreateISA( vQtyPats              = vQtyPats,
                            vTrtLab               = c( 1, 2 ),
                            vPriorA               = vPriorA,
                            vPriorB               = vPriorB,
                            dWeightNonISAPatients = dWeightNonISAPatients,
                            vObsTimeInMonths      = vObsTimeInMonths,
                            dMAV                  = dMAV,
                            vPUpper               = vPUpper,
                            vPLower               = vPLower,
                            dFinalPUpper          = dFinalPUpper,
                            dFinalPLower          = dFinalPLower,
                            bIncreaseParam        = bIncreaseParam,
                            strBorrow             = strBorrow,
                            strModel              = strModel,
                            vMinQtyPats           = vMinQtyPats,
                            vMinFUTime            = vMinFUTime,
                            dQtyMonthsBtwIA       = dQtyMonthsBtwIA )
         
    
    ########################################################################.
    #  ISA 2 Information                                                ####
    ########################################################################.
    
    # Control, Treatment - For ISA 2 we want more patient on Treatment since we can borrow
    vQtyPats     <- c( 36,72 )  # number of patients in c(Control, Treatment)
    vMinQtyPats  <- c( 54,108 )   # Start the analysis when 25 patients have 6 months of data
    
    # Prior parameters of the Beta( A, B) for Control { Beta( vPriorA[1], vPriorB[1]) } and  
    # Treatment { Beta( vPriorA[ 2 ], vPriorB[ 2 ] ) ]
    vPriorA   <- c( 0.3, 0.3 )  
    vPriorB   <- c( 0.7, 0.7 )
    
    cISA2Info <-CreateISA( vQtyPats              = vQtyPats,
                           vTrtLab               = c( 1, 3 ),
                           vPriorA               = vPriorA,
                           vPriorB               = vPriorB,
                           dWeightNonISAPatients = dWeightNonISAPatients,
                           vObsTimeInMonths      = vObsTimeInMonths,
                           dMAV                  = dMAV,
                           vPUpper               = vPUpper,
                           vPLower               = vPLower,
                           dFinalPUpper          = dFinalPUpper,
                           dFinalPLower          = dFinalPLower,
                           bIncreaseParam        = bIncreaseParam,
                           strBorrow             = strBorrow,
                           strModel              = strModel,
                           vMinQtyPats           = vMinQtyPats,
                           vMinFUTime            = vMinFUTime,
                           dQtyMonthsBtwIA       = dQtyMonthsBtwIA )
    
    
    ########################################################################.
    #  ISA 3 Information                                                ####
    ########################################################################.
    
    # Control, Treatment - For ISA 2 we want more patient on Treatment since we can borrow
    vQtyPats     <- c( 24, 72 )  #number of patients in c(Control, Treatment)
    vMinQtyPats  <- c( 48,96 )   # Start the analysis when 25 patients have 6 months of data    
    # Prior parameters for Control, Treatment
    vPriorA   <- c( 0.3, 0.3 )
    vPriorB   <- c( 0.7, 0.7 )
    
    cISA3Info <-CreateISA( vQtyPats              = vQtyPats,
                           vTrtLab               = c( 1, 4 ),
                           vPriorA               = vPriorA,
                           vPriorB               = vPriorB,
                           dWeightNonISAPatients = dWeightNonISAPatients,
                           vObsTimeInMonths      = vObsTimeInMonths,
                           dMAV                  = dMAV,
                           vPUpper               = vPUpper,
                           vPLower               = vPLower,
                           dFinalPUpper          = dFinalPUpper,
                           dFinalPLower          = dFinalPLower,
                           bIncreaseParam        = bIncreaseParam,
                           strBorrow             = strBorrow,
                           strModel              = strModel,
                           vMinQtyPats           = vMinQtyPats,
                           vMinFUTime            = vMinFUTime,
                           dQtyMonthsBtwIA       = dQtyMonthsBtwIA )
    
    
    
    
    # THe new trial design will use the EqualRandomizer to determine how patients are randomized amoung concurent ISAs.
    # This means that if 2 or more ISAs are open at the same time then there will be an equal chance of the patient being 
    # randomized to each ISA.  WIthing the ISA the patients are randomized according to the vQtyPats above
    cTrialDesign <- NewTrialDesign( list( cISA1Info, cISA2Info, cISA3Info ) , strISARandomizer = "EqualRandomizer" )
    
    
    
    return( cTrialDesign )
    
}
