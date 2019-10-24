##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


SetupTrialDesign <- function()
{
    dConvWeeksToMonths <- 12/52


    strBorrow <- "AllControls"
    strModel  <- "BetaBinomial"
    vPUpper   <- c( 1.0, 1.0 )
    vPLower   <- c( 0.0, 0.0 )
    dFinalPUpper <- 0.9
    dFinalPLower <- 0.1

    vQtyPats     <- c( 50, 50 )  # Control, Treatment
    vMinQtyPats  <- c( 25, 100 )
    vMinFUTime   <- c( 6, 6)
    dQtyMonthsBtwIA  <- 1


    ########################################################################.
    #  ISA 1 Information                                                ####
    ########################################################################.

    cISA1Info <- Create1DosePh2AISA( vQtyPats     = vQtyPats,
                                     vTrtLab      = c( 1, 2 ),
                                     vPUpper      = vPUpper,
                                     vPLower      = vPLower,
                                     dFinalPUpper = dFinalPUpper,
                                     dFinalPLower = dFinalPLower,
                                     strBorrow    = strBorrow,
                                     strModel     = strModel,
                                     vMinQtyPats  = vMinQtyPats,
                                     vMinFUTime   = vMinFUTime,
                                     dQtyMonthsBtwIA = dQtyMonthsBtwIA )
    

    ########################################################################.
    #  ISA 2 Information                                                ####
    ########################################################################.

    # Control, Treatment - For ISA 2 we want more patient on Treatment since we can borrow
    vQtyPats     <- c( 25, 75 )  
    cISA2Info <- Create1DosePh2AISA( vQtyPats     = vQtyPats,
                                     vTrtLab      = c( 1, 3 ),
                                     vPUpper      = vPUpper,
                                     vPLower      = vPLower,
                                     dFinalPUpper = dFinalPUpper,
                                     dFinalPLower = dFinalPLower,
                                     strBorrow    = strBorrow,
                                     strModel     = strModel,
                                     vMinQtyPats  = vMinQtyPats,
                                     vMinFUTime   = vMinFUTime,
                                     dQtyMonthsBtwIA = dQtyMonthsBtwIA )




    cTrialDesign <- NewTrialDesign( list( cISA1Info, cISA2Info ), strISARandomizer = "EqualRandomizer" )



    return( cTrialDesign )

}
