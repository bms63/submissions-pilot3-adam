###########################################################################
#' developers : Nicole Jones
#' date: 03FEB2023
#' modification History:
#' QC ADLBC
###########################################################################

library(haven)
library(diffdf)

adlbc <- read_xpt(file.path("submission", "datasets", "adlbc.xpt"))
qc_adlbc <- read_xpt(file.path("adam", "adlbc.xpt"))


diffdf(adlbc, qc_adlbc, keys = c("USUBJID", "PARAMCD", "AVISIT", "LBSEQ"))