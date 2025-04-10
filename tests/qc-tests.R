
## Test that qc functions work

options(width = 80)

library(nhanesA)
library(phonto)


## By default, qc_var extracts relevant metadata subsets from the DB
## as needed. As these tables are not that big, we could also
## pre-fetch them and supply to qc_var().

var <- phonto:::metadata_var()
cb <-  phonto:::metadata_cb()
tab <- phonto:::metadata_tab()

qc_var("PHAFSTMN", var, cb, tab)
qc_var("LBCBHC", var, cb, tab)
qc_var("ENQ100", var, cb, tab)
qc_var("LBXHCT", var, cb, tab)

system.time(qc_var("LBCBHC", var, cb, tab))
system.time(qc_var("LBCBHC"))


qc_var("PHAFSTMN")
qc_var("ENQ100")

qc_var("LBCBHC")
qc_var("LBXHCT")


