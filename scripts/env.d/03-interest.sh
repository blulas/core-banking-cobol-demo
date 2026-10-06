# --- domain 03: deposit interest, CDs, fees, statements, tax reporting -------
export DD_INTSEED="$S/intstate.psv"          # interest/cycle state snapshot (LDINTST)
export DD_RATECHG="$O/ratechg.rpt"           # INTACCR rate change / accrual report
export DD_INTCAPR="$O/intcap.rpt"            # INTCAP capitalisation report
export DD_CDMATRPT="$O/cdmatur.rpt"          # CDMATUR maturity notification report
export DD_FEERPT="$O/feeasses.rpt"           # FEEASSES fee assessment report
export DD_STMTOUT="$O/statements.txt"        # STMTGEN print-image statements (132)
export DD_STMTRPT="$O/stmtgen.rpt"           # STMTGEN control report
export DD_TAX1099="$O/irs1099int.dat"        # INT1099 IRS-style 1099-INT extract
export DD_TAXRPT="$O/int1099.rpt"            # INT1099 control / exception report
