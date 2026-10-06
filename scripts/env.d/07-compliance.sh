#------------------------------------------------------------------------------
# 07-compliance.sh - BSA/AML compliance and end-of-day control DDs.
#   CTRSCAN   CTRFILE (FinCEN 112 CTR records), CTRRPT
#   STRUCTUR  STRCRPT
#   AMLMON    AMLRPT
#   OFACBAT   OFACRPT  (SDNLIST is owned by Payments - default only if unset)
#   EODCTL    EODRPT   (START / END cycle control report, appended)
#------------------------------------------------------------------------------
export DD_CTRFILE="$O/ctrfile.dat"           # CTR records for BSA e-filing
export DD_CTRRPT="$O/ctrscan.rpt"            # CTR determination report
export DD_STRCRPT="$O/structur.rpt"          # structuring detection report
export DD_AMLRPT="$O/amlmon.rpt"             # AML scenario monitoring report
export DD_OFACRPT="$O/ofacbat.rpt"           # OFAC batch re-screening report
export DD_EODRPT="$O/eodctl.rpt"             # EOD cycle control / summary report
export DD_SDNLIST="${DD_SDNLIST:-$S/sdnlist.txt}"
