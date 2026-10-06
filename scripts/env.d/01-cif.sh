#------------------------------------------------------------------------------
# 01-cif.sh - Customer Information File (CIF) / KYC / account lifecycle DDs
#   KYCREVW  month-end KYC review report
#   DORMSCAN month-end dormancy / escheat report
#   RELBAL   relationship balance report
#------------------------------------------------------------------------------
export DD_KYCRPT="$O/kycrevw.rpt"
export DD_DORMRPT="$O/dormscan.rpt"
export DD_RELBRPT="$O/relbal.rpt"
