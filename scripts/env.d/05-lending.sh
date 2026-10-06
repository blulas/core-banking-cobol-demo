# 05-lending - loan master, seeds and loan batch outputs
export DD_LOANMST="$M/loanmst.idx"           # KSDS + AIX on borrower cust id
export DD_LOANSEED="$S/loans.psv"
export DD_LDUESEED="$S/loandue.psv"
export DD_LNBILLS="$O/lnbills.dat"           # LNBILL billing records
export DD_LNAUTOPY="$O/lnautopy.dat"         # LNBILL autopay WLNP debits (TRANREC) for NEXT cycle's TRANIN
export DD_LNPAYHST="$O/lnpayhst.dat"         # LNPAYAP payment history (DB2 LOAD -> CBS.LOAN_PAYMENT_HIST)
export DD_LNACRPT="$O/lnacrpt.txt"           # LNACCR report
export DD_LNBLRPT="$O/lnblrpt.txt"           # LNBILL report
export DD_LNPYRPT="$O/lnpyrpt.txt"           # LNPAYAP report
export DD_LNDQRPT="$O/lndqrpt.txt"           # LNDELQ report
