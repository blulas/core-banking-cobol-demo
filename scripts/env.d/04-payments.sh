# 04-payments - payment instructions, ACH (NACHA) and Fedwire datasets
export DD_PAYMST="$M/paymst.idx"            # payment instruction master (KSDS)
export DD_PAYSEED="$S/payments.psv"         # LDPAYM input
export DD_SDNLIST="$S/sdnlist.txt"          # sanctions list (OFACSCRN callers)
export DD_ACHINBND="$I/achinbnd.ach"        # inbound NACHA file from the Fed
export DD_ACHOUTB="$O/achoutb.ach"          # originated NACHA file (PAYPROC)
export DD_ACHRTN="$O/achrtn.ach"            # return entry staging (ACHIN, ACHRET EXTEND)
export DD_ACHRTNF="$O/achrtnf.ach"          # formatted NACHA return file (ACHRET)
export DD_FEDWOUT="$O/fedwout.txt"          # outbound Fedwire messages (PAYPROC)
export DD_ACHINRPT="$O/achinrpt.txt"        # ACHIN control report
export DD_PAYRPT="$O/payrpt.txt"            # PAYPROC release report
export DD_ACHRRPT="$O/achretrpt.txt"        # ACHRET returns report
