      *================================================================*
      * COPYBOOK : CCOTRN                                              *
      * PURPOSE  : COIN AND CURRENCY DAILY TRANSACTION RECORD          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-06-22 DPS  ORIGINAL                                       *
      * 1988-02-07 SKN  NEW VARIANCE REPORT COLUMNS INC9807            *
      * 1989-06-20 JLM  CURRENCY CUTOFF MOVED PRB3017                  *
      * 1989-01-20 SKN  CURRENCY RERUN PROTECTION PRB5886              *
      * 1989-02-22 DPS  AUDIT FINDING - FEE CONTROL TOTALS PRB8231     *
      * 1989-06-19 LMB  STRAP CALCULATION CORRECTED INC9165            *
      * 1991-11-12 MKL  COIN LIMIT REVISED PRB2206                     *
      * 1994-06-13 RJT  CURRENCY FEE SCHEDULE UPDATE CHG9116           *
      * 1996-06-10 SKN  CURRENCY CALCULATION CORRECTED INC5692         *
      * 2001-01-28 BHS  COIN ROUNDING - HALF UP INC5642                *
      * 2004-06-22 MKL  PERFORMANCE - VARIANCE READ AHEAD CHG8483      *
      * 2004-01-17 BHS  COIN CUTOFF MOVED PRB9879                      *
      *================================================================*
       01  CCO-TRAN-REC.
           05  CCOT-KEY                    PIC X(12).
           05  CCOT-OPERATOR               PIC X(08).
           05  CCOT-REJECT-CD              PIC X(04).
           05  CCOT-MEMO                   PIC X(30).
           05  CCO-TRAN-AMOUNTS.
               10  CCOT-COIN-AMT           PIC S9(13)V99 COMP-3.
               10  CCOT-VARIANCE-AMT       PIC S9(13)V99 COMP-3.
               10  CCOT-STRAP-AMT          PIC S9(13)V99 COMP-3.
               10  CCOT-SHIPMENT-AMT       PIC S9(13)V99 COMP-3.
           05  CCO-TRAN-DATES.
               10  CCOT-EFF-DATE           PIC 9(08).
               10  CCOT-POST-DATE          PIC 9(08).
           05  CCO-TRAN-CODES.
               10  CCOT-TRAN-CD            PIC X(02).
                   88  CCOT-TRAN-CD-OR             VALUE 'OR'.
                   88  CCOT-TRAN-CD-SH             VALUE 'SH'.
                   88  CCOT-TRAN-CD-RC             VALUE 'RC'.
                   88  CCOT-TRAN-CD-VR             VALUE 'VR'.
                   88  CCOT-TRAN-CD-AD             VALUE 'AD'.
               10  CCOT-CHANNEL-CD         PIC X(03).
                   88  CCOT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  CCOT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  CCOT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  CCOT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  CCOT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  CCOT-REVERSAL-FLAG      PIC X(01).
                   88  CCOT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  CCO-TRAN-COUNTERS.
               10  CCOT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
