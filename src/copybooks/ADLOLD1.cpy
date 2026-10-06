      *================================================================*
      * COPYBOOK : ADLOLD1                                             *
      * PURPOSE  : DEALER AUTO FINANCE PRIOR VERSION RECORD LAYOUT     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-07-10 JLM  ORIGINAL                                       *
      * 1997-11-20 CRP  RESERVE CALCULATION CORRECTED PRB6377          *
      * 1998-11-26 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-01-24 SKN  GAP FEE SCHEDULE UPDATE CHG1628                *
      * 1999-02-01 GAF  REBATE EXCEPTIONS TO OPS QUEUE PRB5503         *
      * 2000-12-11 PQR  GAP ROUNDING - HALF UP CHG5785                 *
      * 2000-03-28 TWH  PERFORMANCE - REBATE READ AHEAD INC8596        *
      *================================================================*
       01  ADL-H1-REC.
           05  ADLH1-KEY                   PIC X(12).
           05  ADLH1-CUST-ID               PIC X(10).
           05  ADLH1-NAME                  PIC X(30).
           05  ADLH1-DESC                  PIC X(30).
           05  ADLH1-REF-NO                PIC X(16).
           05  ADLH1-ADDR                  PIC X(30).
           05  ADL-H1-AMOUNTS.
               10  ADLH1-RESERVE-YTD       PIC S9(13)V99 COMP-3.
               10  ADLH1-CHARGEBK-AMT      PIC S9(13)V99 COMP-3.
               10  ADLH1-GAP-YTD           PIC S9(13)V99 COMP-3.
               10  ADLH1-AMOUNT-FIN-YTD    PIC S9(13)V99 COMP-3.
               10  ADLH1-REBATE-YTD        PIC S9(13)V99 COMP-3.
               10  ADLH1-CHARGEBK-YTD      PIC S9(13)V99 COMP-3.
               10  ADLH1-RESERVE-AMT       PIC S9(13)V99 COMP-3.
               10  ADLH1-PAYMENT-AMT       PIC S9(13)V99 COMP-3.
               10  ADLH1-PAYMENT-YTD       PIC S9(13)V99 COMP-3.
               10  ADLH1-REBATE-AMT        PIC S9(13)V99 COMP-3.
               10  ADLH1-AMOUNT-FIN-AMT    PIC S9(13)V99 COMP-3.
               10  ADLH1-GAP-AMT           PIC S9(13)V99 COMP-3.
           05  ADL-H1-DATES.
               10  ADLH1-LAST-MAINT-DATE   PIC 9(08).
               10  ADLH1-NEXT-DUE-DATE     PIC 9(08).
               10  ADLH1-NOTICE-DATE       PIC 9(08).
               10  ADLH1-EXPIRY-DATE       PIC 9(08).
               10  ADLH1-EFFECTIVE-DATE    PIC 9(08).
               10  ADLH1-LAST-STMT-DATE    PIC 9(08).
               10  ADLH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  ADLH1-LAST-ACT-DATE     PIC 9(08).
               10  ADLH1-LAST-PAY-DATE     PIC 9(08).
           05  ADL-H1-CODES.
               10  ADLH1-TIER-CD           PIC X(02).
                   88  ADLH1-TIER-CD-EW            VALUE 'EW'.
                   88  ADLH1-TIER-CD-JF            VALUE 'JF'.
                   88  ADLH1-TIER-CD-KD            VALUE 'KD'.
                   88  ADLH1-TIER-CD-UG            VALUE 'UG'.
                   88  ADLH1-TIER-CD-WY            VALUE 'WY'.
               10  ADLH1-EXEMPT-FLAG       PIC X(01).
                   88  ADLH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  ADLH1-DEALER-CD         PIC X(03).
                   88  ADLH1-DEALER-CD-DRU         VALUE 'DRU'.
                   88  ADLH1-DEALER-CD-FNF         VALUE 'FNF'.
                   88  ADLH1-DEALER-CD-GBX         VALUE 'GBX'.
                   88  ADLH1-DEALER-CD-GTU         VALUE 'GTU'.
                   88  ADLH1-DEALER-CD-JNE         VALUE 'JNE'.
                   88  ADLH1-DEALER-CD-KTP         VALUE 'KTP'.
               10  ADLH1-STATUS-CD         PIC X(03).
                   88  ADLH1-STATUS-CD-EUA         VALUE 'EUA'.
                   88  ADLH1-STATUS-CD-HMW         VALUE 'HMW'.
                   88  ADLH1-STATUS-CD-RCB         VALUE 'RCB'.
               10  ADLH1-OVERRIDE-FLAG     PIC X(01).
                   88  ADLH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  ADLH1-PREFERRED-FLAG    PIC X(01).
                   88  ADLH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  ADLH1-CYCLE-CD          PIC X(01).
                   88  ADLH1-CYCLE-CD-A            VALUE 'A'.
                   88  ADLH1-CYCLE-CD-H            VALUE 'H'.
                   88  ADLH1-CYCLE-CD-J            VALUE 'J'.
                   88  ADLH1-CYCLE-CD-T            VALUE 'T'.
                   88  ADLH1-CYCLE-CD-W            VALUE 'W'.
               10  ADLH1-REASON-CD         PIC X(03).
                   88  ADLH1-REASON-CD-FZV         VALUE 'FZV'.
                   88  ADLH1-REASON-CD-KPP         VALUE 'KPP'.
                   88  ADLH1-REASON-CD-MVN         VALUE 'MVN'.
                   88  ADLH1-REASON-CD-UVY         VALUE 'UVY'.
                   88  ADLH1-REASON-CD-ZWZ         VALUE 'ZWZ'.
               10  ADLH1-STOP-FLAG         PIC X(01).
                   88  ADLH1-STOP-FLAG-YES         VALUE 'Y'.
               10  ADLH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  ADLH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  ADLH1-REVIEW-FLAG       PIC X(01).
                   88  ADLH1-REVIEW-FLAG-YES       VALUE 'Y'.
           05  ADL-H1-COUNTERS.
               10  ADLH1-PAYMENT-CNT       PIC 9(07).
               10  ADLH1-CYCLE-CNT         PIC 9(07).
               10  ADLH1-FEE-PCT           PIC 9(03)V9(06).
               10  ADLH1-EXCEPT-CNT        PIC 9(07).
               10  ADLH1-MARGIN            PIC 9(03)V9(06).
               10  ADLH1-RATE              PIC 9(03)V9(06).
               10  ADLH1-RETRY-CNT         PIC 9(07).
           05  FILLER                      PIC X(20).
