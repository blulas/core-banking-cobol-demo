      *================================================================*
      * COPYBOOK : DRPEXT2                                             *
      * PURPOSE  : DIVIDEND REINVESTMENT EXTRACT RECORD LAYOUT         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-02-23 RJT  ORIGINAL                                       *
      * 1994-01-01 SKN  SHARES-AMT ROUNDING - HALF UP REQ2744          *
      * 1994-03-16 BHS  SHARES-AMT RERUN PROTECTION PRB6533            *
      * 1995-07-04 SKN  NEW DIVIDEND REPORT COLUMNS CHG4968            *
      * 1997-05-03 GAF  FEE RERUN PROTECTION INC1865                   *
      * 2003-04-24 MKL  SHARES-AMT NOTICE TEXT PER LEGAL PRB2075       *
      * 2005-07-04 EWS  WITHHOLD TABLE EXPANDED CHG4204                *
      * 2007-12-26 GAF  DIVIDEND LIMIT REVISED REQ8079                 *
      * 2007-03-11 JLM  FEE ROUNDING - HALF UP PRB6651                 *
      * 2008-12-03 GAF  REINVEST NOTICE TEXT PER LEGAL CHG1320         *
      * 2009-08-05 CRP  SHARES-AMT FEE SCHEDULE UPDATE REQ3077         *
      *================================================================*
       01  DRP-H2-REC.
           05  DRPH2-NAME                  PIC X(30).
           05  DRPH2-ADDR                  PIC X(30).
           05  DRPH2-CUST-ID               PIC X(10).
           05  DRPH2-KEY                   PIC X(12).
           05  DRPH2-DESC                  PIC X(30).
           05  DRPH2-REF-NO                PIC X(16).
           05  DRPH2-CONTACT               PIC X(20).
           05  DRP-H2-AMOUNTS.
               10  DRPH2-FEE-AMT           PIC S9(13)V99 COMP-3.
               10  DRPH2-CASH-PAY-YTD      PIC S9(13)V99 COMP-3.
               10  DRPH2-WITHHOLD-AMT      PIC S9(13)V99 COMP-3.
               10  DRPH2-SHARES-AMT-AMT    PIC S9(13)V99 COMP-3.
               10  DRPH2-SHARES-AMT-YTD    PIC S9(13)V99 COMP-3.
               10  DRPH2-WITHHOLD-YTD      PIC S9(13)V99 COMP-3.
               10  DRPH2-REINVEST-AMT      PIC S9(13)V99 COMP-3.
               10  DRPH2-DIVIDEND-AMT      PIC S9(13)V99 COMP-3.
               10  DRPH2-FEE-YTD           PIC S9(13)V99 COMP-3.
           05  DRP-H2-DATES.
               10  DRPH2-EFFECTIVE-DATE    PIC 9(08).
               10  DRPH2-NEXT-REVIEW-DATE  PIC 9(08).
               10  DRPH2-LAST-MAINT-DATE   PIC 9(08).
               10  DRPH2-NOTICE-DATE       PIC 9(08).
               10  DRPH2-NEXT-DUE-DATE     PIC 9(08).
               10  DRPH2-LAST-ACT-DATE     PIC 9(08).
               10  DRPH2-OPEN-DATE         PIC 9(08).
               10  DRPH2-MATURITY-DATE     PIC 9(08).
               10  DRPH2-LAST-PAY-DATE     PIC 9(08).
           05  DRP-H2-CODES.
               10  DRPH2-OVERRIDE-FLAG     PIC X(01).
                   88  DRPH2-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  DRPH2-HOLDER-TYPE-CD    PIC X(01).
                   88  DRPH2-HOLDER-TYPE-CD-C      VALUE 'C'.
                   88  DRPH2-HOLDER-TYPE-CD-H      VALUE 'H'.
                   88  DRPH2-HOLDER-TYPE-CD-K      VALUE 'K'.
                   88  DRPH2-HOLDER-TYPE-CD-L      VALUE 'L'.
                   88  DRPH2-HOLDER-TYPE-CD-M      VALUE 'M'.
                   88  DRPH2-HOLDER-TYPE-CD-X      VALUE 'X'.
               10  DRPH2-SUSPEND-FLAG      PIC X(01).
                   88  DRPH2-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  DRPH2-REASON-CD         PIC X(01).
               10  DRPH2-CHANNEL-CD        PIC X(02).
                   88  DRPH2-CHANNEL-CD-RM         VALUE 'RM'.
                   88  DRPH2-CHANNEL-CD-VP         VALUE 'VP'.
                   88  DRPH2-CHANNEL-CD-YS         VALUE 'YS'.
               10  DRPH2-CYCLE-CD          PIC X(02).
               10  DRPH2-SOURCE-CD         PIC X(02).
                   88  DRPH2-SOURCE-CD-BT          VALUE 'BT'.
                   88  DRPH2-SOURCE-CD-FL          VALUE 'FL'.
                   88  DRPH2-SOURCE-CD-NK          VALUE 'NK'.
                   88  DRPH2-SOURCE-CD-YC          VALUE 'YC'.
               10  DRPH2-STOP-FLAG         PIC X(01).
                   88  DRPH2-STOP-FLAG-YES         VALUE 'Y'.
               10  DRPH2-PREFERRED-FLAG    PIC X(01).
                   88  DRPH2-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  DRPH2-AUTO-RENEW-FLAG   PIC X(01).
                   88  DRPH2-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  DRPH2-EXEMPT-FLAG       PIC X(01).
                   88  DRPH2-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  DRPH2-PLAN-OPTION-CD    PIC X(01).
                   88  DRPH2-PLAN-OPTION-CD-E      VALUE 'E'.
                   88  DRPH2-PLAN-OPTION-CD-R      VALUE 'R'.
                   88  DRPH2-PLAN-OPTION-CD-T      VALUE 'T'.
                   88  DRPH2-PLAN-OPTION-CD-Z      VALUE 'Z'.
           05  DRP-H2-COUNTERS.
               10  DRPH2-PENALTY-PCT       PIC 9(03)V9(06).
               10  DRPH2-EXCEPT-CNT        PIC 9(07).
               10  DRPH2-RATE              PIC 9(03)V9(06).
               10  DRPH2-NOTICE-CNT        PIC 9(07).
               10  DRPH2-DISCOUNT          PIC 9(03)V9(06).
               10  DRPH2-PAYMENT-CNT       PIC 9(07).
           05  FILLER                      PIC X(20).
