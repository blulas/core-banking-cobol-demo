      *================================================================*
      * COPYBOOK : DRPARC1                                             *
      * PURPOSE  : DIVIDEND REINVESTMENT HISTORY RECORD LAYOUT         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-02-22 CRP  ORIGINAL                                       *
      * 1991-09-27 TWH  PERFORMANCE - WITHHOLD READ AHEAD REQ2639      *
      * 1992-06-20 BHS  DIVIDEND CUTOFF MOVED INC5265                  *
      * 1998-09-03 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-01-03 SKN  NEW REINVEST REPORT COLUMNS REQ0228            *
      * 1999-08-14 DPS  DIVIDEND TABLE EXPANDED CHG5278                *
      * 2000-06-20 LMB  CASH-PAY STATUS CODES ADDED PRB7836            *
      * 2002-09-05 CRP  NEW FEE REPORT COLUMNS CHG9072                 *
      * 2004-06-14 GAF  WITHHOLD CALCULATION CORRECTED PRB0348         *
      * 2007-06-16 MKL  REINVEST ROUNDING - HALF UP PRB6271            *
      * 2009-03-26 BHS  WITHHOLD LIMIT REVISED PRB9819                 *
      * 2010-03-17 DPS  SHARES-AMT ROUNDING - HALF UP INC2092          *
      * 2010-08-08 MKL  PERFORMANCE - WITHHOLD READ AHEAD CHG4581      *
      *================================================================*
       01  DRP-H1-REC.
           05  DRPH1-ADDR                  PIC X(30).
           05  DRPH1-CONTACT               PIC X(20).
           05  DRPH1-DESC                  PIC X(30).
           05  DRPH1-REF-NO                PIC X(16).
           05  DRPH1-CUST-ID               PIC X(10).
           05  DRPH1-NAME                  PIC X(30).
           05  DRPH1-KEY                   PIC X(12).
           05  DRP-H1-AMOUNTS.
               10  DRPH1-DIVIDEND-YTD      PIC S9(13)V99 COMP-3.
               10  DRPH1-WITHHOLD-AMT      PIC S9(13)V99 COMP-3.
               10  DRPH1-SHARES-AMT-YTD    PIC S9(13)V99 COMP-3.
               10  DRPH1-DIVIDEND-AMT      PIC S9(13)V99 COMP-3.
               10  DRPH1-REINVEST-AMT      PIC S9(13)V99 COMP-3.
               10  DRPH1-SHARES-AMT-AMT    PIC S9(13)V99 COMP-3.
               10  DRPH1-CASH-PAY-AMT      PIC S9(13)V99 COMP-3.
               10  DRPH1-FEE-AMT           PIC S9(13)V99 COMP-3.
               10  DRPH1-WITHHOLD-YTD      PIC S9(13)V99 COMP-3.
           05  DRP-H1-DATES.
               10  DRPH1-EFFECTIVE-DATE    PIC 9(08).
               10  DRPH1-OPEN-DATE         PIC 9(08).
               10  DRPH1-LAST-ACT-DATE     PIC 9(08).
               10  DRPH1-NEXT-DUE-DATE     PIC 9(08).
               10  DRPH1-NOTICE-DATE       PIC 9(08).
               10  DRPH1-LAST-MAINT-DATE   PIC 9(08).
               10  DRPH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  DRPH1-MATURITY-DATE     PIC 9(08).
               10  DRPH1-LAST-PAY-DATE     PIC 9(08).
           05  DRP-H1-CODES.
               10  DRPH1-CHANNEL-CD        PIC X(02).
                   88  DRPH1-CHANNEL-CD-RM         VALUE 'RM'.
                   88  DRPH1-CHANNEL-CD-VP         VALUE 'VP'.
                   88  DRPH1-CHANNEL-CD-YS         VALUE 'YS'.
               10  DRPH1-PLAN-OPTION-CD    PIC X(01).
                   88  DRPH1-PLAN-OPTION-CD-E      VALUE 'E'.
                   88  DRPH1-PLAN-OPTION-CD-R      VALUE 'R'.
                   88  DRPH1-PLAN-OPTION-CD-T      VALUE 'T'.
                   88  DRPH1-PLAN-OPTION-CD-Z      VALUE 'Z'.
               10  DRPH1-HOLDER-TYPE-CD    PIC X(01).
                   88  DRPH1-HOLDER-TYPE-CD-C      VALUE 'C'.
                   88  DRPH1-HOLDER-TYPE-CD-H      VALUE 'H'.
                   88  DRPH1-HOLDER-TYPE-CD-K      VALUE 'K'.
                   88  DRPH1-HOLDER-TYPE-CD-L      VALUE 'L'.
                   88  DRPH1-HOLDER-TYPE-CD-M      VALUE 'M'.
                   88  DRPH1-HOLDER-TYPE-CD-X      VALUE 'X'.
               10  DRPH1-EXEMPT-FLAG       PIC X(01).
                   88  DRPH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  DRPH1-CYCLE-CD          PIC X(02).
               10  DRPH1-SUSPEND-FLAG      PIC X(01).
                   88  DRPH1-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  DRPH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  DRPH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  DRPH1-SOURCE-CD         PIC X(02).
                   88  DRPH1-SOURCE-CD-BT          VALUE 'BT'.
                   88  DRPH1-SOURCE-CD-FL          VALUE 'FL'.
                   88  DRPH1-SOURCE-CD-NK          VALUE 'NK'.
                   88  DRPH1-SOURCE-CD-YC          VALUE 'YC'.
               10  DRPH1-PREFERRED-FLAG    PIC X(01).
                   88  DRPH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  DRPH1-STOP-FLAG         PIC X(01).
                   88  DRPH1-STOP-FLAG-YES         VALUE 'Y'.
               10  DRPH1-REASON-CD         PIC X(01).
           05  DRP-H1-COUNTERS.
               10  DRPH1-DISCOUNT          PIC 9(03)V9(06).
               10  DRPH1-PAYMENT-CNT       PIC 9(07).
               10  DRPH1-RATE              PIC 9(03)V9(06).
               10  DRPH1-EXCEPT-CNT        PIC 9(07).
               10  DRPH1-ITEM-CNT          PIC 9(07).
               10  DRPH1-NOTICE-CNT        PIC 9(07).
               10  DRPH1-PENALTY-PCT       PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
