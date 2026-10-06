      *================================================================*
      * COPYBOOK : XCLV1                                               *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS HISTORY RECORD LAYOUT        *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-12-08 RJT  ORIGINAL                                       *
      * 1998-09-07 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-05-01 DPS  BONUS RERUN PROTECTION INC7355                 *
      * 2000-08-11 TWH  MISSED CALCULATION CORRECTED REQ0355           *
      * 2002-06-17 TWH  NEW BONUS REPORT COLUMNS PRB4367               *
      * 2003-04-03 CRP  MISSED FEE SCHEDULE UPDATE CHG7250             *
      *================================================================*
       01  XCL-H1-REC.
           05  XCLH1-ADDR                  PIC X(30).
           05  XCLH1-CUST-ID               PIC X(10).
           05  XCLH1-REF-NO                PIC X(16).
           05  XCLH1-NAME                  PIC X(30).
           05  XCLH1-CONTACT               PIC X(20).
           05  XCLH1-DESC                  PIC X(30).
           05  XCL-H1-AMOUNTS.
               10  XCLH1-PAYOUT-YTD        PIC S9(13)V99 COMP-3.
               10  XCLH1-MISSED-AMT        PIC S9(13)V99 COMP-3.
               10  XCLH1-GOAL-YTD          PIC S9(13)V99 COMP-3.
               10  XCLH1-PAYOUT-AMT        PIC S9(13)V99 COMP-3.
               10  XCLH1-CONTRIB-AMT       PIC S9(13)V99 COMP-3.
               10  XCLH1-ACCRUED-AMT       PIC S9(13)V99 COMP-3.
               10  XCLH1-BONUS-YTD         PIC S9(13)V99 COMP-3.
               10  XCLH1-WEEKLY-AMT        PIC S9(13)V99 COMP-3.
               10  XCLH1-ACCRUED-YTD       PIC S9(13)V99 COMP-3.
               10  XCLH1-CONTRIB-YTD       PIC S9(13)V99 COMP-3.
               10  XCLH1-MISSED-YTD        PIC S9(13)V99 COMP-3.
               10  XCLH1-PENALTY-YTD       PIC S9(13)V99 COMP-3.
               10  XCLH1-WEEKLY-YTD        PIC S9(13)V99 COMP-3.
               10  XCLH1-GOAL-AMT          PIC S9(13)V99 COMP-3.
               10  XCLH1-BONUS-AMT         PIC S9(13)V99 COMP-3.
           05  XCL-H1-DATES.
               10  XCLH1-OPEN-DATE         PIC 9(08).
               10  XCLH1-LAST-STMT-DATE    PIC 9(08).
               10  XCLH1-EXPIRY-DATE       PIC 9(08).
               10  XCLH1-NOTICE-DATE       PIC 9(08).
               10  XCLH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  XCLH1-NEXT-DUE-DATE     PIC 9(08).
               10  XCLH1-MATURITY-DATE     PIC 9(08).
               10  XCLH1-LAST-PAY-DATE     PIC 9(08).
           05  XCL-H1-CODES.
               10  XCLH1-PAYOUT-METHOD-CD  PIC X(01).
                   88  XCLH1-PAYOUT-METHOD-CD-B    VALUE 'B'.
                   88  XCLH1-PAYOUT-METHOD-CD-H    VALUE 'H'.
                   88  XCLH1-PAYOUT-METHOD-CD-U    VALUE 'U'.
                   88  XCLH1-PAYOUT-METHOD-CD-W    VALUE 'W'.
               10  XCLH1-PREFERRED-FLAG    PIC X(01).
                   88  XCLH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  XCLH1-SUSPEND-FLAG      PIC X(01).
                   88  XCLH1-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  XCLH1-OVERRIDE-FLAG     PIC X(01).
                   88  XCLH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  XCLH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  XCLH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  XCLH1-REASON-CD         PIC X(02).
               10  XCLH1-CYCLE-CD          PIC X(02).
               10  XCLH1-STOP-FLAG         PIC X(01).
                   88  XCLH1-STOP-FLAG-YES         VALUE 'Y'.
               10  XCLH1-REGION-CD         PIC X(03).
                   88  XCLH1-REGION-CD-FLM         VALUE 'FLM'.
                   88  XCLH1-REGION-CD-HLS         VALUE 'HLS'.
                   88  XCLH1-REGION-CD-KMJ         VALUE 'KMJ'.
                   88  XCLH1-REGION-CD-PDA         VALUE 'PDA'.
                   88  XCLH1-REGION-CD-RVG         VALUE 'RVG'.
                   88  XCLH1-REGION-CD-WMB         VALUE 'WMB'.
           05  XCL-H1-COUNTERS.
               10  XCLH1-RETRY-CNT         PIC 9(07).
               10  XCLH1-RATE              PIC 9(03)V9(06).
               10  XCLH1-DISCOUNT          PIC 9(03)V9(06).
               10  XCLH1-PAYMENT-CNT       PIC 9(07).
               10  XCLH1-NOTICE-CNT        PIC 9(07).
               10  XCLH1-FEE-PCT           PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
