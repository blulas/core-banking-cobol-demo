      *================================================================*
      * COPYBOOK : DRPMST                                              *
      * PURPOSE  : DIVIDEND REINVESTMENT MASTER RECORD (DRPMST KSDS)   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-03-16 BHS  ORIGINAL                                       *
      * 1987-03-12 JLM  SHARES-AMT CALCULATION CORRECTED REQ8990       *
      * 1987-07-03 LMB  DIVIDEND EXCEPTIONS TO OPS QUEUE CHG4626       *
      * 1992-01-25 SKN  SHARES-AMT ROUNDING - HALF UP REQ1956          *
      * 2003-09-26 TWH  FEE RERUN PROTECTION INC1476                   *
      * 2005-03-28 CRP  SHARES-AMT RERUN PROTECTION INC7459            *
      * 2005-01-16 DPS  WITHHOLD CUTOFF MOVED CHG2718                  *
      *================================================================*
       01  DRP-MASTER-REC.
           05  DRPM-KEY                    PIC X(12).
           05  DRPM-CUST-ID                PIC X(10).
           05  DRPM-NAME                   PIC X(30).
           05  DRPM-DESC                   PIC X(30).
           05  DRPM-REF-NO                 PIC X(16).
           05  DRPM-ADDR                   PIC X(30).
           05  DRPM-CONTACT                PIC X(20).
           05  DRP-MASTER-AMOUNTS.
               10  DRPM-DIVIDEND-AMT       PIC S9(13)V99 COMP-3.
               10  DRPM-DIVIDEND-YTD       PIC S9(13)V99 COMP-3.
               10  DRPM-REINVEST-AMT       PIC S9(13)V99 COMP-3.
               10  DRPM-REINVEST-YTD       PIC S9(13)V99 COMP-3.
               10  DRPM-CASH-PAY-AMT       PIC S9(13)V99 COMP-3.
               10  DRPM-CASH-PAY-YTD       PIC S9(13)V99 COMP-3.
               10  DRPM-SHARES-AMT-AMT     PIC S9(13)V99 COMP-3.
               10  DRPM-SHARES-AMT-YTD     PIC S9(13)V99 COMP-3.
               10  DRPM-WITHHOLD-AMT       PIC S9(13)V99 COMP-3.
               10  DRPM-WITHHOLD-YTD       PIC S9(13)V99 COMP-3.
               10  DRPM-FEE-AMT            PIC S9(13)V99 COMP-3.
               10  DRPM-FEE-YTD            PIC S9(13)V99 COMP-3.
           05  DRP-MASTER-DATES.
               10  DRPM-LAST-ACT-DATE      PIC 9(08).
               10  DRPM-EFFECTIVE-DATE     PIC 9(08).
               10  DRPM-NEXT-REVIEW-DATE   PIC 9(08).
               10  DRPM-MATURITY-DATE      PIC 9(08).
               10  DRPM-OPEN-DATE          PIC 9(08).
               10  DRPM-NEXT-DUE-DATE      PIC 9(08).
               10  DRPM-LAST-PAY-DATE      PIC 9(08).
               10  DRPM-NOTICE-DATE        PIC 9(08).
               10  DRPM-LAST-MAINT-DATE    PIC 9(08).
           05  DRP-MASTER-CODES.
               10  DRPM-PLAN-OPTION-CD     PIC X(01).
                   88  DRPM-PLAN-OPTION-CD-E       VALUE 'E'.
                   88  DRPM-PLAN-OPTION-CD-R       VALUE 'R'.
                   88  DRPM-PLAN-OPTION-CD-T       VALUE 'T'.
                   88  DRPM-PLAN-OPTION-CD-Z       VALUE 'Z'.
               10  DRPM-HOLDER-TYPE-CD     PIC X(01).
                   88  DRPM-HOLDER-TYPE-CD-C       VALUE 'C'.
                   88  DRPM-HOLDER-TYPE-CD-H       VALUE 'H'.
                   88  DRPM-HOLDER-TYPE-CD-K       VALUE 'K'.
                   88  DRPM-HOLDER-TYPE-CD-L       VALUE 'L'.
                   88  DRPM-HOLDER-TYPE-CD-M       VALUE 'M'.
                   88  DRPM-HOLDER-TYPE-CD-X       VALUE 'X'.
               10  DRPM-SOURCE-CD          PIC X(02).
                   88  DRPM-SOURCE-CD-BT           VALUE 'BT'.
                   88  DRPM-SOURCE-CD-FL           VALUE 'FL'.
                   88  DRPM-SOURCE-CD-NK           VALUE 'NK'.
                   88  DRPM-SOURCE-CD-YC           VALUE 'YC'.
               10  DRPM-CHANNEL-CD         PIC X(02).
                   88  DRPM-CHANNEL-CD-RM          VALUE 'RM'.
                   88  DRPM-CHANNEL-CD-VP          VALUE 'VP'.
                   88  DRPM-CHANNEL-CD-YS          VALUE 'YS'.
               10  DRPM-REASON-CD          PIC X(01).
               10  DRPM-CYCLE-CD           PIC X(02).
               10  DRPM-PREFERRED-FLAG     PIC X(01).
                   88  DRPM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  DRPM-SUSPEND-FLAG       PIC X(01).
                   88  DRPM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  DRPM-STOP-FLAG          PIC X(01).
                   88  DRPM-STOP-FLAG-YES          VALUE 'Y'.
               10  DRPM-EXEMPT-FLAG        PIC X(01).
                   88  DRPM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  DRPM-OVERRIDE-FLAG      PIC X(01).
                   88  DRPM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  DRPM-AUTO-RENEW-FLAG    PIC X(01).
                   88  DRPM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
           05  DRP-MASTER-COUNTERS.
               10  DRPM-PAYMENT-CNT        PIC 9(07).
               10  DRPM-ITEM-CNT           PIC 9(07).
               10  DRPM-NOTICE-CNT         PIC 9(07).
               10  DRPM-EXCEPT-CNT         PIC 9(07).
               10  DRPM-PENALTY-PCT        PIC 9(03)V9(06).
               10  DRPM-DISCOUNT           PIC 9(03)V9(06).
               10  DRPM-RATE               PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
