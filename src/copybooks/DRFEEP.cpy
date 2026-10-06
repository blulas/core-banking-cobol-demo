      *================================================================*
      * COPYBOOK : DRFEEP                                              *
      * PURPOSE  : LINKAGE FOR DIVIDEND REINVESTMENT RULE MODULE DRFEE *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-07-11 SKN  ORIGINAL                                       *
      * 1988-10-12 CRP  REINVEST TABLE EXPANDED PRB1244                *
      * 1993-07-26 SKN  REINVEST CALCULATION CORRECTED REQ7475         *
      * 2000-05-27 CRP  FEE LIMIT REVISED INC5408                      *
      * 2000-09-15 RJT  WITHHOLD RERUN PROTECTION REQ1906              *
      * 2001-01-09 DPS  AUDIT FINDING - FEE CONTROL TOTALS INC6759     *
      * 2005-04-14 SKN  WITHHOLD CUTOFF MOVED REQ5840                  *
      * 2006-05-10 LMB  AUDIT FINDING - WITHHOLD CONTROL TOTALS PRB8613*
      *================================================================*
       01  DRFEE-PARMS.
           05  DRFEE-CASH-PAY-YTD          PIC S9(13)V99 COMP-3.
           05  DRFEE-WITHHOLD-YTD          PIC S9(13)V99 COMP-3.
           05  DRFEE-SHARES-AMT-AMT        PIC S9(13)V99 COMP-3.
           05  DRFEE-FEE-AMT               PIC S9(13)V99 COMP-3.
           05  DRFEE-REINVEST-YTD          PIC S9(13)V99 COMP-3.
           05  DRFEE-SHARES-AMT-YTD        PIC S9(13)V99 COMP-3.
           05  DRFEE-NOTICE-DATE           PIC 9(08).
           05  DRFEE-LAST-PAY-DATE         PIC 9(08).
           05  DRFEE-NEXT-REVIEW-DATE      PIC 9(08).
           05  DRFEE-OPEN-DATE             PIC 9(08).
           05  DRFEE-REASON-CD             PIC X(01).
           05  DRFEE-CYCLE-CD              PIC X(02).
           05  DRFEE-PLAN-OPTION-CD        PIC X(01).
               88  DRFEE-PLAN-OPTION-CD-E          VALUE 'E'.
               88  DRFEE-PLAN-OPTION-CD-R          VALUE 'R'.
               88  DRFEE-PLAN-OPTION-CD-T          VALUE 'T'.
               88  DRFEE-PLAN-OPTION-CD-Z          VALUE 'Z'.
           05  DRFEE-HOLDER-TYPE-CD        PIC X(01).
               88  DRFEE-HOLDER-TYPE-CD-C          VALUE 'C'.
               88  DRFEE-HOLDER-TYPE-CD-H          VALUE 'H'.
               88  DRFEE-HOLDER-TYPE-CD-K          VALUE 'K'.
               88  DRFEE-HOLDER-TYPE-CD-L          VALUE 'L'.
               88  DRFEE-HOLDER-TYPE-CD-M          VALUE 'M'.
               88  DRFEE-HOLDER-TYPE-CD-X          VALUE 'X'.
           05  DRFEE-OVERRIDE-FLAG         PIC X(01).
               88  DRFEE-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  DRFEE-SUSPEND-FLAG          PIC X(01).
               88  DRFEE-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  DRFEE-PREFERRED-FLAG        PIC X(01).
               88  DRFEE-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  DRFEE-PAYMENT-CNT           PIC 9(07).
           05  DRFEE-NOTICE-CNT            PIC 9(07).
           05  DRFEE-DISCOUNT              PIC 9(03)V9(06).
           05  DRFEE-RATE                  PIC 9(03)V9(06).
           05  DRFEE-DESC                  PIC X(30).
           05  DRFEE-ADDR                  PIC X(30).
           05  DRFEE-RETURN-CODE           PIC 9(02).
