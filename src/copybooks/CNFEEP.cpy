      *================================================================*
      * COPYBOOK : CNFEEP                                              *
      * PURPOSE  : LINKAGE FOR COIN AND CURRENCY RULE MODULE CNFEE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-07-07 RJT  ORIGINAL                                       *
      * 1991-06-15 BHS  FEE CUTOFF MOVED CHG9614                       *
      * 1996-05-28 LMB  COIN ROUNDING - HALF UP INC0474                *
      * 1996-05-01 EWS  NEW STRAP REPORT COLUMNS CHG2045               *
      * 1999-09-16 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-06-21 EWS  STRAP TABLE EXPANDED PRB4501                   *
      * 2002-09-28 PQR  COIN NOTICE TEXT PER LEGAL CHG3544             *
      *================================================================*
       01  CNFEE-PARMS.
           05  CNFEE-STRAP-YTD             PIC S9(13)V99 COMP-3.
           05  CNFEE-CURRENCY-YTD          PIC S9(13)V99 COMP-3.
           05  CNFEE-STRAP-AMT             PIC S9(13)V99 COMP-3.
           05  CNFEE-FEE-AMT               PIC S9(13)V99 COMP-3.
           05  CNFEE-VARIANCE-YTD          PIC S9(13)V99 COMP-3.
           05  CNFEE-COIN-YTD              PIC S9(13)V99 COMP-3.
           05  CNFEE-NEXT-REVIEW-DATE      PIC 9(08).
           05  CNFEE-LAST-PAY-DATE         PIC 9(08).
           05  CNFEE-NOTICE-DATE           PIC 9(08).
           05  CNFEE-EFFECTIVE-DATE        PIC 9(08).
           05  CNFEE-REASON-CD             PIC X(02).
               88  CNFEE-REASON-CD-JL              VALUE 'JL'.
               88  CNFEE-REASON-CD-LW              VALUE 'LW'.
               88  CNFEE-REASON-CD-PS              VALUE 'PS'.
           05  CNFEE-SOURCE-CD             PIC X(03).
           05  CNFEE-DENOM-CD              PIC X(01).
               88  CNFEE-DENOM-CD-B                VALUE 'B'.
               88  CNFEE-DENOM-CD-D                VALUE 'D'.
               88  CNFEE-DENOM-CD-P                VALUE 'P'.
               88  CNFEE-DENOM-CD-S                VALUE 'S'.
               88  CNFEE-DENOM-CD-W                VALUE 'W'.
           05  CNFEE-CARRIER-CD            PIC X(01).
               88  CNFEE-CARRIER-CD-J              VALUE 'J'.
               88  CNFEE-CARRIER-CD-L              VALUE 'L'.
               88  CNFEE-CARRIER-CD-M              VALUE 'M'.
               88  CNFEE-CARRIER-CD-X              VALUE 'X'.
           05  CNFEE-EXEMPT-FLAG           PIC X(01).
               88  CNFEE-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  CNFEE-AUTO-RENEW-FLAG       PIC X(01).
               88  CNFEE-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  CNFEE-HOLD-FLAG             PIC X(01).
               88  CNFEE-HOLD-FLAG-YES             VALUE 'Y'.
           05  CNFEE-CYCLE-CNT             PIC 9(07).
           05  CNFEE-NOTICE-CNT            PIC 9(07).
           05  CNFEE-RATE                  PIC 9(03)V9(06).
           05  CNFEE-FEE-PCT               PIC 9(03)V9(06).
           05  CNFEE-DESC                  PIC X(30).
           05  CNFEE-REF-NO                PIC X(16).
           05  CNFEE-RETURN-CODE           PIC 9(02).
