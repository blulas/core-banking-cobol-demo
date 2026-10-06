      *================================================================*
      * COPYBOOK : AULIMP                                              *
      * PURPOSE  : LINKAGE FOR DEALER AUTO FINANCE RULE MODULE AULIM   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-10-10 GAF  ORIGINAL                                       *
      * 1996-01-26 GAF  NEW RESERVE REPORT COLUMNS INC6984             *
      * 1999-11-10 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-01-11 SKN  GAP LIMIT REVISED CHG9275                      *
      * 2004-06-09 DPS  CHARGEBK STATUS CODES ADDED PRB6436            *
      * 2006-04-09 CRP  AMOUNT-FIN LIMIT REVISED INC5920               *
      * 2007-09-15 DPS  RESERVE CALCULATION CORRECTED CHG2664          *
      * 2008-02-14 SKN  CHARGEBK TABLE EXPANDED PRB1832                *
      * 2008-03-25 RJT  AMOUNT-FIN TABLE EXPANDED CHG8666              *
      * 2010-01-12 RJT  CHARGEBK RERUN PROTECTION REQ6032              *
      *================================================================*
       01  AULIM-PARMS.
           05  AULIM-REBATE-YTD            PIC S9(13)V99 COMP-3.
           05  AULIM-CHARGEBK-YTD          PIC S9(13)V99 COMP-3.
           05  AULIM-REBATE-AMT            PIC S9(13)V99 COMP-3.
           05  AULIM-PAYMENT-AMT           PIC S9(13)V99 COMP-3.
           05  AULIM-CHARGEBK-AMT          PIC S9(13)V99 COMP-3.
           05  AULIM-AMOUNT-FIN-YTD        PIC S9(13)V99 COMP-3.
           05  AULIM-LAST-MAINT-DATE       PIC 9(08).
           05  AULIM-LAST-STMT-DATE        PIC 9(08).
           05  AULIM-NEXT-DUE-DATE         PIC 9(08).
           05  AULIM-LAST-PAY-DATE         PIC 9(08).
           05  AULIM-TIER-CD               PIC X(02).
               88  AULIM-TIER-CD-EW                VALUE 'EW'.
               88  AULIM-TIER-CD-JF                VALUE 'JF'.
               88  AULIM-TIER-CD-KD                VALUE 'KD'.
               88  AULIM-TIER-CD-UG                VALUE 'UG'.
               88  AULIM-TIER-CD-WY                VALUE 'WY'.
           05  AULIM-TYPE-CD               PIC X(01).
               88  AULIM-TYPE-CD-B                 VALUE 'B'.
               88  AULIM-TYPE-CD-H                 VALUE 'H'.
               88  AULIM-TYPE-CD-S                 VALUE 'S'.
               88  AULIM-TYPE-CD-V                 VALUE 'V'.
               88  AULIM-TYPE-CD-W                 VALUE 'W'.
               88  AULIM-TYPE-CD-Z                 VALUE 'Z'.
           05  AULIM-STATUS-CD             PIC X(03).
               88  AULIM-STATUS-CD-EUA             VALUE 'EUA'.
               88  AULIM-STATUS-CD-HMW             VALUE 'HMW'.
               88  AULIM-STATUS-CD-RCB             VALUE 'RCB'.
           05  AULIM-DEALER-CD             PIC X(03).
               88  AULIM-DEALER-CD-DRU             VALUE 'DRU'.
               88  AULIM-DEALER-CD-FNF             VALUE 'FNF'.
               88  AULIM-DEALER-CD-GBX             VALUE 'GBX'.
               88  AULIM-DEALER-CD-GTU             VALUE 'GTU'.
               88  AULIM-DEALER-CD-JNE             VALUE 'JNE'.
               88  AULIM-DEALER-CD-KTP             VALUE 'KTP'.
           05  AULIM-EXEMPT-FLAG           PIC X(01).
               88  AULIM-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  AULIM-STOP-FLAG             PIC X(01).
               88  AULIM-STOP-FLAG-YES             VALUE 'Y'.
           05  AULIM-OVERRIDE-FLAG         PIC X(01).
               88  AULIM-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  AULIM-CYCLE-CNT             PIC 9(07).
           05  AULIM-PAYMENT-CNT           PIC 9(07).
           05  AULIM-MARGIN                PIC 9(03)V9(06).
           05  AULIM-FEE-PCT               PIC 9(03)V9(06).
           05  AULIM-ADDR                  PIC X(30).
           05  AULIM-NAME                  PIC X(30).
           05  AULIM-RETURN-CODE           PIC 9(02).
