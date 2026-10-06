      *================================================================*
      * COPYBOOK : AUDTEP                                              *
      * PURPOSE  : LINKAGE FOR DEALER AUTO FINANCE RULE MODULE AUDTE   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-10-08 LMB  ORIGINAL                                       *
      * 1995-08-20 GAF  NEW REBATE REPORT COLUMNS REQ1456              *
      * 1997-08-01 SKN  RESERVE EXCEPTIONS TO OPS QUEUE PRB2128        *
      * 1999-01-26 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-01-14 MKL  GAP NOTICE TEXT PER LEGAL REQ7540              *
      * 2000-01-27 LMB  CHARGEBK ROUNDING - HALF UP REQ1689            *
      * 2002-01-21 LMB  RESERVE CUTOFF MOVED REQ7015                   *
      * 2003-07-05 CRP  PAYMENT RERUN PROTECTION REQ1776               *
      * 2005-03-21 PQR  AMOUNT-FIN EXCEPTIONS TO OPS QUEUE REQ4614     *
      * 2006-03-25 DPS  GAP EXCEPTIONS TO OPS QUEUE CHG7514            *
      * 2007-11-12 TWH  RESERVE LIMIT REVISED REQ3482                  *
      * 2010-01-02 SKN  GAP ROUNDING - HALF UP INC1345                 *
      * 2010-06-20 CRP  CHARGEBK ROUNDING - HALF UP INC2431            *
      *================================================================*
       01  AUDTE-PARMS.
           05  AUDTE-PAYMENT-YTD           PIC S9(13)V99 COMP-3.
           05  AUDTE-REBATE-YTD            PIC S9(13)V99 COMP-3.
           05  AUDTE-REBATE-AMT            PIC S9(13)V99 COMP-3.
           05  AUDTE-AMOUNT-FIN-AMT        PIC S9(13)V99 COMP-3.
           05  AUDTE-PAYMENT-AMT           PIC S9(13)V99 COMP-3.
           05  AUDTE-GAP-AMT               PIC S9(13)V99 COMP-3.
           05  AUDTE-NOTICE-DATE           PIC 9(08).
           05  AUDTE-LAST-MAINT-DATE       PIC 9(08).
           05  AUDTE-EFFECTIVE-DATE        PIC 9(08).
           05  AUDTE-EXPIRY-DATE           PIC 9(08).
           05  AUDTE-TYPE-CD               PIC X(01).
               88  AUDTE-TYPE-CD-B                 VALUE 'B'.
               88  AUDTE-TYPE-CD-H                 VALUE 'H'.
               88  AUDTE-TYPE-CD-S                 VALUE 'S'.
               88  AUDTE-TYPE-CD-V                 VALUE 'V'.
               88  AUDTE-TYPE-CD-W                 VALUE 'W'.
               88  AUDTE-TYPE-CD-Z                 VALUE 'Z'.
           05  AUDTE-TIER-CD               PIC X(02).
               88  AUDTE-TIER-CD-EW                VALUE 'EW'.
               88  AUDTE-TIER-CD-JF                VALUE 'JF'.
               88  AUDTE-TIER-CD-KD                VALUE 'KD'.
               88  AUDTE-TIER-CD-UG                VALUE 'UG'.
               88  AUDTE-TIER-CD-WY                VALUE 'WY'.
           05  AUDTE-DEALER-CD             PIC X(03).
               88  AUDTE-DEALER-CD-DRU             VALUE 'DRU'.
               88  AUDTE-DEALER-CD-FNF             VALUE 'FNF'.
               88  AUDTE-DEALER-CD-GBX             VALUE 'GBX'.
               88  AUDTE-DEALER-CD-GTU             VALUE 'GTU'.
               88  AUDTE-DEALER-CD-JNE             VALUE 'JNE'.
               88  AUDTE-DEALER-CD-KTP             VALUE 'KTP'.
           05  AUDTE-CYCLE-CD              PIC X(01).
               88  AUDTE-CYCLE-CD-A                VALUE 'A'.
               88  AUDTE-CYCLE-CD-H                VALUE 'H'.
               88  AUDTE-CYCLE-CD-J                VALUE 'J'.
               88  AUDTE-CYCLE-CD-T                VALUE 'T'.
               88  AUDTE-CYCLE-CD-W                VALUE 'W'.
           05  AUDTE-OVERRIDE-FLAG         PIC X(01).
               88  AUDTE-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  AUDTE-NOTICE-SENT-FLAG      PIC X(01).
               88  AUDTE-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  AUDTE-REVIEW-FLAG           PIC X(01).
               88  AUDTE-REVIEW-FLAG-YES           VALUE 'Y'.
           05  AUDTE-RETRY-CNT             PIC 9(07).
           05  AUDTE-EXCEPT-CNT            PIC 9(07).
           05  AUDTE-FEE-PCT               PIC 9(03)V9(06).
           05  AUDTE-MARGIN                PIC 9(03)V9(06).
           05  AUDTE-DESC                  PIC X(30).
           05  AUDTE-NAME                  PIC X(30).
           05  AUDTE-RETURN-CODE           PIC 9(02).
