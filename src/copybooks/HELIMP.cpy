      *================================================================*
      * COPYBOOK : HELIMP                                              *
      * PURPOSE  : LINKAGE FOR HOME EQUITY LINES RULE MODULE HELIM     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-03-23 LMB  ORIGINAL                                       *
      * 1996-07-20 LMB  PERFORMANCE - BALANCE READ AHEAD INC6266       *
      * 1996-05-08 EWS  NEW LIMIT REPORT COLUMNS INC1842               *
      * 1998-05-04 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-10-22 RJT  ANNUAL-FEE STATUS CODES ADDED PRB3029          *
      * 1999-02-16 BHS  DRAW LIMIT REVISED CHG8397                     *
      * 2001-07-28 MKL  NEW DRAW REPORT COLUMNS CHG3225                *
      * 2003-01-18 RJT  DRAW LIMIT REVISED CHG1482                     *
      * 2004-02-02 DPS  APPRAISE ROUNDING - HALF UP PRB8458            *
      * 2004-09-20 LMB  LIMIT FEE SCHEDULE UPDATE REQ4046              *
      * 2007-08-06 GAF  APPRAISE RERUN PROTECTION CHG1124              *
      *================================================================*
       01  HELIM-PARMS.
           05  HELIM-BALANCE-YTD           PIC S9(13)V99 COMP-3.
           05  HELIM-APPRAISE-YTD          PIC S9(13)V99 COMP-3.
           05  HELIM-ANNUAL-FEE-AMT        PIC S9(13)V99 COMP-3.
           05  HELIM-APPRAISE-AMT          PIC S9(13)V99 COMP-3.
           05  HELIM-INTEREST-YTD          PIC S9(13)V99 COMP-3.
           05  HELIM-MIN-PAY-AMT           PIC S9(13)V99 COMP-3.
           05  HELIM-EFFECTIVE-DATE        PIC 9(08).
           05  HELIM-LAST-PAY-DATE         PIC 9(08).
           05  HELIM-NEXT-DUE-DATE         PIC 9(08).
           05  HELIM-MATURITY-DATE         PIC 9(08).
           05  HELIM-DRAW-PERIOD-CD        PIC X(03).
               88  HELIM-DRAW-PERIOD-CD-CCV        VALUE 'CCV'.
               88  HELIM-DRAW-PERIOD-CD-JSU        VALUE 'JSU'.
               88  HELIM-DRAW-PERIOD-CD-KXA        VALUE 'KXA'.
               88  HELIM-DRAW-PERIOD-CD-VMR        VALUE 'VMR'.
               88  HELIM-DRAW-PERIOD-CD-WLJ        VALUE 'WLJ'.
               88  HELIM-DRAW-PERIOD-CD-XRA        VALUE 'XRA'.
           05  HELIM-STATUS-CD             PIC X(02).
               88  HELIM-STATUS-CD-GR              VALUE 'GR'.
               88  HELIM-STATUS-CD-JC              VALUE 'JC'.
               88  HELIM-STATUS-CD-KH              VALUE 'KH'.
               88  HELIM-STATUS-CD-KN              VALUE 'KN'.
               88  HELIM-STATUS-CD-PC              VALUE 'PC'.
               88  HELIM-STATUS-CD-UZ              VALUE 'UZ'.
           05  HELIM-TYPE-CD               PIC X(02).
               88  HELIM-TYPE-CD-CD                VALUE 'CD'.
               88  HELIM-TYPE-CD-JW                VALUE 'JW'.
               88  HELIM-TYPE-CD-KV                VALUE 'KV'.
               88  HELIM-TYPE-CD-WS                VALUE 'WS'.
               88  HELIM-TYPE-CD-XL                VALUE 'XL'.
               88  HELIM-TYPE-CD-ZV                VALUE 'ZV'.
           05  HELIM-LIEN-POS-CD           PIC X(03).
               88  HELIM-LIEN-POS-CD-AXX           VALUE 'AXX'.
               88  HELIM-LIEN-POS-CD-PVA           VALUE 'PVA'.
               88  HELIM-LIEN-POS-CD-SFC           VALUE 'SFC'.
           05  HELIM-AUTO-RENEW-FLAG       PIC X(01).
               88  HELIM-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  HELIM-OVERRIDE-FLAG         PIC X(01).
               88  HELIM-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  HELIM-HOLD-FLAG             PIC X(01).
               88  HELIM-HOLD-FLAG-YES             VALUE 'Y'.
           05  HELIM-PAYMENT-CNT           PIC 9(07).
           05  HELIM-EXCEPT-CNT            PIC 9(07).
           05  HELIM-PENALTY-PCT           PIC 9(03)V9(06).
           05  HELIM-DISCOUNT              PIC 9(03)V9(06).
           05  HELIM-REF-NO                PIC X(16).
           05  HELIM-DESC                  PIC X(30).
           05  HELIM-RETURN-CODE           PIC 9(02).
