      *================================================================*
      * COPYBOOK : CRCHST1                                             *
      * PURPOSE  : PROPRIETARY CREDIT CARD HISTORY RECORD LAYOUT       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-02-23 MKL  ORIGINAL                                       *
      * 1997-09-03 DPS  PERFORMANCE - LATE-FEE READ AHEAD INC0945      *
      * 1998-06-21 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-10-07 CRP  FINANCE CALCULATION CORRECTED INC1617          *
      * 2001-11-26 BHS  FINANCE CUTOFF MOVED CHG5383                   *
      * 2001-09-12 CRP  PURCHASE STATUS CODES ADDED CHG7506            *
      * 2001-05-06 CRP  LATE-FEE RERUN PROTECTION PRB6659              *
      * 2003-02-01 EWS  NEW FINANCE REPORT COLUMNS CHG3909             *
      * 2006-12-19 JLM  FINANCE LIMIT REVISED CHG4847                  *
      *================================================================*
       01  CRC-H1-REC.
           05  CRCH1-CONTACT               PIC X(20).
           05  CRCH1-KEY                   PIC X(12).
           05  CRCH1-REF-NO                PIC X(16).
           05  CRCH1-CUST-ID               PIC X(10).
           05  CRCH1-NAME                  PIC X(30).
           05  CRCH1-DESC                  PIC X(30).
           05  CRCH1-ADDR                  PIC X(30).
           05  CRC-H1-AMOUNTS.
               10  CRCH1-MIN-DUE-AMT       PIC S9(13)V99 COMP-3.
               10  CRCH1-CASH-ADV-AMT      PIC S9(13)V99 COMP-3.
               10  CRCH1-FINANCE-AMT       PIC S9(13)V99 COMP-3.
               10  CRCH1-LIMIT-AMT         PIC S9(13)V99 COMP-3.
               10  CRCH1-FINANCE-YTD       PIC S9(13)V99 COMP-3.
               10  CRCH1-MIN-DUE-YTD       PIC S9(13)V99 COMP-3.
               10  CRCH1-PURCHASE-AMT      PIC S9(13)V99 COMP-3.
               10  CRCH1-LATE-FEE-YTD      PIC S9(13)V99 COMP-3.
               10  CRCH1-LIMIT-YTD         PIC S9(13)V99 COMP-3.
               10  CRCH1-PURCHASE-YTD      PIC S9(13)V99 COMP-3.
               10  CRCH1-CASH-ADV-YTD      PIC S9(13)V99 COMP-3.
               10  CRCH1-PAYMENT-AMT       PIC S9(13)V99 COMP-3.
               10  CRCH1-LATE-FEE-AMT      PIC S9(13)V99 COMP-3.
               10  CRCH1-PAYMENT-YTD       PIC S9(13)V99 COMP-3.
           05  CRC-H1-DATES.
               10  CRCH1-LAST-ACT-DATE     PIC 9(08).
               10  CRCH1-EXPIRY-DATE       PIC 9(08).
               10  CRCH1-OPEN-DATE         PIC 9(08).
               10  CRCH1-MATURITY-DATE     PIC 9(08).
               10  CRCH1-EFFECTIVE-DATE    PIC 9(08).
               10  CRCH1-NOTICE-DATE       PIC 9(08).
               10  CRCH1-CLOSE-DATE        PIC 9(08).
           05  CRC-H1-CODES.
               10  CRCH1-BRANCH-CD         PIC X(03).
                   88  CRCH1-BRANCH-CD-CTB         VALUE 'CTB'.
                   88  CRCH1-BRANCH-CD-LNX         VALUE 'LNX'.
                   88  CRCH1-BRANCH-CD-MGS         VALUE 'MGS'.
                   88  CRCH1-BRANCH-CD-YNJ         VALUE 'YNJ'.
               10  CRCH1-STOP-FLAG         PIC X(01).
                   88  CRCH1-STOP-FLAG-YES         VALUE 'Y'.
               10  CRCH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  CRCH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  CRCH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  CRCH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  CRCH1-SOURCE-CD         PIC X(01).
                   88  CRCH1-SOURCE-CD-C           VALUE 'C'.
                   88  CRCH1-SOURCE-CD-H           VALUE 'H'.
                   88  CRCH1-SOURCE-CD-M           VALUE 'M'.
                   88  CRCH1-SOURCE-CD-R           VALUE 'R'.
                   88  CRCH1-SOURCE-CD-S           VALUE 'S'.
                   88  CRCH1-SOURCE-CD-T           VALUE 'T'.
               10  CRCH1-TYPE-CD           PIC X(02).
                   88  CRCH1-TYPE-CD-GP            VALUE 'GP'.
                   88  CRCH1-TYPE-CD-UV            VALUE 'UV'.
                   88  CRCH1-TYPE-CD-YK            VALUE 'YK'.
               10  CRCH1-BILL-CYCLE-CD     PIC X(01).
                   88  CRCH1-BILL-CYCLE-CD-B       VALUE 'B'.
                   88  CRCH1-BILL-CYCLE-CD-D       VALUE 'D'.
                   88  CRCH1-BILL-CYCLE-CD-E       VALUE 'E'.
                   88  CRCH1-BILL-CYCLE-CD-F       VALUE 'F'.
                   88  CRCH1-BILL-CYCLE-CD-H       VALUE 'H'.
                   88  CRCH1-BILL-CYCLE-CD-P       VALUE 'P'.
               10  CRCH1-PREFERRED-FLAG    PIC X(01).
                   88  CRCH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  CRCH1-OVERRIDE-FLAG     PIC X(01).
                   88  CRCH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  CRCH1-EXEMPT-FLAG       PIC X(01).
                   88  CRCH1-EXEMPT-FLAG-YES       VALUE 'Y'.
           05  CRC-H1-COUNTERS.
               10  CRCH1-NOTICE-CNT        PIC 9(07).
               10  CRCH1-PENALTY-PCT       PIC 9(03)V9(06).
               10  CRCH1-CYCLE-CNT         PIC 9(07).
               10  CRCH1-DISCOUNT          PIC 9(03)V9(06).
               10  CRCH1-ITEM-CNT          PIC 9(07).
               10  CRCH1-RETRY-CNT         PIC 9(07).
               10  CRCH1-RATE              PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
