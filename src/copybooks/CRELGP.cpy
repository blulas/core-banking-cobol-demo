      *================================================================*
      * COPYBOOK : CRELGP                                              *
      * PURPOSE  : LINKAGE FOR PROPRIETARY CREDIT CARD RULE MODULE     *
      *            CRELG                                               *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-10-25 EWS  ORIGINAL                                       *
      * 1996-02-20 JLM  CASH-ADV CALCULATION CORRECTED PRB1563         *
      * 1996-04-01 EWS  MIN-DUE RERUN PROTECTION PRB3416               *
      * 1998-07-03 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-06-26 MKL  LIMIT NOTICE TEXT PER LEGAL REQ2138            *
      * 2003-03-20 TWH  FINANCE NOTICE TEXT PER LEGAL CHG1941          *
      * 2006-02-13 BHS  PURCHASE STATUS CODES ADDED REQ5691            *
      * 2006-05-09 TWH  LATE-FEE LIMIT REVISED REQ6238                 *
      * 2007-12-10 TWH  LIMIT TABLE EXPANDED REQ5393                   *
      *================================================================*
       01  CRELG-PARMS.
           05  CRELG-LIMIT-YTD             PIC S9(13)V99 COMP-3.
           05  CRELG-PURCHASE-AMT          PIC S9(13)V99 COMP-3.
           05  CRELG-MIN-DUE-YTD           PIC S9(13)V99 COMP-3.
           05  CRELG-LATE-FEE-AMT          PIC S9(13)V99 COMP-3.
           05  CRELG-CASH-ADV-AMT          PIC S9(13)V99 COMP-3.
           05  CRELG-LIMIT-AMT             PIC S9(13)V99 COMP-3.
           05  CRELG-LAST-ACT-DATE         PIC 9(08).
           05  CRELG-OPEN-DATE             PIC 9(08).
           05  CRELG-LAST-PAY-DATE         PIC 9(08).
           05  CRELG-MATURITY-DATE         PIC 9(08).
           05  CRELG-TYPE-CD               PIC X(02).
               88  CRELG-TYPE-CD-GP                VALUE 'GP'.
               88  CRELG-TYPE-CD-UV                VALUE 'UV'.
               88  CRELG-TYPE-CD-YK                VALUE 'YK'.
           05  CRELG-CARD-TYPE-CD          PIC X(03).
               88  CRELG-CARD-TYPE-CD-PGU          VALUE 'PGU'.
               88  CRELG-CARD-TYPE-CD-TRN          VALUE 'TRN'.
               88  CRELG-CARD-TYPE-CD-WHR          VALUE 'WHR'.
               88  CRELG-CARD-TYPE-CD-ZWB          VALUE 'ZWB'.
           05  CRELG-BILL-CYCLE-CD         PIC X(01).
               88  CRELG-BILL-CYCLE-CD-B           VALUE 'B'.
               88  CRELG-BILL-CYCLE-CD-D           VALUE 'D'.
               88  CRELG-BILL-CYCLE-CD-E           VALUE 'E'.
               88  CRELG-BILL-CYCLE-CD-F           VALUE 'F'.
               88  CRELG-BILL-CYCLE-CD-H           VALUE 'H'.
               88  CRELG-BILL-CYCLE-CD-P           VALUE 'P'.
           05  CRELG-SOURCE-CD             PIC X(01).
               88  CRELG-SOURCE-CD-C               VALUE 'C'.
               88  CRELG-SOURCE-CD-H               VALUE 'H'.
               88  CRELG-SOURCE-CD-M               VALUE 'M'.
               88  CRELG-SOURCE-CD-R               VALUE 'R'.
               88  CRELG-SOURCE-CD-S               VALUE 'S'.
               88  CRELG-SOURCE-CD-T               VALUE 'T'.
           05  CRELG-EXEMPT-FLAG           PIC X(01).
               88  CRELG-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  CRELG-AUTO-RENEW-FLAG       PIC X(01).
               88  CRELG-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  CRELG-STOP-FLAG             PIC X(01).
               88  CRELG-STOP-FLAG-YES             VALUE 'Y'.
           05  CRELG-CYCLE-CNT             PIC 9(07).
           05  CRELG-RETRY-CNT             PIC 9(07).
           05  CRELG-DISCOUNT              PIC 9(03)V9(06).
           05  CRELG-RATE                  PIC 9(03)V9(06).
           05  CRELG-DESC                  PIC X(30).
           05  CRELG-ADDR                  PIC X(30).
           05  CRELG-RETURN-CODE           PIC 9(02).
