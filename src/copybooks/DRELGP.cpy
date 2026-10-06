      *================================================================*
      * COPYBOOK : DRELGP                                              *
      * PURPOSE  : LINKAGE FOR DIVIDEND REINVESTMENT RULE MODULE DRELG *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-10-26 TWH  ORIGINAL                                       *
      * 1990-01-26 JLM  REINVEST CUTOFF MOVED PRB5216                  *
      * 1992-10-16 BHS  CASH-PAY RERUN PROTECTION PRB3310              *
      * 1992-08-10 DPS  FEE CALCULATION CORRECTED CHG6396              *
      * 1997-05-08 BHS  SHARES-AMT NOTICE TEXT PER LEGAL PRB6255       *
      * 1998-08-03 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-11-15 LMB  CASH-PAY NOTICE TEXT PER LEGAL REQ0108         *
      * 2000-04-06 BHS  DIVIDEND TABLE EXPANDED INC4769                *
      * 2010-02-15 BHS  CASH-PAY LIMIT REVISED PRB2496                 *
      *================================================================*
       01  DRELG-PARMS.
           05  DRELG-REINVEST-AMT          PIC S9(13)V99 COMP-3.
           05  DRELG-WITHHOLD-AMT          PIC S9(13)V99 COMP-3.
           05  DRELG-FEE-YTD               PIC S9(13)V99 COMP-3.
           05  DRELG-SHARES-AMT-AMT        PIC S9(13)V99 COMP-3.
           05  DRELG-CASH-PAY-AMT          PIC S9(13)V99 COMP-3.
           05  DRELG-CASH-PAY-YTD          PIC S9(13)V99 COMP-3.
           05  DRELG-LAST-MAINT-DATE       PIC 9(08).
           05  DRELG-MATURITY-DATE         PIC 9(08).
           05  DRELG-OPEN-DATE             PIC 9(08).
           05  DRELG-EFFECTIVE-DATE        PIC 9(08).
           05  DRELG-SOURCE-CD             PIC X(02).
               88  DRELG-SOURCE-CD-BT              VALUE 'BT'.
               88  DRELG-SOURCE-CD-FL              VALUE 'FL'.
               88  DRELG-SOURCE-CD-NK              VALUE 'NK'.
               88  DRELG-SOURCE-CD-YC              VALUE 'YC'.
           05  DRELG-PLAN-OPTION-CD        PIC X(01).
               88  DRELG-PLAN-OPTION-CD-E          VALUE 'E'.
               88  DRELG-PLAN-OPTION-CD-R          VALUE 'R'.
               88  DRELG-PLAN-OPTION-CD-T          VALUE 'T'.
               88  DRELG-PLAN-OPTION-CD-Z          VALUE 'Z'.
           05  DRELG-CYCLE-CD              PIC X(02).
           05  DRELG-CHANNEL-CD            PIC X(02).
               88  DRELG-CHANNEL-CD-RM             VALUE 'RM'.
               88  DRELG-CHANNEL-CD-VP             VALUE 'VP'.
               88  DRELG-CHANNEL-CD-YS             VALUE 'YS'.
           05  DRELG-STOP-FLAG             PIC X(01).
               88  DRELG-STOP-FLAG-YES             VALUE 'Y'.
           05  DRELG-SUSPEND-FLAG          PIC X(01).
               88  DRELG-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  DRELG-PREFERRED-FLAG        PIC X(01).
               88  DRELG-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  DRELG-EXCEPT-CNT            PIC 9(07).
           05  DRELG-ITEM-CNT              PIC 9(07).
           05  DRELG-DISCOUNT              PIC 9(03)V9(06).
           05  DRELG-RATE                  PIC 9(03)V9(06).
           05  DRELG-REF-NO                PIC X(16).
           05  DRELG-CUST-ID               PIC X(10).
           05  DRELG-RETURN-CODE           PIC 9(02).
