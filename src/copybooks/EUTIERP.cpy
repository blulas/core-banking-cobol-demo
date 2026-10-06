      *================================================================*
      * COPYBOOK : EUTIERP                                             *
      * PURPOSE  : LINKAGE FOR EURO CONVERSION RULE MODULE EUTIER      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-12-28 LMB  ORIGINAL                                       *
      * 1997-06-17 EWS  CONVERTED CALCULATION CORRECTED PRB4135        *
      * 1998-04-10 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-05-23 CRP  TRIANGULAR RERUN PROTECTION CHG0242            *
      * 2002-11-24 LMB  TRIANGULAR ROUNDING - HALF UP PRB5486          *
      * 2004-04-26 PQR  TRIANGULAR CUTOFF MOVED PRB3941                *
      * 2006-08-20 LMB  EURO NOTICE TEXT PER LEGAL PRB6166             *
      *================================================================*
       01  EUTIER-PARMS.
           05  EUTIER-FEE-YTD              PIC S9(13)V99 COMP-3.
           05  EUTIER-ROUNDING-YTD         PIC S9(13)V99 COMP-3.
           05  EUTIER-TRIANGULAR-AMT       PIC S9(13)V99 COMP-3.
           05  EUTIER-EURO-YTD             PIC S9(13)V99 COMP-3.
           05  EUTIER-NATIONAL-YTD         PIC S9(13)V99 COMP-3.
           05  EUTIER-ROUNDING-AMT         PIC S9(13)V99 COMP-3.
           05  EUTIER-NOTICE-DATE          PIC 9(08).
           05  EUTIER-LAST-MAINT-DATE      PIC 9(08).
           05  EUTIER-MATURITY-DATE        PIC 9(08).
           05  EUTIER-LAST-STMT-DATE       PIC 9(08).
           05  EUTIER-CHANNEL-CD           PIC X(03).
               88  EUTIER-CHANNEL-CD-AEA           VALUE 'AEA'.
               88  EUTIER-CHANNEL-CD-EBM           VALUE 'EBM'.
               88  EUTIER-CHANNEL-CD-GAU           VALUE 'GAU'.
               88  EUTIER-CHANNEL-CD-GSU           VALUE 'GSU'.
               88  EUTIER-CHANNEL-CD-VWP           VALUE 'VWP'.
               88  EUTIER-CHANNEL-CD-XDF           VALUE 'XDF'.
           05  EUTIER-REGION-CD            PIC X(03).
               88  EUTIER-REGION-CD-DTL            VALUE 'DTL'.
               88  EUTIER-REGION-CD-HYD            VALUE 'HYD'.
               88  EUTIER-REGION-CD-JUL            VALUE 'JUL'.
               88  EUTIER-REGION-CD-WSE            VALUE 'WSE'.
               88  EUTIER-REGION-CD-YAU            VALUE 'YAU'.
               88  EUTIER-REGION-CD-YJB            VALUE 'YJB'.
           05  EUTIER-STATUS-CD            PIC X(01).
           05  EUTIER-CURRENCY-CD          PIC X(02).
               88  EUTIER-CURRENCY-CD-EW           VALUE 'EW'.
               88  EUTIER-CURRENCY-CD-FJ           VALUE 'FJ'.
               88  EUTIER-CURRENCY-CD-JC           VALUE 'JC'.
           05  EUTIER-AUTO-RENEW-FLAG      PIC X(01).
               88  EUTIER-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  EUTIER-EXEMPT-FLAG          PIC X(01).
               88  EUTIER-EXEMPT-FLAG-YES          VALUE 'Y'.
           05  EUTIER-STOP-FLAG            PIC X(01).
               88  EUTIER-STOP-FLAG-YES            VALUE 'Y'.
           05  EUTIER-PAYMENT-CNT          PIC 9(07).
           05  EUTIER-EXCEPT-CNT           PIC 9(07).
           05  EUTIER-FEE-PCT              PIC 9(03)V9(06).
           05  EUTIER-PENALTY-PCT          PIC 9(03)V9(06).
           05  EUTIER-CUST-ID              PIC X(10).
           05  EUTIER-REF-NO               PIC X(16).
           05  EUTIER-RETURN-CODE          PIC 9(02).
