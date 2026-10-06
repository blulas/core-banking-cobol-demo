      *================================================================*
      * COPYBOOK : EUREXT1                                             *
      * PURPOSE  : EURO CONVERSION ARCHIVE RECORD LAYOUT               *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-11-18 MKL  ORIGINAL                                       *
      * 2000-04-25 DPS  CONVERTED STATUS CODES ADDED REQ3214           *
      * 2001-05-23 PQR  PERFORMANCE - TRIANGULAR READ AHEAD REQ8219    *
      * 2007-08-04 TWH  NATIONAL NOTICE TEXT PER LEGAL PRB3561         *
      * 2008-09-28 RJT  NEW RESIDUAL REPORT COLUMNS CHG9232            *
      * 2008-04-17 PQR  AUDIT FINDING - EURO CONTROL TOTALS PRB9971    *
      * 2009-09-01 EWS  TRIANGULAR LIMIT REVISED CHG9122               *
      * 2009-01-15 EWS  ROUNDING STATUS CODES ADDED REQ6818            *
      * 2010-12-21 LMB  EURO RERUN PROTECTION PRB5588                  *
      * 2010-04-18 LMB  TRIANGULAR LIMIT REVISED PRB9657               *
      *================================================================*
       01  EUR-H1-REC.
           05  EURH1-DESC                  PIC X(30).
           05  EURH1-CUST-ID               PIC X(10).
           05  EURH1-CONTACT               PIC X(20).
           05  EURH1-KEY                   PIC X(12).
           05  EURH1-NAME                  PIC X(30).
           05  EURH1-ADDR                  PIC X(30).
           05  EURH1-REF-NO                PIC X(16).
           05  EUR-H1-AMOUNTS.
               10  EURH1-ROUNDING-YTD      PIC S9(13)V99 COMP-3.
               10  EURH1-NATIONAL-AMT      PIC S9(13)V99 COMP-3.
               10  EURH1-TRIANGULAR-YTD    PIC S9(13)V99 COMP-3.
               10  EURH1-CONVERTED-AMT     PIC S9(13)V99 COMP-3.
               10  EURH1-TRIANGULAR-AMT    PIC S9(13)V99 COMP-3.
               10  EURH1-EURO-AMT          PIC S9(13)V99 COMP-3.
               10  EURH1-ROUNDING-AMT      PIC S9(13)V99 COMP-3.
               10  EURH1-EURO-YTD          PIC S9(13)V99 COMP-3.
               10  EURH1-NATIONAL-YTD      PIC S9(13)V99 COMP-3.
               10  EURH1-RESIDUAL-AMT      PIC S9(13)V99 COMP-3.
               10  EURH1-FEE-YTD           PIC S9(13)V99 COMP-3.
           05  EUR-H1-DATES.
               10  EURH1-LAST-STMT-DATE    PIC 9(08).
               10  EURH1-NOTICE-DATE       PIC 9(08).
               10  EURH1-LAST-MAINT-DATE   PIC 9(08).
               10  EURH1-LAST-PAY-DATE     PIC 9(08).
               10  EURH1-OPEN-DATE         PIC 9(08).
               10  EURH1-EFFECTIVE-DATE    PIC 9(08).
               10  EURH1-NEXT-DUE-DATE     PIC 9(08).
               10  EURH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  EURH1-MATURITY-DATE     PIC 9(08).
           05  EUR-H1-CODES.
               10  EURH1-STATUS-CD         PIC X(01).
               10  EURH1-PREFERRED-FLAG    PIC X(01).
                   88  EURH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  EURH1-REVIEW-FLAG       PIC X(01).
                   88  EURH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  EURH1-OVERRIDE-FLAG     PIC X(01).
                   88  EURH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  EURH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  EURH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  EURH1-STOP-FLAG         PIC X(01).
                   88  EURH1-STOP-FLAG-YES         VALUE 'Y'.
               10  EURH1-CHANNEL-CD        PIC X(03).
                   88  EURH1-CHANNEL-CD-AEA        VALUE 'AEA'.
                   88  EURH1-CHANNEL-CD-EBM        VALUE 'EBM'.
                   88  EURH1-CHANNEL-CD-GAU        VALUE 'GAU'.
                   88  EURH1-CHANNEL-CD-GSU        VALUE 'GSU'.
                   88  EURH1-CHANNEL-CD-VWP        VALUE 'VWP'.
                   88  EURH1-CHANNEL-CD-XDF        VALUE 'XDF'.
               10  EURH1-TYPE-CD           PIC X(03).
                   88  EURH1-TYPE-CD-AGL           VALUE 'AGL'.
                   88  EURH1-TYPE-CD-SMF           VALUE 'SMF'.
                   88  EURH1-TYPE-CD-UEC           VALUE 'UEC'.
                   88  EURH1-TYPE-CD-VJA           VALUE 'VJA'.
                   88  EURH1-TYPE-CD-WPH           VALUE 'WPH'.
               10  EURH1-CONV-METHOD-CD    PIC X(02).
                   88  EURH1-CONV-METHOD-CD-GL     VALUE 'GL'.
                   88  EURH1-CONV-METHOD-CD-JA     VALUE 'JA'.
                   88  EURH1-CONV-METHOD-CD-LD     VALUE 'LD'.
                   88  EURH1-CONV-METHOD-CD-MR     VALUE 'MR'.
                   88  EURH1-CONV-METHOD-CD-VC     VALUE 'VC'.
               10  EURH1-CURRENCY-CD       PIC X(02).
                   88  EURH1-CURRENCY-CD-EW        VALUE 'EW'.
                   88  EURH1-CURRENCY-CD-FJ        VALUE 'FJ'.
                   88  EURH1-CURRENCY-CD-JC        VALUE 'JC'.
               10  EURH1-EXEMPT-FLAG       PIC X(01).
                   88  EURH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  EURH1-REGION-CD         PIC X(03).
                   88  EURH1-REGION-CD-DTL         VALUE 'DTL'.
                   88  EURH1-REGION-CD-HYD         VALUE 'HYD'.
                   88  EURH1-REGION-CD-JUL         VALUE 'JUL'.
                   88  EURH1-REGION-CD-WSE         VALUE 'WSE'.
                   88  EURH1-REGION-CD-YAU         VALUE 'YAU'.
                   88  EURH1-REGION-CD-YJB         VALUE 'YJB'.
           05  EUR-H1-COUNTERS.
               10  EURH1-EXCEPT-CNT        PIC 9(07).
               10  EURH1-CYCLE-CNT         PIC 9(07).
               10  EURH1-PENALTY-PCT       PIC 9(03)V9(06).
               10  EURH1-PAYMENT-CNT       PIC 9(07).
               10  EURH1-FEE-PCT           PIC 9(03)V9(06).
               10  EURH1-RATE              PIC 9(03)V9(06).
               10  EURH1-ITEM-CNT          PIC 9(07).
           05  FILLER                      PIC X(20).
