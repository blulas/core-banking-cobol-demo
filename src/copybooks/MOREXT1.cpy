      *================================================================*
      * COPYBOOK : MOREXT1                                             *
      * PURPOSE  : MONEY ORDERS PRIOR VERSION RECORD LAYOUT            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-02-20 EWS  ORIGINAL                                       *
      * 1994-12-13 DPS  PAID LIMIT REVISED PRB2606                     *
      * 1998-11-09 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-03-09 TWH  FEE NOTICE TEXT PER LEGAL CHG4317              *
      * 1999-08-11 MKL  ESCHEAT STATUS CODES ADDED PRB0203             *
      * 2004-03-22 LMB  OUTSTAND TABLE EXPANDED REQ3065                *
      * 2004-07-24 BHS  FACE NOTICE TEXT PER LEGAL PRB8985             *
      * 2004-08-23 JLM  ESCHEAT RERUN PROTECTION REQ9842               *
      * 2005-11-09 BHS  NEW VOIDED REPORT COLUMNS PRB5546              *
      *================================================================*
       01  MOR-H1-REC.
           05  MORH1-DESC                  PIC X(30).
           05  MORH1-KEY                   PIC X(12).
           05  MORH1-ADDR                  PIC X(30).
           05  MORH1-REF-NO                PIC X(16).
           05  MORH1-CONTACT               PIC X(20).
           05  MORH1-NAME                  PIC X(30).
           05  MOR-H1-AMOUNTS.
               10  MORH1-ESCHEAT-AMT       PIC S9(13)V99 COMP-3.
               10  MORH1-FEE-YTD           PIC S9(13)V99 COMP-3.
               10  MORH1-FACE-AMT          PIC S9(13)V99 COMP-3.
               10  MORH1-FACE-YTD          PIC S9(13)V99 COMP-3.
               10  MORH1-PAID-AMT          PIC S9(13)V99 COMP-3.
               10  MORH1-VOIDED-AMT        PIC S9(13)V99 COMP-3.
               10  MORH1-VOIDED-YTD        PIC S9(13)V99 COMP-3.
               10  MORH1-OUTSTAND-YTD      PIC S9(13)V99 COMP-3.
               10  MORH1-OUTSTAND-AMT      PIC S9(13)V99 COMP-3.
               10  MORH1-PAID-YTD          PIC S9(13)V99 COMP-3.
               10  MORH1-ESCHEAT-YTD       PIC S9(13)V99 COMP-3.
           05  MOR-H1-DATES.
               10  MORH1-LAST-MAINT-DATE   PIC 9(08).
               10  MORH1-NOTICE-DATE       PIC 9(08).
               10  MORH1-OPEN-DATE         PIC 9(08).
               10  MORH1-EFFECTIVE-DATE    PIC 9(08).
               10  MORH1-CLOSE-DATE        PIC 9(08).
               10  MORH1-LAST-ACT-DATE     PIC 9(08).
               10  MORH1-LAST-STMT-DATE    PIC 9(08).
               10  MORH1-LAST-PAY-DATE     PIC 9(08).
               10  MORH1-EXPIRY-DATE       PIC 9(08).
           05  MOR-H1-CODES.
               10  MORH1-REVIEW-FLAG       PIC X(01).
                   88  MORH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  MORH1-PAYEE-TYPE-CD     PIC X(02).
                   88  MORH1-PAYEE-TYPE-CD-CJ      VALUE 'CJ'.
                   88  MORH1-PAYEE-TYPE-CD-FY      VALUE 'FY'.
                   88  MORH1-PAYEE-TYPE-CD-HX      VALUE 'HX'.
                   88  MORH1-PAYEE-TYPE-CD-UV      VALUE 'UV'.
                   88  MORH1-PAYEE-TYPE-CD-VV      VALUE 'VV'.
               10  MORH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  MORH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  MORH1-TYPE-CD           PIC X(03).
                   88  MORH1-TYPE-CD-AGE           VALUE 'AGE'.
                   88  MORH1-TYPE-CD-JDX           VALUE 'JDX'.
                   88  MORH1-TYPE-CD-LDX           VALUE 'LDX'.
                   88  MORH1-TYPE-CD-RHT           VALUE 'RHT'.
               10  MORH1-CHANNEL-CD        PIC X(02).
                   88  MORH1-CHANNEL-CD-DR         VALUE 'DR'.
                   88  MORH1-CHANNEL-CD-DX         VALUE 'DX'.
                   88  MORH1-CHANNEL-CD-PT         VALUE 'PT'.
                   88  MORH1-CHANNEL-CD-RN         VALUE 'RN'.
                   88  MORH1-CHANNEL-CD-YD         VALUE 'YD'.
                   88  MORH1-CHANNEL-CD-YU         VALUE 'YU'.
               10  MORH1-ITEM-TYPE-CD      PIC X(01).
                   88  MORH1-ITEM-TYPE-CD-B        VALUE 'B'.
                   88  MORH1-ITEM-TYPE-CD-H        VALUE 'H'.
                   88  MORH1-ITEM-TYPE-CD-S        VALUE 'S'.
                   88  MORH1-ITEM-TYPE-CD-T        VALUE 'T'.
               10  MORH1-STOP-FLAG         PIC X(01).
                   88  MORH1-STOP-FLAG-YES         VALUE 'Y'.
               10  MORH1-EXEMPT-FLAG       PIC X(01).
                   88  MORH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  MORH1-PREFERRED-FLAG    PIC X(01).
                   88  MORH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  MORH1-CYCLE-CD          PIC X(03).
                   88  MORH1-CYCLE-CD-DGU          VALUE 'DGU'.
                   88  MORH1-CYCLE-CD-GEJ          VALUE 'GEJ'.
                   88  MORH1-CYCLE-CD-JBE          VALUE 'JBE'.
           05  MOR-H1-COUNTERS.
               10  MORH1-ITEM-CNT          PIC 9(07).
               10  MORH1-RETRY-CNT         PIC 9(07).
               10  MORH1-DISCOUNT          PIC 9(03)V9(06).
               10  MORH1-FEE-PCT           PIC 9(03)V9(06).
               10  MORH1-NOTICE-CNT        PIC 9(07).
               10  MORH1-PAYMENT-CNT       PIC 9(07).
               10  MORH1-PENALTY-PCT       PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
