      *================================================================*
      * COPYBOOK : MORMST                                              *
      * PURPOSE  : MONEY ORDERS MASTER RECORD (MORMST KSDS)            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-12-27 GAF  ORIGINAL                                       *
      * 1993-10-15 DPS  ESCHEAT RERUN PROTECTION REQ0898               *
      * 1995-12-08 JLM  ESCHEAT RERUN PROTECTION CHG8999               *
      * 1995-07-08 GAF  VOIDED ROUNDING - HALF UP CHG0229              *
      * 1995-09-22 DPS  VOIDED RERUN PROTECTION CHG3462                *
      * 1995-09-03 CRP  AUDIT FINDING - OUTSTAND CONTROL TOTALS PRB7554*
      * 1997-10-06 BHS  ESCHEAT FEE SCHEDULE UPDATE CHG3844            *
      * 2000-03-28 PQR  FEE STATUS CODES ADDED CHG3899                 *
      * 2001-10-17 EWS  PERFORMANCE - FEE READ AHEAD REQ8130           *
      * 2001-11-08 BHS  PERFORMANCE - VOIDED READ AHEAD INC5221        *
      *================================================================*
       01  MOR-MASTER-REC.
           05  MORM-KEY                    PIC X(12).
           05  MORM-CUST-ID                PIC X(10).
           05  MORM-NAME                   PIC X(30).
           05  MORM-DESC                   PIC X(30).
           05  MORM-REF-NO                 PIC X(16).
           05  MORM-ADDR                   PIC X(30).
           05  MORM-CONTACT                PIC X(20).
           05  MOR-MASTER-AMOUNTS.
               10  MORM-FACE-AMT           PIC S9(13)V99 COMP-3.
               10  MORM-FACE-YTD           PIC S9(13)V99 COMP-3.
               10  MORM-FEE-AMT            PIC S9(13)V99 COMP-3.
               10  MORM-FEE-YTD            PIC S9(13)V99 COMP-3.
               10  MORM-PAID-AMT           PIC S9(13)V99 COMP-3.
               10  MORM-PAID-YTD           PIC S9(13)V99 COMP-3.
               10  MORM-OUTSTAND-AMT       PIC S9(13)V99 COMP-3.
               10  MORM-OUTSTAND-YTD       PIC S9(13)V99 COMP-3.
               10  MORM-ESCHEAT-AMT        PIC S9(13)V99 COMP-3.
               10  MORM-ESCHEAT-YTD        PIC S9(13)V99 COMP-3.
               10  MORM-VOIDED-AMT         PIC S9(13)V99 COMP-3.
               10  MORM-VOIDED-YTD         PIC S9(13)V99 COMP-3.
           05  MOR-MASTER-DATES.
               10  MORM-LAST-PAY-DATE      PIC 9(08).
               10  MORM-LAST-STMT-DATE     PIC 9(08).
               10  MORM-OPEN-DATE          PIC 9(08).
               10  MORM-LAST-MAINT-DATE    PIC 9(08).
               10  MORM-EFFECTIVE-DATE     PIC 9(08).
               10  MORM-LAST-ACT-DATE      PIC 9(08).
               10  MORM-CLOSE-DATE         PIC 9(08).
               10  MORM-NOTICE-DATE        PIC 9(08).
               10  MORM-EXPIRY-DATE        PIC 9(08).
           05  MOR-MASTER-CODES.
               10  MORM-ITEM-TYPE-CD       PIC X(01).
                   88  MORM-ITEM-TYPE-CD-B         VALUE 'B'.
                   88  MORM-ITEM-TYPE-CD-H         VALUE 'H'.
                   88  MORM-ITEM-TYPE-CD-S         VALUE 'S'.
                   88  MORM-ITEM-TYPE-CD-T         VALUE 'T'.
               10  MORM-PAYEE-TYPE-CD      PIC X(02).
                   88  MORM-PAYEE-TYPE-CD-CJ       VALUE 'CJ'.
                   88  MORM-PAYEE-TYPE-CD-FY       VALUE 'FY'.
                   88  MORM-PAYEE-TYPE-CD-HX       VALUE 'HX'.
                   88  MORM-PAYEE-TYPE-CD-UV       VALUE 'UV'.
                   88  MORM-PAYEE-TYPE-CD-VV       VALUE 'VV'.
               10  MORM-REASON-CD          PIC X(01).
                   88  MORM-REASON-CD-B            VALUE 'B'.
                   88  MORM-REASON-CD-L            VALUE 'L'.
                   88  MORM-REASON-CD-P            VALUE 'P'.
                   88  MORM-REASON-CD-S            VALUE 'S'.
                   88  MORM-REASON-CD-T            VALUE 'T'.
                   88  MORM-REASON-CD-Z            VALUE 'Z'.
               10  MORM-TYPE-CD            PIC X(03).
                   88  MORM-TYPE-CD-AGE            VALUE 'AGE'.
                   88  MORM-TYPE-CD-JDX            VALUE 'JDX'.
                   88  MORM-TYPE-CD-LDX            VALUE 'LDX'.
                   88  MORM-TYPE-CD-RHT            VALUE 'RHT'.
               10  MORM-CHANNEL-CD         PIC X(02).
                   88  MORM-CHANNEL-CD-DR          VALUE 'DR'.
                   88  MORM-CHANNEL-CD-DX          VALUE 'DX'.
                   88  MORM-CHANNEL-CD-PT          VALUE 'PT'.
                   88  MORM-CHANNEL-CD-RN          VALUE 'RN'.
                   88  MORM-CHANNEL-CD-YD          VALUE 'YD'.
                   88  MORM-CHANNEL-CD-YU          VALUE 'YU'.
               10  MORM-CYCLE-CD           PIC X(03).
                   88  MORM-CYCLE-CD-DGU           VALUE 'DGU'.
                   88  MORM-CYCLE-CD-GEJ           VALUE 'GEJ'.
                   88  MORM-CYCLE-CD-JBE           VALUE 'JBE'.
               10  MORM-PREFERRED-FLAG     PIC X(01).
                   88  MORM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  MORM-OVERRIDE-FLAG      PIC X(01).
                   88  MORM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  MORM-AUTO-RENEW-FLAG    PIC X(01).
                   88  MORM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  MORM-REVIEW-FLAG        PIC X(01).
                   88  MORM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  MORM-EXEMPT-FLAG        PIC X(01).
                   88  MORM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  MORM-STOP-FLAG          PIC X(01).
                   88  MORM-STOP-FLAG-YES          VALUE 'Y'.
           05  MOR-MASTER-COUNTERS.
               10  MORM-NOTICE-CNT         PIC 9(07).
               10  MORM-RETRY-CNT          PIC 9(07).
               10  MORM-PAYMENT-CNT        PIC 9(07).
               10  MORM-ITEM-CNT           PIC 9(07).
               10  MORM-DISCOUNT           PIC 9(03)V9(06).
               10  MORM-PENALTY-PCT        PIC 9(03)V9(06).
               10  MORM-FEE-PCT            PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
