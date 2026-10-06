      *================================================================*
      * COPYBOOK : LBXMST                                              *
      * PURPOSE  : LOCKBOX REMITTANCE MASTER RECORD (LBXMST KSDS)      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-07-24 TWH  ORIGINAL                                       *
      * 1998-01-01 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-07-11 BHS  REMIT STATUS CODES ADDED REQ2905               *
      * 2002-12-13 LMB  CASH-ON-HAND LIMIT REVISED PRB1829             *
      * 2003-04-15 DPS  CASH-ON-HAND STATUS CODES ADDED CHG5048        *
      * 2007-12-14 LMB  CASH-ON-HAND TABLE EXPANDED CHG7792            *
      * 2009-07-15 LMB  DEPOSIT STATUS CODES ADDED INC7278             *
      * 2009-08-08 PQR  NEW FLOAT REPORT COLUMNS INC5352               *
      * 2010-11-05 EWS  CHECK ROUNDING - HALF UP CHG6635               *
      * 2010-06-16 CRP  EXCEPT TABLE EXPANDED CHG3723                  *
      * 2010-02-02 CRP  CHECK LIMIT REVISED CHG1981                    *
      * 2011-10-22 EWS  FEE ROUNDING - HALF UP REQ5698                 *
      * 2012-11-01 CRP  FEE NOTICE TEXT PER LEGAL PRB5952              *
      *================================================================*
       01  LBX-MASTER-REC.
           05  LBXM-KEY                    PIC X(12).
           05  LBXM-CUST-ID                PIC X(10).
           05  LBXM-NAME                   PIC X(30).
           05  LBXM-DESC                   PIC X(30).
           05  LBXM-REF-NO                 PIC X(16).
           05  LBXM-ADDR                   PIC X(30).
           05  LBXM-CONTACT                PIC X(20).
           05  LBX-MASTER-AMOUNTS.
               10  LBXM-REMIT-AMT          PIC S9(13)V99 COMP-3.
               10  LBXM-REMIT-YTD          PIC S9(13)V99 COMP-3.
               10  LBXM-CHECK-AMT          PIC S9(13)V99 COMP-3.
               10  LBXM-CHECK-YTD          PIC S9(13)V99 COMP-3.
               10  LBXM-CASH-ON-HAND-AMT   PIC S9(13)V99 COMP-3.
               10  LBXM-CASH-ON-HAND-YTD   PIC S9(13)V99 COMP-3.
               10  LBXM-FLOAT-AMT          PIC S9(13)V99 COMP-3.
               10  LBXM-FLOAT-YTD          PIC S9(13)V99 COMP-3.
               10  LBXM-DEPOSIT-AMT        PIC S9(13)V99 COMP-3.
               10  LBXM-DEPOSIT-YTD        PIC S9(13)V99 COMP-3.
               10  LBXM-FEE-AMT            PIC S9(13)V99 COMP-3.
               10  LBXM-FEE-YTD            PIC S9(13)V99 COMP-3.
               10  LBXM-EXCEPT-AMT         PIC S9(13)V99 COMP-3.
               10  LBXM-EXCEPT-YTD         PIC S9(13)V99 COMP-3.
           05  LBX-MASTER-DATES.
               10  LBXM-NEXT-DUE-DATE      PIC 9(08).
               10  LBXM-EXPIRY-DATE        PIC 9(08).
               10  LBXM-LAST-PAY-DATE      PIC 9(08).
               10  LBXM-LAST-STMT-DATE     PIC 9(08).
               10  LBXM-MATURITY-DATE      PIC 9(08).
               10  LBXM-EFFECTIVE-DATE     PIC 9(08).
               10  LBXM-NOTICE-DATE        PIC 9(08).
               10  LBXM-CLOSE-DATE         PIC 9(08).
               10  LBXM-OPEN-DATE          PIC 9(08).
           05  LBX-MASTER-CODES.
               10  LBXM-LOCKBOX-CD         PIC X(02).
                   88  LBXM-LOCKBOX-CD-DX          VALUE 'DX'.
                   88  LBXM-LOCKBOX-CD-JM          VALUE 'JM'.
                   88  LBXM-LOCKBOX-CD-WW          VALUE 'WW'.
                   88  LBXM-LOCKBOX-CD-XE          VALUE 'XE'.
               10  LBXM-BATCH-TYPE-CD      PIC X(01).
               10  LBXM-CHANNEL-CD         PIC X(01).
                   88  LBXM-CHANNEL-CD-B           VALUE 'B'.
                   88  LBXM-CHANNEL-CD-H           VALUE 'H'.
                   88  LBXM-CHANNEL-CD-Z           VALUE 'Z'.
               10  LBXM-REGION-CD          PIC X(03).
                   88  LBXM-REGION-CD-DVP          VALUE 'DVP'.
                   88  LBXM-REGION-CD-FMZ          VALUE 'FMZ'.
                   88  LBXM-REGION-CD-KSL          VALUE 'KSL'.
                   88  LBXM-REGION-CD-NYK          VALUE 'NYK'.
               10  LBXM-CYCLE-CD           PIC X(03).
                   88  LBXM-CYCLE-CD-EDV           VALUE 'EDV'.
                   88  LBXM-CYCLE-CD-EPM           VALUE 'EPM'.
                   88  LBXM-CYCLE-CD-FZY           VALUE 'FZY'.
                   88  LBXM-CYCLE-CD-XUU           VALUE 'XUU'.
                   88  LBXM-CYCLE-CD-YBH           VALUE 'YBH'.
               10  LBXM-REASON-CD          PIC X(02).
                   88  LBXM-REASON-CD-GF           VALUE 'GF'.
                   88  LBXM-REASON-CD-UK           VALUE 'UK'.
                   88  LBXM-REASON-CD-VR           VALUE 'VR'.
                   88  LBXM-REASON-CD-XG           VALUE 'XG'.
               10  LBXM-NOTICE-SENT-FLAG   PIC X(01).
                   88  LBXM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  LBXM-PREFERRED-FLAG     PIC X(01).
                   88  LBXM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  LBXM-HOLD-FLAG          PIC X(01).
                   88  LBXM-HOLD-FLAG-YES          VALUE 'Y'.
               10  LBXM-AUTO-RENEW-FLAG    PIC X(01).
                   88  LBXM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  LBXM-EXEMPT-FLAG        PIC X(01).
                   88  LBXM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  LBXM-STOP-FLAG          PIC X(01).
                   88  LBXM-STOP-FLAG-YES          VALUE 'Y'.
           05  LBX-MASTER-COUNTERS.
               10  LBXM-ITEM-CNT           PIC 9(07).
               10  LBXM-PAYMENT-CNT        PIC 9(07).
               10  LBXM-RETRY-CNT          PIC 9(07).
               10  LBXM-EXCEPT-CNT         PIC 9(07).
               10  LBXM-FEE-PCT            PIC 9(03)V9(06).
               10  LBXM-PENALTY-PCT        PIC 9(03)V9(06).
               10  LBXM-DISCOUNT           PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
