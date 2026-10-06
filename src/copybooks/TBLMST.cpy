      *================================================================*
      * COPYBOOK : TBLMST                                              *
      * PURPOSE  : TELLER BALANCING MASTER RECORD (TBLMST KSDS)        *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-11-12 DPS  ORIGINAL                                       *
      * 1992-10-07 GAF  VAULT-BUY STATUS CODES ADDED REQ6631           *
      * 1999-01-02 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-09-17 JLM  PERFORMANCE - OVERSHORT READ AHEAD CHG1693     *
      * 2007-03-14 DPS  CASH-IN CALCULATION CORRECTED INC4309          *
      * 2010-02-20 TWH  VAULT-BUY TABLE EXPANDED INC9337               *
      * 2010-12-17 MKL  CASH-OUT NOTICE TEXT PER LEGAL PRB4191         *
      * 2010-01-17 RJT  OVERSHORT EXCEPTIONS TO OPS QUEUE CHG9437      *
      * 2011-04-15 RJT  VAULT-SELL CUTOFF MOVED PRB2945                *
      * 2012-12-28 SKN  BATCH TABLE EXPANDED REQ6587                   *
      * 2012-06-18 RJT  AUDIT FINDING - BATCH CONTROL TOTALS CHG0737   *
      *================================================================*
       01  TBL-MASTER-REC.
           05  TBLM-KEY                    PIC X(12).
           05  TBLM-CUST-ID                PIC X(10).
           05  TBLM-NAME                   PIC X(30).
           05  TBLM-DESC                   PIC X(30).
           05  TBLM-REF-NO                 PIC X(16).
           05  TBLM-ADDR                   PIC X(30).
           05  TBLM-CONTACT                PIC X(20).
           05  TBL-MASTER-AMOUNTS.
               10  TBLM-CASH-IN-AMT        PIC S9(13)V99 COMP-3.
               10  TBLM-CASH-IN-YTD        PIC S9(13)V99 COMP-3.
               10  TBLM-CASH-OUT-AMT       PIC S9(13)V99 COMP-3.
               10  TBLM-CASH-OUT-YTD       PIC S9(13)V99 COMP-3.
               10  TBLM-OVERSHORT-AMT      PIC S9(13)V99 COMP-3.
               10  TBLM-OVERSHORT-YTD      PIC S9(13)V99 COMP-3.
               10  TBLM-VAULT-BUY-AMT      PIC S9(13)V99 COMP-3.
               10  TBLM-VAULT-BUY-YTD      PIC S9(13)V99 COMP-3.
               10  TBLM-VAULT-SELL-AMT     PIC S9(13)V99 COMP-3.
               10  TBLM-VAULT-SELL-YTD     PIC S9(13)V99 COMP-3.
               10  TBLM-BATCH-AMT          PIC S9(13)V99 COMP-3.
               10  TBLM-BATCH-YTD          PIC S9(13)V99 COMP-3.
           05  TBL-MASTER-DATES.
               10  TBLM-OPEN-DATE          PIC 9(08).
               10  TBLM-NOTICE-DATE        PIC 9(08).
               10  TBLM-LAST-ACT-DATE      PIC 9(08).
               10  TBLM-LAST-MAINT-DATE    PIC 9(08).
               10  TBLM-LAST-PAY-DATE      PIC 9(08).
               10  TBLM-LAST-STMT-DATE     PIC 9(08).
               10  TBLM-EFFECTIVE-DATE     PIC 9(08).
               10  TBLM-EXPIRY-DATE        PIC 9(08).
               10  TBLM-NEXT-REVIEW-DATE   PIC 9(08).
           05  TBL-MASTER-CODES.
               10  TBLM-DRAWER-CD          PIC X(01).
                   88  TBLM-DRAWER-CD-D            VALUE 'D'.
                   88  TBLM-DRAWER-CD-F            VALUE 'F'.
                   88  TBLM-DRAWER-CD-R            VALUE 'R'.
               10  TBLM-PROOF-TYPE-CD      PIC X(02).
                   88  TBLM-PROOF-TYPE-CD-JG       VALUE 'JG'.
                   88  TBLM-PROOF-TYPE-CD-RX       VALUE 'RX'.
                   88  TBLM-PROOF-TYPE-CD-TW       VALUE 'TW'.
                   88  TBLM-PROOF-TYPE-CD-UG       VALUE 'UG'.
                   88  TBLM-PROOF-TYPE-CD-XY       VALUE 'XY'.
               10  TBLM-CHANNEL-CD         PIC X(01).
                   88  TBLM-CHANNEL-CD-D           VALUE 'D'.
                   88  TBLM-CHANNEL-CD-F           VALUE 'F'.
                   88  TBLM-CHANNEL-CD-M           VALUE 'M'.
                   88  TBLM-CHANNEL-CD-N           VALUE 'N'.
                   88  TBLM-CHANNEL-CD-R           VALUE 'R'.
                   88  TBLM-CHANNEL-CD-T           VALUE 'T'.
               10  TBLM-SOURCE-CD          PIC X(02).
                   88  TBLM-SOURCE-CD-AF           VALUE 'AF'.
                   88  TBLM-SOURCE-CD-PT           VALUE 'PT'.
                   88  TBLM-SOURCE-CD-SZ           VALUE 'SZ'.
                   88  TBLM-SOURCE-CD-VK           VALUE 'VK'.
                   88  TBLM-SOURCE-CD-VW           VALUE 'VW'.
               10  TBLM-CYCLE-CD           PIC X(02).
                   88  TBLM-CYCLE-CD-KX            VALUE 'KX'.
                   88  TBLM-CYCLE-CD-TA            VALUE 'TA'.
                   88  TBLM-CYCLE-CD-TG            VALUE 'TG'.
                   88  TBLM-CYCLE-CD-WG            VALUE 'WG'.
               10  TBLM-REASON-CD          PIC X(02).
                   88  TBLM-REASON-CD-GS           VALUE 'GS'.
                   88  TBLM-REASON-CD-GV           VALUE 'GV'.
                   88  TBLM-REASON-CD-PG           VALUE 'PG'.
                   88  TBLM-REASON-CD-SX           VALUE 'SX'.
                   88  TBLM-REASON-CD-XN           VALUE 'XN'.
                   88  TBLM-REASON-CD-YZ           VALUE 'YZ'.
               10  TBLM-STOP-FLAG          PIC X(01).
                   88  TBLM-STOP-FLAG-YES          VALUE 'Y'.
               10  TBLM-EXEMPT-FLAG        PIC X(01).
                   88  TBLM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  TBLM-REVIEW-FLAG        PIC X(01).
                   88  TBLM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  TBLM-OVERRIDE-FLAG      PIC X(01).
                   88  TBLM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  TBLM-SUSPEND-FLAG       PIC X(01).
                   88  TBLM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  TBLM-HOLD-FLAG          PIC X(01).
                   88  TBLM-HOLD-FLAG-YES          VALUE 'Y'.
           05  TBL-MASTER-COUNTERS.
               10  TBLM-ITEM-CNT           PIC 9(07).
               10  TBLM-EXCEPT-CNT         PIC 9(07).
               10  TBLM-NOTICE-CNT         PIC 9(07).
               10  TBLM-RETRY-CNT          PIC 9(07).
               10  TBLM-RATE               PIC 9(03)V9(06).
               10  TBLM-DISCOUNT           PIC 9(03)V9(06).
               10  TBLM-FEE-PCT            PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
