      *================================================================*
      * COPYBOOK : MFCMST                                              *
      * PURPOSE  : MICROFICHE STATEMENTS MASTER RECORD (MFCMST KSDS)   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-10-14 CRP  ORIGINAL                                       *
      * 1998-11-25 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-11-12 PQR  COPY-FEE STATUS CODES ADDED INC3520            *
      * 2000-11-25 BHS  POSTAGE STATUS CODES ADDED REQ9806             *
      * 2001-12-01 SKN  STORAGE CUTOFF MOVED CHG5937                   *
      * 2001-01-21 JLM  POSTAGE CUTOFF MOVED PRB6847                   *
      * 2002-10-18 EWS  COPY-FEE NOTICE TEXT PER LEGAL PRB7652         *
      * 2003-03-21 LMB  COPY-FEE CALCULATION CORRECTED REQ9016         *
      * 2004-08-02 CRP  POSTAGE ROUNDING - HALF UP INC2968             *
      *================================================================*
       01  MFC-MASTER-REC.
           05  MFCM-KEY                    PIC X(12).
           05  MFCM-CUST-ID                PIC X(10).
           05  MFCM-NAME                   PIC X(30).
           05  MFCM-DESC                   PIC X(30).
           05  MFCM-REF-NO                 PIC X(16).
           05  MFCM-ADDR                   PIC X(30).
           05  MFCM-CONTACT                PIC X(20).
           05  MFC-MASTER-AMOUNTS.
               10  MFCM-RESEARCH-AMT       PIC S9(13)V99 COMP-3.
               10  MFCM-RESEARCH-YTD       PIC S9(13)V99 COMP-3.
               10  MFCM-COPY-FEE-AMT       PIC S9(13)V99 COMP-3.
               10  MFCM-COPY-FEE-YTD       PIC S9(13)V99 COMP-3.
               10  MFCM-POSTAGE-AMT        PIC S9(13)V99 COMP-3.
               10  MFCM-POSTAGE-YTD        PIC S9(13)V99 COMP-3.
               10  MFCM-STORAGE-AMT        PIC S9(13)V99 COMP-3.
               10  MFCM-STORAGE-YTD        PIC S9(13)V99 COMP-3.
           05  MFC-MASTER-DATES.
               10  MFCM-OPEN-DATE          PIC 9(08).
               10  MFCM-NOTICE-DATE        PIC 9(08).
               10  MFCM-NEXT-DUE-DATE      PIC 9(08).
               10  MFCM-CLOSE-DATE         PIC 9(08).
               10  MFCM-EXPIRY-DATE        PIC 9(08).
               10  MFCM-MATURITY-DATE      PIC 9(08).
               10  MFCM-LAST-STMT-DATE     PIC 9(08).
               10  MFCM-LAST-PAY-DATE      PIC 9(08).
               10  MFCM-LAST-MAINT-DATE    PIC 9(08).
           05  MFC-MASTER-CODES.
               10  MFCM-FICHE-TYPE-CD      PIC X(02).
                   88  MFCM-FICHE-TYPE-CD-AW       VALUE 'AW'.
                   88  MFCM-FICHE-TYPE-CD-JM       VALUE 'JM'.
                   88  MFCM-FICHE-TYPE-CD-KV       VALUE 'KV'.
                   88  MFCM-FICHE-TYPE-CD-MD       VALUE 'MD'.
                   88  MFCM-FICHE-TYPE-CD-NR       VALUE 'NR'.
               10  MFCM-RETENTION-CD       PIC X(02).
                   88  MFCM-RETENTION-CD-KT        VALUE 'KT'.
                   88  MFCM-RETENTION-CD-SJ        VALUE 'SJ'.
                   88  MFCM-RETENTION-CD-WV        VALUE 'WV'.
               10  MFCM-CHANNEL-CD         PIC X(02).
               10  MFCM-BRANCH-CD          PIC X(02).
                   88  MFCM-BRANCH-CD-AT           VALUE 'AT'.
                   88  MFCM-BRANCH-CD-KR           VALUE 'KR'.
                   88  MFCM-BRANCH-CD-LK           VALUE 'LK'.
                   88  MFCM-BRANCH-CD-US           VALUE 'US'.
               10  MFCM-TYPE-CD            PIC X(01).
                   88  MFCM-TYPE-CD-G              VALUE 'G'.
                   88  MFCM-TYPE-CD-K              VALUE 'K'.
                   88  MFCM-TYPE-CD-W              VALUE 'W'.
                   88  MFCM-TYPE-CD-Y              VALUE 'Y'.
               10  MFCM-STATUS-CD          PIC X(03).
                   88  MFCM-STATUS-CD-AST          VALUE 'AST'.
                   88  MFCM-STATUS-CD-JXN          VALUE 'JXN'.
                   88  MFCM-STATUS-CD-KJJ          VALUE 'KJJ'.
                   88  MFCM-STATUS-CD-TZL          VALUE 'TZL'.
                   88  MFCM-STATUS-CD-XZM          VALUE 'XZM'.
               10  MFCM-AUTO-RENEW-FLAG    PIC X(01).
                   88  MFCM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  MFCM-EXEMPT-FLAG        PIC X(01).
                   88  MFCM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  MFCM-REVIEW-FLAG        PIC X(01).
                   88  MFCM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  MFCM-NOTICE-SENT-FLAG   PIC X(01).
                   88  MFCM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  MFCM-HOLD-FLAG          PIC X(01).
                   88  MFCM-HOLD-FLAG-YES          VALUE 'Y'.
               10  MFCM-STOP-FLAG          PIC X(01).
                   88  MFCM-STOP-FLAG-YES          VALUE 'Y'.
           05  MFC-MASTER-COUNTERS.
               10  MFCM-CYCLE-CNT          PIC 9(07).
               10  MFCM-PAYMENT-CNT        PIC 9(07).
               10  MFCM-NOTICE-CNT         PIC 9(07).
               10  MFCM-ITEM-CNT           PIC 9(07).
               10  MFCM-MARGIN             PIC 9(03)V9(06).
               10  MFCM-DISCOUNT           PIC 9(03)V9(06).
               10  MFCM-FEE-PCT            PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
