      *================================================================*
      * COPYBOOK : MFCOLD2                                             *
      * PURPOSE  : MICROFICHE STATEMENTS HISTORY RECORD LAYOUT         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-07-03 EWS  ORIGINAL                                       *
      * 1997-01-04 LMB  PERFORMANCE - COPY-FEE READ AHEAD REQ9558      *
      * 1999-11-04 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-02-09 MKL  RESEARCH EXCEPTIONS TO OPS QUEUE PRB6266       *
      * 2003-09-17 LMB  COPY-FEE CALCULATION CORRECTED CHG3861         *
      * 2003-01-18 MKL  COPY-FEE CUTOFF MOVED INC4598                  *
      * 2004-12-22 EWS  COPY-FEE CUTOFF MOVED INC9631                  *
      * 2004-05-01 RJT  NEW RESEARCH REPORT COLUMNS REQ1560            *
      * 2005-09-11 JLM  COPY-FEE NOTICE TEXT PER LEGAL REQ2274         *
      *================================================================*
       01  MFC-H2-REC.
           05  MFCH2-CONTACT               PIC X(20).
           05  MFCH2-KEY                   PIC X(12).
           05  MFCH2-ADDR                  PIC X(30).
           05  MFCH2-NAME                  PIC X(30).
           05  MFCH2-CUST-ID               PIC X(10).
           05  MFCH2-REF-NO                PIC X(16).
           05  MFC-H2-AMOUNTS.
               10  MFCH2-RESEARCH-YTD      PIC S9(13)V99 COMP-3.
               10  MFCH2-STORAGE-YTD       PIC S9(13)V99 COMP-3.
               10  MFCH2-POSTAGE-YTD       PIC S9(13)V99 COMP-3.
               10  MFCH2-COPY-FEE-AMT      PIC S9(13)V99 COMP-3.
               10  MFCH2-RESEARCH-AMT      PIC S9(13)V99 COMP-3.
               10  MFCH2-STORAGE-AMT       PIC S9(13)V99 COMP-3.
               10  MFCH2-COPY-FEE-YTD      PIC S9(13)V99 COMP-3.
           05  MFC-H2-DATES.
               10  MFCH2-NOTICE-DATE       PIC 9(08).
               10  MFCH2-NEXT-DUE-DATE     PIC 9(08).
               10  MFCH2-LAST-PAY-DATE     PIC 9(08).
               10  MFCH2-OPEN-DATE         PIC 9(08).
               10  MFCH2-CLOSE-DATE        PIC 9(08).
               10  MFCH2-MATURITY-DATE     PIC 9(08).
               10  MFCH2-LAST-STMT-DATE    PIC 9(08).
               10  MFCH2-LAST-MAINT-DATE   PIC 9(08).
           05  MFC-H2-CODES.
               10  MFCH2-AUTO-RENEW-FLAG   PIC X(01).
                   88  MFCH2-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  MFCH2-NOTICE-SENT-FLAG  PIC X(01).
                   88  MFCH2-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  MFCH2-CHANNEL-CD        PIC X(02).
               10  MFCH2-HOLD-FLAG         PIC X(01).
                   88  MFCH2-HOLD-FLAG-YES         VALUE 'Y'.
               10  MFCH2-REVIEW-FLAG       PIC X(01).
                   88  MFCH2-REVIEW-FLAG-YES       VALUE 'Y'.
               10  MFCH2-BRANCH-CD         PIC X(02).
                   88  MFCH2-BRANCH-CD-AT          VALUE 'AT'.
                   88  MFCH2-BRANCH-CD-KR          VALUE 'KR'.
                   88  MFCH2-BRANCH-CD-LK          VALUE 'LK'.
                   88  MFCH2-BRANCH-CD-US          VALUE 'US'.
               10  MFCH2-FICHE-TYPE-CD     PIC X(02).
                   88  MFCH2-FICHE-TYPE-CD-AW      VALUE 'AW'.
                   88  MFCH2-FICHE-TYPE-CD-JM      VALUE 'JM'.
                   88  MFCH2-FICHE-TYPE-CD-KV      VALUE 'KV'.
                   88  MFCH2-FICHE-TYPE-CD-MD      VALUE 'MD'.
                   88  MFCH2-FICHE-TYPE-CD-NR      VALUE 'NR'.
               10  MFCH2-TYPE-CD           PIC X(01).
                   88  MFCH2-TYPE-CD-G             VALUE 'G'.
                   88  MFCH2-TYPE-CD-K             VALUE 'K'.
                   88  MFCH2-TYPE-CD-W             VALUE 'W'.
                   88  MFCH2-TYPE-CD-Y             VALUE 'Y'.
               10  MFCH2-RETENTION-CD      PIC X(02).
                   88  MFCH2-RETENTION-CD-KT       VALUE 'KT'.
                   88  MFCH2-RETENTION-CD-SJ       VALUE 'SJ'.
                   88  MFCH2-RETENTION-CD-WV       VALUE 'WV'.
               10  MFCH2-EXEMPT-FLAG       PIC X(01).
                   88  MFCH2-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  MFCH2-STATUS-CD         PIC X(03).
                   88  MFCH2-STATUS-CD-AST         VALUE 'AST'.
                   88  MFCH2-STATUS-CD-JXN         VALUE 'JXN'.
                   88  MFCH2-STATUS-CD-KJJ         VALUE 'KJJ'.
                   88  MFCH2-STATUS-CD-TZL         VALUE 'TZL'.
                   88  MFCH2-STATUS-CD-XZM         VALUE 'XZM'.
               10  MFCH2-STOP-FLAG         PIC X(01).
                   88  MFCH2-STOP-FLAG-YES         VALUE 'Y'.
           05  MFC-H2-COUNTERS.
               10  MFCH2-MARGIN            PIC 9(03)V9(06).
               10  MFCH2-NOTICE-CNT        PIC 9(07).
               10  MFCH2-PAYMENT-CNT       PIC 9(07).
               10  MFCH2-DISCOUNT          PIC 9(03)V9(06).
               10  MFCH2-ITEM-CNT          PIC 9(07).
               10  MFCH2-CYCLE-CNT         PIC 9(07).
               10  MFCH2-FEE-PCT           PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
