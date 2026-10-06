      *================================================================*
      * COPYBOOK : MFCHST1                                             *
      * PURPOSE  : MICROFICHE STATEMENTS PRIOR VERSION RECORD LAYOUT   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-08-19 TWH  ORIGINAL                                       *
      * 1998-11-11 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-12-06 JLM  POSTAGE STATUS CODES ADDED PRB7630             *
      * 2003-04-13 MKL  POSTAGE ROUNDING - HALF UP REQ7061             *
      * 2006-03-28 PQR  RESEARCH TABLE EXPANDED PRB1418                *
      *================================================================*
       01  MFC-H1-REC.
           05  MFCH1-KEY                   PIC X(12).
           05  MFCH1-DESC                  PIC X(30).
           05  MFCH1-NAME                  PIC X(30).
           05  MFCH1-CUST-ID               PIC X(10).
           05  MFCH1-ADDR                  PIC X(30).
           05  MFCH1-CONTACT               PIC X(20).
           05  MFCH1-REF-NO                PIC X(16).
           05  MFC-H1-AMOUNTS.
               10  MFCH1-COPY-FEE-YTD      PIC S9(13)V99 COMP-3.
               10  MFCH1-COPY-FEE-AMT      PIC S9(13)V99 COMP-3.
               10  MFCH1-RESEARCH-AMT      PIC S9(13)V99 COMP-3.
               10  MFCH1-POSTAGE-YTD       PIC S9(13)V99 COMP-3.
               10  MFCH1-POSTAGE-AMT       PIC S9(13)V99 COMP-3.
           05  MFC-H1-DATES.
               10  MFCH1-NEXT-DUE-DATE     PIC 9(08).
               10  MFCH1-LAST-STMT-DATE    PIC 9(08).
               10  MFCH1-LAST-MAINT-DATE   PIC 9(08).
               10  MFCH1-LAST-PAY-DATE     PIC 9(08).
               10  MFCH1-EXPIRY-DATE       PIC 9(08).
               10  MFCH1-OPEN-DATE         PIC 9(08).
               10  MFCH1-CLOSE-DATE        PIC 9(08).
               10  MFCH1-NOTICE-DATE       PIC 9(08).
               10  MFCH1-MATURITY-DATE     PIC 9(08).
           05  MFC-H1-CODES.
               10  MFCH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  MFCH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  MFCH1-TYPE-CD           PIC X(01).
                   88  MFCH1-TYPE-CD-G             VALUE 'G'.
                   88  MFCH1-TYPE-CD-K             VALUE 'K'.
                   88  MFCH1-TYPE-CD-W             VALUE 'W'.
                   88  MFCH1-TYPE-CD-Y             VALUE 'Y'.
               10  MFCH1-STOP-FLAG         PIC X(01).
                   88  MFCH1-STOP-FLAG-YES         VALUE 'Y'.
               10  MFCH1-BRANCH-CD         PIC X(02).
                   88  MFCH1-BRANCH-CD-AT          VALUE 'AT'.
                   88  MFCH1-BRANCH-CD-KR          VALUE 'KR'.
                   88  MFCH1-BRANCH-CD-LK          VALUE 'LK'.
                   88  MFCH1-BRANCH-CD-US          VALUE 'US'.
               10  MFCH1-RETENTION-CD      PIC X(02).
                   88  MFCH1-RETENTION-CD-KT       VALUE 'KT'.
                   88  MFCH1-RETENTION-CD-SJ       VALUE 'SJ'.
                   88  MFCH1-RETENTION-CD-WV       VALUE 'WV'.
               10  MFCH1-CHANNEL-CD        PIC X(02).
               10  MFCH1-HOLD-FLAG         PIC X(01).
                   88  MFCH1-HOLD-FLAG-YES         VALUE 'Y'.
               10  MFCH1-FICHE-TYPE-CD     PIC X(02).
                   88  MFCH1-FICHE-TYPE-CD-AW      VALUE 'AW'.
                   88  MFCH1-FICHE-TYPE-CD-JM      VALUE 'JM'.
                   88  MFCH1-FICHE-TYPE-CD-KV      VALUE 'KV'.
                   88  MFCH1-FICHE-TYPE-CD-MD      VALUE 'MD'.
                   88  MFCH1-FICHE-TYPE-CD-NR      VALUE 'NR'.
               10  MFCH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  MFCH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  MFCH1-STATUS-CD         PIC X(03).
                   88  MFCH1-STATUS-CD-AST         VALUE 'AST'.
                   88  MFCH1-STATUS-CD-JXN         VALUE 'JXN'.
                   88  MFCH1-STATUS-CD-KJJ         VALUE 'KJJ'.
                   88  MFCH1-STATUS-CD-TZL         VALUE 'TZL'.
                   88  MFCH1-STATUS-CD-XZM         VALUE 'XZM'.
           05  MFC-H1-COUNTERS.
               10  MFCH1-ITEM-CNT          PIC 9(07).
               10  MFCH1-MARGIN            PIC 9(03)V9(06).
               10  MFCH1-CYCLE-CNT         PIC 9(07).
               10  MFCH1-FEE-PCT           PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
