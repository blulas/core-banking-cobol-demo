      *================================================================*
      * COPYBOOK : MFRISKP                                             *
      * PURPOSE  : LINKAGE FOR MICROFICHE STATEMENTS RULE MODULE MFRISK*
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-12-26 TWH  ORIGINAL                                       *
      * 1997-01-22 DPS  RESEARCH NOTICE TEXT PER LEGAL CHG6687         *
      * 1998-08-15 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-01-08 GAF  COPY-FEE CUTOFF MOVED CHG0868                  *
      * 2000-04-23 CRP  COPY-FEE ROUNDING - HALF UP INC2290            *
      * 2000-10-21 SKN  COPY-FEE STATUS CODES ADDED INC6305            *
      * 2002-08-21 JLM  STORAGE TABLE EXPANDED PRB5602                 *
      * 2004-01-19 GAF  AUDIT FINDING - POSTAGE CONTROL TOTALS CHG5638 *
      * 2004-07-24 CRP  POSTAGE TABLE EXPANDED REQ6455                 *
      * 2006-09-05 SKN  RESEARCH FEE SCHEDULE UPDATE INC1290           *
      * 2008-04-12 EWS  RESEARCH TABLE EXPANDED PRB4618                *
      *================================================================*
       01  MFRISK-PARMS.
           05  MFRISK-COPY-FEE-YTD         PIC S9(13)V99 COMP-3.
           05  MFRISK-COPY-FEE-AMT         PIC S9(13)V99 COMP-3.
           05  MFRISK-RESEARCH-YTD         PIC S9(13)V99 COMP-3.
           05  MFRISK-RESEARCH-AMT         PIC S9(13)V99 COMP-3.
           05  MFRISK-STORAGE-AMT          PIC S9(13)V99 COMP-3.
           05  MFRISK-STORAGE-YTD          PIC S9(13)V99 COMP-3.
           05  MFRISK-NOTICE-DATE          PIC 9(08).
           05  MFRISK-LAST-PAY-DATE        PIC 9(08).
           05  MFRISK-OPEN-DATE            PIC 9(08).
           05  MFRISK-LAST-MAINT-DATE      PIC 9(08).
           05  MFRISK-CHANNEL-CD           PIC X(02).
           05  MFRISK-BRANCH-CD            PIC X(02).
               88  MFRISK-BRANCH-CD-AT             VALUE 'AT'.
               88  MFRISK-BRANCH-CD-KR             VALUE 'KR'.
               88  MFRISK-BRANCH-CD-LK             VALUE 'LK'.
               88  MFRISK-BRANCH-CD-US             VALUE 'US'.
           05  MFRISK-RETENTION-CD         PIC X(02).
               88  MFRISK-RETENTION-CD-KT          VALUE 'KT'.
               88  MFRISK-RETENTION-CD-SJ          VALUE 'SJ'.
               88  MFRISK-RETENTION-CD-WV          VALUE 'WV'.
           05  MFRISK-STATUS-CD            PIC X(03).
               88  MFRISK-STATUS-CD-AST            VALUE 'AST'.
               88  MFRISK-STATUS-CD-JXN            VALUE 'JXN'.
               88  MFRISK-STATUS-CD-KJJ            VALUE 'KJJ'.
               88  MFRISK-STATUS-CD-TZL            VALUE 'TZL'.
               88  MFRISK-STATUS-CD-XZM            VALUE 'XZM'.
           05  MFRISK-REVIEW-FLAG          PIC X(01).
               88  MFRISK-REVIEW-FLAG-YES          VALUE 'Y'.
           05  MFRISK-AUTO-RENEW-FLAG      PIC X(01).
               88  MFRISK-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  MFRISK-STOP-FLAG            PIC X(01).
               88  MFRISK-STOP-FLAG-YES            VALUE 'Y'.
           05  MFRISK-CYCLE-CNT            PIC 9(07).
           05  MFRISK-NOTICE-CNT           PIC 9(07).
           05  MFRISK-MARGIN               PIC 9(03)V9(06).
           05  MFRISK-FEE-PCT              PIC 9(03)V9(06).
           05  MFRISK-NAME                 PIC X(30).
           05  MFRISK-ADDR                 PIC X(30).
           05  MFRISK-RETURN-CODE          PIC 9(02).
