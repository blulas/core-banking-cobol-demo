      *================================================================*
      * COPYBOOK : MFSTATP                                             *
      * PURPOSE  : LINKAGE FOR MICROFICHE STATEMENTS RULE MODULE MFSTAT*
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-02-06 LMB  ORIGINAL                                       *
      * 2000-09-20 MKL  NEW POSTAGE REPORT COLUMNS CHG5834             *
      * 2002-10-10 RJT  COPY-FEE TABLE EXPANDED INC7979                *
      * 2002-09-16 DPS  NEW STORAGE REPORT COLUMNS CHG2556             *
      * 2003-05-08 TWH  STORAGE CALCULATION CORRECTED PRB2593          *
      * 2004-07-27 CRP  POSTAGE TABLE EXPANDED PRB3559                 *
      * 2005-11-06 LMB  NEW COPY-FEE REPORT COLUMNS PRB1661            *
      * 2005-11-21 TWH  COPY-FEE RERUN PROTECTION PRB1775              *
      * 2005-02-01 SKN  PERFORMANCE - POSTAGE READ AHEAD PRB9334       *
      *================================================================*
       01  MFSTAT-PARMS.
           05  MFSTAT-COPY-FEE-YTD         PIC S9(13)V99 COMP-3.
           05  MFSTAT-STORAGE-YTD          PIC S9(13)V99 COMP-3.
           05  MFSTAT-RESEARCH-YTD         PIC S9(13)V99 COMP-3.
           05  MFSTAT-STORAGE-AMT          PIC S9(13)V99 COMP-3.
           05  MFSTAT-POSTAGE-AMT          PIC S9(13)V99 COMP-3.
           05  MFSTAT-POSTAGE-YTD          PIC S9(13)V99 COMP-3.
           05  MFSTAT-CLOSE-DATE           PIC 9(08).
           05  MFSTAT-LAST-PAY-DATE        PIC 9(08).
           05  MFSTAT-OPEN-DATE            PIC 9(08).
           05  MFSTAT-NEXT-DUE-DATE        PIC 9(08).
           05  MFSTAT-RETENTION-CD         PIC X(02).
               88  MFSTAT-RETENTION-CD-KT          VALUE 'KT'.
               88  MFSTAT-RETENTION-CD-SJ          VALUE 'SJ'.
               88  MFSTAT-RETENTION-CD-WV          VALUE 'WV'.
           05  MFSTAT-CHANNEL-CD           PIC X(02).
           05  MFSTAT-STATUS-CD            PIC X(03).
               88  MFSTAT-STATUS-CD-AST            VALUE 'AST'.
               88  MFSTAT-STATUS-CD-JXN            VALUE 'JXN'.
               88  MFSTAT-STATUS-CD-KJJ            VALUE 'KJJ'.
               88  MFSTAT-STATUS-CD-TZL            VALUE 'TZL'.
               88  MFSTAT-STATUS-CD-XZM            VALUE 'XZM'.
           05  MFSTAT-BRANCH-CD            PIC X(02).
               88  MFSTAT-BRANCH-CD-AT             VALUE 'AT'.
               88  MFSTAT-BRANCH-CD-KR             VALUE 'KR'.
               88  MFSTAT-BRANCH-CD-LK             VALUE 'LK'.
               88  MFSTAT-BRANCH-CD-US             VALUE 'US'.
           05  MFSTAT-AUTO-RENEW-FLAG      PIC X(01).
               88  MFSTAT-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  MFSTAT-NOTICE-SENT-FLAG     PIC X(01).
               88  MFSTAT-NOTICE-SENT-FLAG-YES     VALUE 'Y'.
           05  MFSTAT-EXEMPT-FLAG          PIC X(01).
               88  MFSTAT-EXEMPT-FLAG-YES          VALUE 'Y'.
           05  MFSTAT-CYCLE-CNT            PIC 9(07).
           05  MFSTAT-ITEM-CNT             PIC 9(07).
           05  MFSTAT-MARGIN               PIC 9(03)V9(06).
           05  MFSTAT-DISCOUNT             PIC 9(03)V9(06).
           05  MFSTAT-CONTACT              PIC X(20).
           05  MFSTAT-REF-NO               PIC X(16).
           05  MFSTAT-RETURN-CODE          PIC 9(02).
