      *================================================================*
      * COPYBOOK : MFLIMP                                              *
      * PURPOSE  : LINKAGE FOR MICROFICHE STATEMENTS RULE MODULE MFLIM *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-05-26 CRP  ORIGINAL                                       *
      * 2000-12-16 EWS  PERFORMANCE - COPY-FEE READ AHEAD REQ5799      *
      * 2000-07-04 RJT  AUDIT FINDING - STORAGE CONTROL TOTALS REQ3916 *
      * 2001-08-08 CRP  COPY-FEE RERUN PROTECTION INC6500              *
      * 2006-02-17 SKN  STORAGE RERUN PROTECTION REQ8893               *
      * 2008-11-03 GAF  COPY-FEE RERUN PROTECTION INC9341              *
      *================================================================*
       01  MFLIM-PARMS.
           05  MFLIM-COPY-FEE-YTD          PIC S9(13)V99 COMP-3.
           05  MFLIM-STORAGE-AMT           PIC S9(13)V99 COMP-3.
           05  MFLIM-STORAGE-YTD           PIC S9(13)V99 COMP-3.
           05  MFLIM-RESEARCH-YTD          PIC S9(13)V99 COMP-3.
           05  MFLIM-POSTAGE-AMT           PIC S9(13)V99 COMP-3.
           05  MFLIM-POSTAGE-YTD           PIC S9(13)V99 COMP-3.
           05  MFLIM-CLOSE-DATE            PIC 9(08).
           05  MFLIM-EXPIRY-DATE           PIC 9(08).
           05  MFLIM-LAST-PAY-DATE         PIC 9(08).
           05  MFLIM-OPEN-DATE             PIC 9(08).
           05  MFLIM-TYPE-CD               PIC X(01).
               88  MFLIM-TYPE-CD-G                 VALUE 'G'.
               88  MFLIM-TYPE-CD-K                 VALUE 'K'.
               88  MFLIM-TYPE-CD-W                 VALUE 'W'.
               88  MFLIM-TYPE-CD-Y                 VALUE 'Y'.
           05  MFLIM-RETENTION-CD          PIC X(02).
               88  MFLIM-RETENTION-CD-KT           VALUE 'KT'.
               88  MFLIM-RETENTION-CD-SJ           VALUE 'SJ'.
               88  MFLIM-RETENTION-CD-WV           VALUE 'WV'.
           05  MFLIM-CHANNEL-CD            PIC X(02).
           05  MFLIM-BRANCH-CD             PIC X(02).
               88  MFLIM-BRANCH-CD-AT              VALUE 'AT'.
               88  MFLIM-BRANCH-CD-KR              VALUE 'KR'.
               88  MFLIM-BRANCH-CD-LK              VALUE 'LK'.
               88  MFLIM-BRANCH-CD-US              VALUE 'US'.
           05  MFLIM-REVIEW-FLAG           PIC X(01).
               88  MFLIM-REVIEW-FLAG-YES           VALUE 'Y'.
           05  MFLIM-STOP-FLAG             PIC X(01).
               88  MFLIM-STOP-FLAG-YES             VALUE 'Y'.
           05  MFLIM-NOTICE-SENT-FLAG      PIC X(01).
               88  MFLIM-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  MFLIM-CYCLE-CNT             PIC 9(07).
           05  MFLIM-PAYMENT-CNT           PIC 9(07).
           05  MFLIM-DISCOUNT              PIC 9(03)V9(06).
           05  MFLIM-MARGIN                PIC 9(03)V9(06).
           05  MFLIM-ADDR                  PIC X(30).
           05  MFLIM-CONTACT               PIC X(20).
           05  MFLIM-RETURN-CODE           PIC 9(02).
