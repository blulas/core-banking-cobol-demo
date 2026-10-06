      *================================================================*
      * DCLGEN TABLE(CBS.MICROFICHE_STATEME)                           *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLMFC))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.MICROFICHE_STATEME TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             RESEARCH_AMT                 DECIMAL(15, 2) NOT NULL,
             RESEARCH_YTD                 DECIMAL(15, 2) NOT NULL,
             COPY_FEE_AMT                 DECIMAL(15, 2) NOT NULL,
             COPY_FEE_YTD                 DECIMAL(15, 2) NOT NULL,
             POSTAGE_AMT                  DECIMAL(15, 2) NOT NULL,
             POSTAGE_YTD                  DECIMAL(15, 2) NOT NULL,
             STORAGE_AMT                  DECIMAL(15, 2) NOT NULL,
             STORAGE_YTD                  DECIMAL(15, 2) NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             FICHE_TYPE_CD                CHAR(2) NOT NULL,
             RETENTION_CD                 CHAR(2) NOT NULL,
             CHANNEL_CD                   CHAR(2) NOT NULL,
             BRANCH_CD                    CHAR(2) NOT NULL,
             TYPE_CD                      CHAR(1) NOT NULL,
             STATUS_CD                    CHAR(3) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.MICROFICHE_STATEME             *
      *================================================================*
       01  DCLMICROFICHE-STATEME.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-RESEARCH-AMT              PIC S9(13)V99 COMP-3.
           10  H-RESEARCH-YTD              PIC S9(13)V99 COMP-3.
           10  H-COPY-FEE-AMT              PIC S9(13)V99 COMP-3.
           10  H-COPY-FEE-YTD              PIC S9(13)V99 COMP-3.
           10  H-POSTAGE-AMT               PIC S9(13)V99 COMP-3.
           10  H-POSTAGE-YTD               PIC S9(13)V99 COMP-3.
           10  H-STORAGE-AMT               PIC S9(13)V99 COMP-3.
           10  H-STORAGE-YTD               PIC S9(13)V99 COMP-3.
           10  H-OPEN-DATE                 PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-FICHE-TYPE-CD             PIC X(02).
           10  H-RETENTION-CD              PIC X(02).
           10  H-CHANNEL-CD                PIC X(02).
           10  H-BRANCH-CD                 PIC X(02).
           10  H-TYPE-CD                   PIC X(01).
           10  H-STATUS-CD                 PIC X(03).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 43      *
      *================================================================*
