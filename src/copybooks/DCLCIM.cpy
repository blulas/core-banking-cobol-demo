      *================================================================*
      * DCLGEN TABLE(CBS.CHECK_IMAGE_ARCHIV)                           *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLCIM))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.CHECK_IMAGE_ARCHIV TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             ITEM_AMT                     DECIMAL(15, 2) NOT NULL,
             ITEM_YTD                     DECIMAL(15, 2) NOT NULL,
             RETRIEVE_FEE_AMT             DECIMAL(15, 2) NOT NULL,
             RETRIEVE_FEE_YTD             DECIMAL(15, 2) NOT NULL,
             IMAGE_FEE_AMT                DECIMAL(15, 2) NOT NULL,
             IMAGE_FEE_YTD                DECIMAL(15, 2) NOT NULL,
             STORAGE_AMT                  DECIMAL(15, 2) NOT NULL,
             STORAGE_YTD                  DECIMAL(15, 2) NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             CAPTURE_SITE_CD              CHAR(2) NOT NULL,
             ITEM_TYPE_CD                 CHAR(2) NOT NULL,
             TYPE_CD                      CHAR(2) NOT NULL,
             BRANCH_CD                    CHAR(2) NOT NULL,
             STATUS_CD                    CHAR(2) NOT NULL,
             CHANNEL_CD                   CHAR(2) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.CHECK_IMAGE_ARCHIV             *
      *================================================================*
       01  DCLCHECK-IMAGE-ARCHIV.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-ITEM-AMT                  PIC S9(13)V99 COMP-3.
           10  H-ITEM-YTD                  PIC S9(13)V99 COMP-3.
           10  H-RETRIEVE-FEE-AMT          PIC S9(13)V99 COMP-3.
           10  H-RETRIEVE-FEE-YTD          PIC S9(13)V99 COMP-3.
           10  H-IMAGE-FEE-AMT             PIC S9(13)V99 COMP-3.
           10  H-IMAGE-FEE-YTD             PIC S9(13)V99 COMP-3.
           10  H-STORAGE-AMT               PIC S9(13)V99 COMP-3.
           10  H-STORAGE-YTD               PIC S9(13)V99 COMP-3.
           10  H-MATURITY-DATE             PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-CAPTURE-SITE-CD           PIC X(02).
           10  H-ITEM-TYPE-CD              PIC X(02).
           10  H-TYPE-CD                   PIC X(02).
           10  H-BRANCH-CD                 PIC X(02).
           10  H-STATUS-CD                 PIC X(02).
           10  H-CHANNEL-CD                PIC X(02).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 43      *
      *================================================================*
