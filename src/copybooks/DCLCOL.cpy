      *================================================================*
      * DCLGEN TABLE(CBS.COLLECTIONS_QUEUE)                            *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLCOL))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.COLLECTIONS_QUEUE TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             PAST_DUE_AMT                 DECIMAL(15, 2) NOT NULL,
             PAST_DUE_YTD                 DECIMAL(15, 2) NOT NULL,
             PROMISED_AMT                 DECIMAL(15, 2) NOT NULL,
             PROMISED_YTD                 DECIMAL(15, 2) NOT NULL,
             COLLECTED_AMT                DECIMAL(15, 2) NOT NULL,
             COLLECTED_YTD                DECIMAL(15, 2) NOT NULL,
             WRITEOFF_AMT                 DECIMAL(15, 2) NOT NULL,
             WRITEOFF_YTD                 DECIMAL(15, 2) NOT NULL,
             FEE_AMT                      DECIMAL(15, 2) NOT NULL,
             FEE_YTD                      DECIMAL(15, 2) NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             QUEUE_CD                     CHAR(2) NOT NULL,
             ACTION_CD                    CHAR(2) NOT NULL,
             CYCLE_CD                     CHAR(2) NOT NULL,
             STATUS_CD                    CHAR(2) NOT NULL,
             REASON_CD                    CHAR(2) NOT NULL,
             REGION_CD                    CHAR(2) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.COLLECTIONS_QUEUE              *
      *================================================================*
       01  DCLCOLLECTIONS-QUEUE.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-PAST-DUE-AMT              PIC S9(13)V99 COMP-3.
           10  H-PAST-DUE-YTD              PIC S9(13)V99 COMP-3.
           10  H-PROMISED-AMT              PIC S9(13)V99 COMP-3.
           10  H-PROMISED-YTD              PIC S9(13)V99 COMP-3.
           10  H-COLLECTED-AMT             PIC S9(13)V99 COMP-3.
           10  H-COLLECTED-YTD             PIC S9(13)V99 COMP-3.
           10  H-WRITEOFF-AMT              PIC S9(13)V99 COMP-3.
           10  H-WRITEOFF-YTD              PIC S9(13)V99 COMP-3.
           10  H-FEE-AMT                   PIC S9(13)V99 COMP-3.
           10  H-FEE-YTD                   PIC S9(13)V99 COMP-3.
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-QUEUE-CD                  PIC X(02).
           10  H-ACTION-CD                 PIC X(02).
           10  H-CYCLE-CD                  PIC X(02).
           10  H-STATUS-CD                 PIC X(02).
           10  H-REASON-CD                 PIC X(02).
           10  H-REGION-CD                 PIC X(02).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 45      *
      *================================================================*
