      *================================================================*
      * DCLGEN TABLE(CBS.TELLER_BALANCING)                             *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLTBL))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.TELLER_BALANCING TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             CASH_IN_AMT                  DECIMAL(15, 2) NOT NULL,
             CASH_IN_YTD                  DECIMAL(15, 2) NOT NULL,
             CASH_OUT_AMT                 DECIMAL(15, 2) NOT NULL,
             CASH_OUT_YTD                 DECIMAL(15, 2) NOT NULL,
             OVERSHORT_AMT                DECIMAL(15, 2) NOT NULL,
             OVERSHORT_YTD                DECIMAL(15, 2) NOT NULL,
             VAULT_BUY_AMT                DECIMAL(15, 2) NOT NULL,
             VAULT_BUY_YTD                DECIMAL(15, 2) NOT NULL,
             VAULT_SELL_AMT               DECIMAL(15, 2) NOT NULL,
             VAULT_SELL_YTD               DECIMAL(15, 2) NOT NULL,
             BATCH_AMT                    DECIMAL(15, 2) NOT NULL,
             BATCH_YTD                    DECIMAL(15, 2) NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             DRAWER_CD                    CHAR(1) NOT NULL,
             PROOF_TYPE_CD                CHAR(2) NOT NULL,
             CHANNEL_CD                   CHAR(1) NOT NULL,
             SOURCE_CD                    CHAR(2) NOT NULL,
             CYCLE_CD                     CHAR(2) NOT NULL,
             REASON_CD                    CHAR(2) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.TELLER_BALANCING               *
      *================================================================*
       01  DCLTELLER-BALANCING.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-CASH-IN-AMT               PIC S9(13)V99 COMP-3.
           10  H-CASH-IN-YTD               PIC S9(13)V99 COMP-3.
           10  H-CASH-OUT-AMT              PIC S9(13)V99 COMP-3.
           10  H-CASH-OUT-YTD              PIC S9(13)V99 COMP-3.
           10  H-OVERSHORT-AMT             PIC S9(13)V99 COMP-3.
           10  H-OVERSHORT-YTD             PIC S9(13)V99 COMP-3.
           10  H-VAULT-BUY-AMT             PIC S9(13)V99 COMP-3.
           10  H-VAULT-BUY-YTD             PIC S9(13)V99 COMP-3.
           10  H-VAULT-SELL-AMT            PIC S9(13)V99 COMP-3.
           10  H-VAULT-SELL-YTD            PIC S9(13)V99 COMP-3.
           10  H-BATCH-AMT                 PIC S9(13)V99 COMP-3.
           10  H-BATCH-YTD                 PIC S9(13)V99 COMP-3.
           10  H-OPEN-DATE                 PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-DRAWER-CD                 PIC X(01).
           10  H-PROOF-TYPE-CD             PIC X(02).
           10  H-CHANNEL-CD                PIC X(01).
           10  H-SOURCE-CD                 PIC X(02).
           10  H-CYCLE-CD                  PIC X(02).
           10  H-REASON-CD                 PIC X(02).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      *================================================================*
