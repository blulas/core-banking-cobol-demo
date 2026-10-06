      *================================================================*
      * DCLGEN TABLE(CBS.SAFE_DEPOSIT_BOXES)                           *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLSDB))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.SAFE_DEPOSIT_BOXES TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             RENT_AMT                     DECIMAL(15, 2) NOT NULL,
             RENT_YTD                     DECIMAL(15, 2) NOT NULL,
             DEPOSIT_AMT                  DECIMAL(15, 2) NOT NULL,
             DEPOSIT_YTD                  DECIMAL(15, 2) NOT NULL,
             DRILL_AMT                    DECIMAL(15, 2) NOT NULL,
             DRILL_YTD                    DECIMAL(15, 2) NOT NULL,
             LATE_FEE_AMT                 DECIMAL(15, 2) NOT NULL,
             LATE_FEE_YTD                 DECIMAL(15, 2) NOT NULL,
             KEY_FEE_AMT                  DECIMAL(15, 2) NOT NULL,
             KEY_FEE_YTD                  DECIMAL(15, 2) NOT NULL,
             PREPAID_AMT                  DECIMAL(15, 2) NOT NULL,
             PREPAID_YTD                  DECIMAL(15, 2) NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             BOX_SIZE_CD                  CHAR(2) NOT NULL,
             VAULT_CD                     CHAR(2) NOT NULL,
             BRANCH_CD                    CHAR(3) NOT NULL,
             REASON_CD                    CHAR(1) NOT NULL,
             CYCLE_CD                     CHAR(2) NOT NULL,
             SOURCE_CD                    CHAR(2) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.SAFE_DEPOSIT_BOXES             *
      *================================================================*
       01  DCLSAFE-DEPOSIT-BOXES.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-RENT-AMT                  PIC S9(13)V99 COMP-3.
           10  H-RENT-YTD                  PIC S9(13)V99 COMP-3.
           10  H-DEPOSIT-AMT               PIC S9(13)V99 COMP-3.
           10  H-DEPOSIT-YTD               PIC S9(13)V99 COMP-3.
           10  H-DRILL-AMT                 PIC S9(13)V99 COMP-3.
           10  H-DRILL-YTD                 PIC S9(13)V99 COMP-3.
           10  H-LATE-FEE-AMT              PIC S9(13)V99 COMP-3.
           10  H-LATE-FEE-YTD              PIC S9(13)V99 COMP-3.
           10  H-KEY-FEE-AMT               PIC S9(13)V99 COMP-3.
           10  H-KEY-FEE-YTD               PIC S9(13)V99 COMP-3.
           10  H-PREPAID-AMT               PIC S9(13)V99 COMP-3.
           10  H-PREPAID-YTD               PIC S9(13)V99 COMP-3.
           10  H-NOTICE-DATE               PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-BOX-SIZE-CD               PIC X(02).
           10  H-VAULT-CD                  PIC X(02).
           10  H-BRANCH-CD                 PIC X(03).
           10  H-REASON-CD                 PIC X(01).
           10  H-CYCLE-CD                  PIC X(02).
           10  H-SOURCE-CD                 PIC X(02).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      *================================================================*
