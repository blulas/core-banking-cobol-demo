      *================================================================*
      * DCLGEN TABLE(CBS.DIVIDEND_REINVESTM)                           *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLDRP))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.DIVIDEND_REINVESTM TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             DIVIDEND_AMT                 DECIMAL(15, 2) NOT NULL,
             DIVIDEND_YTD                 DECIMAL(15, 2) NOT NULL,
             REINVEST_AMT                 DECIMAL(15, 2) NOT NULL,
             REINVEST_YTD                 DECIMAL(15, 2) NOT NULL,
             CASH_PAY_AMT                 DECIMAL(15, 2) NOT NULL,
             CASH_PAY_YTD                 DECIMAL(15, 2) NOT NULL,
             SHARES_AMT_AMT               DECIMAL(15, 2) NOT NULL,
             SHARES_AMT_YTD               DECIMAL(15, 2) NOT NULL,
             WITHHOLD_AMT                 DECIMAL(15, 2) NOT NULL,
             WITHHOLD_YTD                 DECIMAL(15, 2) NOT NULL,
             FEE_AMT                      DECIMAL(15, 2) NOT NULL,
             FEE_YTD                      DECIMAL(15, 2) NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             PLAN_OPTION_CD               CHAR(1) NOT NULL,
             HOLDER_TYPE_CD               CHAR(1) NOT NULL,
             SOURCE_CD                    CHAR(2) NOT NULL,
             CHANNEL_CD                   CHAR(2) NOT NULL,
             REASON_CD                    CHAR(1) NOT NULL,
             CYCLE_CD                     CHAR(2) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.DIVIDEND_REINVESTM             *
      *================================================================*
       01  DCLDIVIDEND-REINVESTM.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-DIVIDEND-AMT              PIC S9(13)V99 COMP-3.
           10  H-DIVIDEND-YTD              PIC S9(13)V99 COMP-3.
           10  H-REINVEST-AMT              PIC S9(13)V99 COMP-3.
           10  H-REINVEST-YTD              PIC S9(13)V99 COMP-3.
           10  H-CASH-PAY-AMT              PIC S9(13)V99 COMP-3.
           10  H-CASH-PAY-YTD              PIC S9(13)V99 COMP-3.
           10  H-SHARES-AMT-AMT            PIC S9(13)V99 COMP-3.
           10  H-SHARES-AMT-YTD            PIC S9(13)V99 COMP-3.
           10  H-WITHHOLD-AMT              PIC S9(13)V99 COMP-3.
           10  H-WITHHOLD-YTD              PIC S9(13)V99 COMP-3.
           10  H-FEE-AMT                   PIC S9(13)V99 COMP-3.
           10  H-FEE-YTD                   PIC S9(13)V99 COMP-3.
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-PLAN-OPTION-CD            PIC X(01).
           10  H-HOLDER-TYPE-CD            PIC X(01).
           10  H-SOURCE-CD                 PIC X(02).
           10  H-CHANNEL-CD                PIC X(02).
           10  H-REASON-CD                 PIC X(01).
           10  H-CYCLE-CD                  PIC X(02).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      *================================================================*
