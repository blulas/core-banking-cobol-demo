      *================================================================*
      * DCLGEN TABLE(CBS.PROPRIETARY_CREDIT)                           *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLCRC))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.PROPRIETARY_CREDIT TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             LIMIT_AMT                    DECIMAL(15, 2) NOT NULL,
             LIMIT_YTD                    DECIMAL(15, 2) NOT NULL,
             PURCHASE_AMT                 DECIMAL(15, 2) NOT NULL,
             PURCHASE_YTD                 DECIMAL(15, 2) NOT NULL,
             CASH_ADV_AMT                 DECIMAL(15, 2) NOT NULL,
             CASH_ADV_YTD                 DECIMAL(15, 2) NOT NULL,
             FINANCE_AMT                  DECIMAL(15, 2) NOT NULL,
             FINANCE_YTD                  DECIMAL(15, 2) NOT NULL,
             MIN_DUE_AMT                  DECIMAL(15, 2) NOT NULL,
             MIN_DUE_YTD                  DECIMAL(15, 2) NOT NULL,
             LATE_FEE_AMT                 DECIMAL(15, 2) NOT NULL,
             LATE_FEE_YTD                 DECIMAL(15, 2) NOT NULL,
             PAYMENT_AMT                  DECIMAL(15, 2) NOT NULL,
             PAYMENT_YTD                  DECIMAL(15, 2) NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             CARD_TYPE_CD                 CHAR(3) NOT NULL,
             BILL_CYCLE_CD                CHAR(1) NOT NULL,
             TYPE_CD                      CHAR(2) NOT NULL,
             SOURCE_CD                    CHAR(1) NOT NULL,
             STATUS_CD                    CHAR(3) NOT NULL,
             BRANCH_CD                    CHAR(3) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.PROPRIETARY_CREDIT             *
      *================================================================*
       01  DCLPROPRIETARY-CREDIT.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-LIMIT-AMT                 PIC S9(13)V99 COMP-3.
           10  H-LIMIT-YTD                 PIC S9(13)V99 COMP-3.
           10  H-PURCHASE-AMT              PIC S9(13)V99 COMP-3.
           10  H-PURCHASE-YTD              PIC S9(13)V99 COMP-3.
           10  H-CASH-ADV-AMT              PIC S9(13)V99 COMP-3.
           10  H-CASH-ADV-YTD              PIC S9(13)V99 COMP-3.
           10  H-FINANCE-AMT               PIC S9(13)V99 COMP-3.
           10  H-FINANCE-YTD               PIC S9(13)V99 COMP-3.
           10  H-MIN-DUE-AMT               PIC S9(13)V99 COMP-3.
           10  H-MIN-DUE-YTD               PIC S9(13)V99 COMP-3.
           10  H-LATE-FEE-AMT              PIC S9(13)V99 COMP-3.
           10  H-LATE-FEE-YTD              PIC S9(13)V99 COMP-3.
           10  H-PAYMENT-AMT               PIC S9(13)V99 COMP-3.
           10  H-PAYMENT-YTD               PIC S9(13)V99 COMP-3.
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-CARD-TYPE-CD              PIC X(03).
           10  H-BILL-CYCLE-CD             PIC X(01).
           10  H-TYPE-CD                   PIC X(02).
           10  H-SOURCE-CD                 PIC X(01).
           10  H-STATUS-CD                 PIC X(03).
           10  H-BRANCH-CD                 PIC X(03).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 49      *
      *================================================================*
