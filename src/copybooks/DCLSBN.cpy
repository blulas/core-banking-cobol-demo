      *================================================================*
      * DCLGEN TABLE(CBS.SAVINGS_BOND_SERVI)                           *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLSBN))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.SAVINGS_BOND_SERVI TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             FACE_AMT                     DECIMAL(15, 2) NOT NULL,
             FACE_YTD                     DECIMAL(15, 2) NOT NULL,
             REDEEM_AMT                   DECIMAL(15, 2) NOT NULL,
             REDEEM_YTD                   DECIMAL(15, 2) NOT NULL,
             INTEREST_AMT                 DECIMAL(15, 2) NOT NULL,
             INTEREST_YTD                 DECIMAL(15, 2) NOT NULL,
             PURCHASE_AMT                 DECIMAL(15, 2) NOT NULL,
             PURCHASE_YTD                 DECIMAL(15, 2) NOT NULL,
             ACCRUAL_AMT                  DECIMAL(15, 2) NOT NULL,
             ACCRUAL_YTD                  DECIMAL(15, 2) NOT NULL,
             TAXABLE_AMT                  DECIMAL(15, 2) NOT NULL,
             TAXABLE_YTD                  DECIMAL(15, 2) NOT NULL,
             ISSUE_PRICE_AMT              DECIMAL(15, 2) NOT NULL,
             ISSUE_PRICE_YTD              DECIMAL(15, 2) NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             SERIES_CD                    CHAR(2) NOT NULL,
             ISSUE_TYPE_CD                CHAR(2) NOT NULL,
             SOURCE_CD                    CHAR(1) NOT NULL,
             STATUS_CD                    CHAR(2) NOT NULL,
             REASON_CD                    CHAR(2) NOT NULL,
             BRANCH_CD                    CHAR(2) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.SAVINGS_BOND_SERVI             *
      *================================================================*
       01  DCLSAVINGS-BOND-SERVI.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-FACE-AMT                  PIC S9(13)V99 COMP-3.
           10  H-FACE-YTD                  PIC S9(13)V99 COMP-3.
           10  H-REDEEM-AMT                PIC S9(13)V99 COMP-3.
           10  H-REDEEM-YTD                PIC S9(13)V99 COMP-3.
           10  H-INTEREST-AMT              PIC S9(13)V99 COMP-3.
           10  H-INTEREST-YTD              PIC S9(13)V99 COMP-3.
           10  H-PURCHASE-AMT              PIC S9(13)V99 COMP-3.
           10  H-PURCHASE-YTD              PIC S9(13)V99 COMP-3.
           10  H-ACCRUAL-AMT               PIC S9(13)V99 COMP-3.
           10  H-ACCRUAL-YTD               PIC S9(13)V99 COMP-3.
           10  H-TAXABLE-AMT               PIC S9(13)V99 COMP-3.
           10  H-TAXABLE-YTD               PIC S9(13)V99 COMP-3.
           10  H-ISSUE-PRICE-AMT           PIC S9(13)V99 COMP-3.
           10  H-ISSUE-PRICE-YTD           PIC S9(13)V99 COMP-3.
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-SERIES-CD                 PIC X(02).
           10  H-ISSUE-TYPE-CD             PIC X(02).
           10  H-SOURCE-CD                 PIC X(01).
           10  H-STATUS-CD                 PIC X(02).
           10  H-REASON-CD                 PIC X(02).
           10  H-BRANCH-CD                 PIC X(02).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 49      *
      *================================================================*
