      *================================================================*
      * DCLGEN TABLE(CBS.DATE_WINDOW_BRIDGE)                           *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLYDB))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.DATE_WINDOW_BRIDGE TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             AMOUNT_1_AMT                 DECIMAL(15, 2) NOT NULL,
             AMOUNT_1_YTD                 DECIMAL(15, 2) NOT NULL,
             AMOUNT_2_AMT                 DECIMAL(15, 2) NOT NULL,
             AMOUNT_2_YTD                 DECIMAL(15, 2) NOT NULL,
             CONTROL_AMT                  DECIMAL(15, 2) NOT NULL,
             CONTROL_YTD                  DECIMAL(15, 2) NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             WINDOW_RULE_CD               CHAR(1) NOT NULL,
             PIVOT_CD                     CHAR(3) NOT NULL,
             REGION_CD                    CHAR(1) NOT NULL,
             REASON_CD                    CHAR(2) NOT NULL,
             SOURCE_CD                    CHAR(3) NOT NULL,
             CYCLE_CD                     CHAR(2) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.DATE_WINDOW_BRIDGE             *
      *================================================================*
       01  DCLDATE-WINDOW-BRIDGE.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-AMOUNT-1-AMT              PIC S9(13)V99 COMP-3.
           10  H-AMOUNT-1-YTD              PIC S9(13)V99 COMP-3.
           10  H-AMOUNT-2-AMT              PIC S9(13)V99 COMP-3.
           10  H-AMOUNT-2-YTD              PIC S9(13)V99 COMP-3.
           10  H-CONTROL-AMT               PIC S9(13)V99 COMP-3.
           10  H-CONTROL-YTD               PIC S9(13)V99 COMP-3.
           10  H-OPEN-DATE                 PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-WINDOW-RULE-CD            PIC X(01).
           10  H-PIVOT-CD                  PIC X(03).
           10  H-REGION-CD                 PIC X(01).
           10  H-REASON-CD                 PIC X(02).
           10  H-SOURCE-CD                 PIC X(03).
           10  H-CYCLE-CD                  PIC X(02).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 41      *
      *================================================================*
