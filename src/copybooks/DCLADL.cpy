      *================================================================*
      * DCLGEN TABLE(CBS.DEALER_AUTO_FINANC)                           *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLADL))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.DEALER_AUTO_FINANC TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             AMOUNT_FIN_AMT               DECIMAL(15, 2) NOT NULL,
             AMOUNT_FIN_YTD               DECIMAL(15, 2) NOT NULL,
             RESERVE_AMT                  DECIMAL(15, 2) NOT NULL,
             RESERVE_YTD                  DECIMAL(15, 2) NOT NULL,
             CHARGEBK_AMT                 DECIMAL(15, 2) NOT NULL,
             CHARGEBK_YTD                 DECIMAL(15, 2) NOT NULL,
             REBATE_AMT                   DECIMAL(15, 2) NOT NULL,
             REBATE_YTD                   DECIMAL(15, 2) NOT NULL,
             PAYMENT_AMT                  DECIMAL(15, 2) NOT NULL,
             PAYMENT_YTD                  DECIMAL(15, 2) NOT NULL,
             GAP_AMT                      DECIMAL(15, 2) NOT NULL,
             GAP_YTD                      DECIMAL(15, 2) NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             DEALER_CD                    CHAR(3) NOT NULL,
             TIER_CD                      CHAR(2) NOT NULL,
             STATUS_CD                    CHAR(3) NOT NULL,
             TYPE_CD                      CHAR(1) NOT NULL,
             REASON_CD                    CHAR(3) NOT NULL,
             CYCLE_CD                     CHAR(1) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.DEALER_AUTO_FINANC             *
      *================================================================*
       01  DCLDEALER-AUTO-FINANC.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-AMOUNT-FIN-AMT            PIC S9(13)V99 COMP-3.
           10  H-AMOUNT-FIN-YTD            PIC S9(13)V99 COMP-3.
           10  H-RESERVE-AMT               PIC S9(13)V99 COMP-3.
           10  H-RESERVE-YTD               PIC S9(13)V99 COMP-3.
           10  H-CHARGEBK-AMT              PIC S9(13)V99 COMP-3.
           10  H-CHARGEBK-YTD              PIC S9(13)V99 COMP-3.
           10  H-REBATE-AMT                PIC S9(13)V99 COMP-3.
           10  H-REBATE-YTD                PIC S9(13)V99 COMP-3.
           10  H-PAYMENT-AMT               PIC S9(13)V99 COMP-3.
           10  H-PAYMENT-YTD               PIC S9(13)V99 COMP-3.
           10  H-GAP-AMT                   PIC S9(13)V99 COMP-3.
           10  H-GAP-YTD                   PIC S9(13)V99 COMP-3.
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-DEALER-CD                 PIC X(03).
           10  H-TIER-CD                   PIC X(02).
           10  H-STATUS-CD                 PIC X(03).
           10  H-TYPE-CD                   PIC X(01).
           10  H-REASON-CD                 PIC X(03).
           10  H-CYCLE-CD                  PIC X(01).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      *================================================================*
