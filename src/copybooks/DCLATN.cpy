      *================================================================*
      * DCLGEN TABLE(CBS.ATM_NETWORK_SETTLE)                           *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLATN))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.ATM_NETWORK_SETTLE TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             DISPENSED_AMT                DECIMAL(15, 2) NOT NULL,
             DISPENSED_YTD                DECIMAL(15, 2) NOT NULL,
             INTERCHG_AMT                 DECIMAL(15, 2) NOT NULL,
             INTERCHG_YTD                 DECIMAL(15, 2) NOT NULL,
             SURCHG_AMT                   DECIMAL(15, 2) NOT NULL,
             SURCHG_YTD                   DECIMAL(15, 2) NOT NULL,
             SETTLE_AMT                   DECIMAL(15, 2) NOT NULL,
             SETTLE_YTD                   DECIMAL(15, 2) NOT NULL,
             ADJUST_AMT                   DECIMAL(15, 2) NOT NULL,
             ADJUST_YTD                   DECIMAL(15, 2) NOT NULL,
             CASSETTE_AMT                 DECIMAL(15, 2) NOT NULL,
             CASSETTE_YTD                 DECIMAL(15, 2) NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             NETWORK_CD                   CHAR(1) NOT NULL,
             TERMINAL_CD                  CHAR(2) NOT NULL,
             REASON_CD                    CHAR(2) NOT NULL,
             BRANCH_CD                    CHAR(2) NOT NULL,
             REGION_CD                    CHAR(1) NOT NULL,
             STATUS_CD                    CHAR(2) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.ATM_NETWORK_SETTLE             *
      *================================================================*
       01  DCLATM-NETWORK-SETTLE.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-DISPENSED-AMT             PIC S9(13)V99 COMP-3.
           10  H-DISPENSED-YTD             PIC S9(13)V99 COMP-3.
           10  H-INTERCHG-AMT              PIC S9(13)V99 COMP-3.
           10  H-INTERCHG-YTD              PIC S9(13)V99 COMP-3.
           10  H-SURCHG-AMT                PIC S9(13)V99 COMP-3.
           10  H-SURCHG-YTD                PIC S9(13)V99 COMP-3.
           10  H-SETTLE-AMT                PIC S9(13)V99 COMP-3.
           10  H-SETTLE-YTD                PIC S9(13)V99 COMP-3.
           10  H-ADJUST-AMT                PIC S9(13)V99 COMP-3.
           10  H-ADJUST-YTD                PIC S9(13)V99 COMP-3.
           10  H-CASSETTE-AMT              PIC S9(13)V99 COMP-3.
           10  H-CASSETTE-YTD              PIC S9(13)V99 COMP-3.
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-NETWORK-CD                PIC X(01).
           10  H-TERMINAL-CD               PIC X(02).
           10  H-REASON-CD                 PIC X(02).
           10  H-BRANCH-CD                 PIC X(02).
           10  H-REGION-CD                 PIC X(01).
           10  H-STATUS-CD                 PIC X(02).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      *================================================================*
