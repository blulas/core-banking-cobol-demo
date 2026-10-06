      *================================================================*
      * DCLGEN TABLE(CBS.LOCKBOX_REMITTANCE)                           *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLLBX))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.LOCKBOX_REMITTANCE TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             REMIT_AMT                    DECIMAL(15, 2) NOT NULL,
             REMIT_YTD                    DECIMAL(15, 2) NOT NULL,
             CHECK_AMT                    DECIMAL(15, 2) NOT NULL,
             CHECK_YTD                    DECIMAL(15, 2) NOT NULL,
             CASH_ON_HAND_AMT             DECIMAL(15, 2) NOT NULL,
             CASH_ON_HAND_YTD             DECIMAL(15, 2) NOT NULL,
             FLOAT_AMT                    DECIMAL(15, 2) NOT NULL,
             FLOAT_YTD                    DECIMAL(15, 2) NOT NULL,
             DEPOSIT_AMT                  DECIMAL(15, 2) NOT NULL,
             DEPOSIT_YTD                  DECIMAL(15, 2) NOT NULL,
             FEE_AMT                      DECIMAL(15, 2) NOT NULL,
             FEE_YTD                      DECIMAL(15, 2) NOT NULL,
             EXCEPT_AMT                   DECIMAL(15, 2) NOT NULL,
             EXCEPT_YTD                   DECIMAL(15, 2) NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             LOCKBOX_CD                   CHAR(2) NOT NULL,
             BATCH_TYPE_CD                CHAR(1) NOT NULL,
             CHANNEL_CD                   CHAR(1) NOT NULL,
             REGION_CD                    CHAR(3) NOT NULL,
             CYCLE_CD                     CHAR(3) NOT NULL,
             REASON_CD                    CHAR(2) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.LOCKBOX_REMITTANCE             *
      *================================================================*
       01  DCLLOCKBOX-REMITTANCE.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-REMIT-AMT                 PIC S9(13)V99 COMP-3.
           10  H-REMIT-YTD                 PIC S9(13)V99 COMP-3.
           10  H-CHECK-AMT                 PIC S9(13)V99 COMP-3.
           10  H-CHECK-YTD                 PIC S9(13)V99 COMP-3.
           10  H-CASH-ON-HAND-AMT          PIC S9(13)V99 COMP-3.
           10  H-CASH-ON-HAND-YTD          PIC S9(13)V99 COMP-3.
           10  H-FLOAT-AMT                 PIC S9(13)V99 COMP-3.
           10  H-FLOAT-YTD                 PIC S9(13)V99 COMP-3.
           10  H-DEPOSIT-AMT               PIC S9(13)V99 COMP-3.
           10  H-DEPOSIT-YTD               PIC S9(13)V99 COMP-3.
           10  H-FEE-AMT                   PIC S9(13)V99 COMP-3.
           10  H-FEE-YTD                   PIC S9(13)V99 COMP-3.
           10  H-EXCEPT-AMT                PIC S9(13)V99 COMP-3.
           10  H-EXCEPT-YTD                PIC S9(13)V99 COMP-3.
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-LOCKBOX-CD                PIC X(02).
           10  H-BATCH-TYPE-CD             PIC X(01).
           10  H-CHANNEL-CD                PIC X(01).
           10  H-REGION-CD                 PIC X(03).
           10  H-CYCLE-CD                  PIC X(03).
           10  H-REASON-CD                 PIC X(02).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 49      *
      *================================================================*
