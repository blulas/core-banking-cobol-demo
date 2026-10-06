      *================================================================*
      * DCLGEN TABLE(CBS.BILL_PAYMENT_SERVI)                           *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLBPY))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.BILL_PAYMENT_SERVI TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             PAYMENT_AMT                  DECIMAL(15, 2) NOT NULL,
             PAYMENT_YTD                  DECIMAL(15, 2) NOT NULL,
             FEE_AMT                      DECIMAL(15, 2) NOT NULL,
             FEE_YTD                      DECIMAL(15, 2) NOT NULL,
             RETURNED_AMT                 DECIMAL(15, 2) NOT NULL,
             RETURNED_YTD                 DECIMAL(15, 2) NOT NULL,
             REFUND_AMT                   DECIMAL(15, 2) NOT NULL,
             REFUND_YTD                   DECIMAL(15, 2) NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             PAYEE_TYPE_CD                CHAR(2) NOT NULL,
             REMIT_METHOD_CD              CHAR(3) NOT NULL,
             REASON_CD                    CHAR(2) NOT NULL,
             CYCLE_CD                     CHAR(2) NOT NULL,
             STATUS_CD                    CHAR(2) NOT NULL,
             SOURCE_CD                    CHAR(2) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.BILL_PAYMENT_SERVI             *
      *================================================================*
       01  DCLBILL-PAYMENT-SERVI.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-PAYMENT-AMT               PIC S9(13)V99 COMP-3.
           10  H-PAYMENT-YTD               PIC S9(13)V99 COMP-3.
           10  H-FEE-AMT                   PIC S9(13)V99 COMP-3.
           10  H-FEE-YTD                   PIC S9(13)V99 COMP-3.
           10  H-RETURNED-AMT              PIC S9(13)V99 COMP-3.
           10  H-RETURNED-YTD              PIC S9(13)V99 COMP-3.
           10  H-REFUND-AMT                PIC S9(13)V99 COMP-3.
           10  H-REFUND-YTD                PIC S9(13)V99 COMP-3.
           10  H-MATURITY-DATE             PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-PAYEE-TYPE-CD             PIC X(02).
           10  H-REMIT-METHOD-CD           PIC X(03).
           10  H-REASON-CD                 PIC X(02).
           10  H-CYCLE-CD                  PIC X(02).
           10  H-STATUS-CD                 PIC X(02).
           10  H-SOURCE-CD                 PIC X(02).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 43      *
      *================================================================*
