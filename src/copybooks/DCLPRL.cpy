      *================================================================*
      * DCLGEN TABLE(CBS.PAYROLL_SERVICES)                             *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLPRL))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.PAYROLL_SERVICES TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             GROSS_AMT                    DECIMAL(15, 2) NOT NULL,
             GROSS_YTD                    DECIMAL(15, 2) NOT NULL,
             NET_AMT                      DECIMAL(15, 2) NOT NULL,
             NET_YTD                      DECIMAL(15, 2) NOT NULL,
             FED_TAX_AMT                  DECIMAL(15, 2) NOT NULL,
             FED_TAX_YTD                  DECIMAL(15, 2) NOT NULL,
             STATE_TAX_AMT                DECIMAL(15, 2) NOT NULL,
             STATE_TAX_YTD                DECIMAL(15, 2) NOT NULL,
             FICA_AMT                     DECIMAL(15, 2) NOT NULL,
             FICA_YTD                     DECIMAL(15, 2) NOT NULL,
             DEDUCT_AMT                   DECIMAL(15, 2) NOT NULL,
             DEDUCT_YTD                   DECIMAL(15, 2) NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             PAY_FREQ_CD                  CHAR(2) NOT NULL,
             TAX_STATE_CD                 CHAR(3) NOT NULL,
             STATUS_CD                    CHAR(2) NOT NULL,
             SOURCE_CD                    CHAR(2) NOT NULL,
             REGION_CD                    CHAR(2) NOT NULL,
             REASON_CD                    CHAR(1) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.PAYROLL_SERVICES               *
      *================================================================*
       01  DCLPAYROLL-SERVICES.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-GROSS-AMT                 PIC S9(13)V99 COMP-3.
           10  H-GROSS-YTD                 PIC S9(13)V99 COMP-3.
           10  H-NET-AMT                   PIC S9(13)V99 COMP-3.
           10  H-NET-YTD                   PIC S9(13)V99 COMP-3.
           10  H-FED-TAX-AMT               PIC S9(13)V99 COMP-3.
           10  H-FED-TAX-YTD               PIC S9(13)V99 COMP-3.
           10  H-STATE-TAX-AMT             PIC S9(13)V99 COMP-3.
           10  H-STATE-TAX-YTD             PIC S9(13)V99 COMP-3.
           10  H-FICA-AMT                  PIC S9(13)V99 COMP-3.
           10  H-FICA-YTD                  PIC S9(13)V99 COMP-3.
           10  H-DEDUCT-AMT                PIC S9(13)V99 COMP-3.
           10  H-DEDUCT-YTD                PIC S9(13)V99 COMP-3.
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-PAY-FREQ-CD               PIC X(02).
           10  H-TAX-STATE-CD              PIC X(03).
           10  H-STATUS-CD                 PIC X(02).
           10  H-SOURCE-CD                 PIC X(02).
           10  H-REGION-CD                 PIC X(02).
           10  H-REASON-CD                 PIC X(01).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      *================================================================*
