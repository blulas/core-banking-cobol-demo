      *================================================================*
      * DCLGEN TABLE(CBS.MORTGAGE_ESCROW)                              *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLESC))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.MORTGAGE_ESCROW TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             TAX_AMT                      DECIMAL(15, 2) NOT NULL,
             TAX_YTD                      DECIMAL(15, 2) NOT NULL,
             INSURANCE_AMT                DECIMAL(15, 2) NOT NULL,
             INSURANCE_YTD                DECIMAL(15, 2) NOT NULL,
             CUSHION_AMT                  DECIMAL(15, 2) NOT NULL,
             CUSHION_YTD                  DECIMAL(15, 2) NOT NULL,
             SHORTAGE_AMT                 DECIMAL(15, 2) NOT NULL,
             SHORTAGE_YTD                 DECIMAL(15, 2) NOT NULL,
             SURPLUS_AMT                  DECIMAL(15, 2) NOT NULL,
             SURPLUS_YTD                  DECIMAL(15, 2) NOT NULL,
             DISBURSE_AMT                 DECIMAL(15, 2) NOT NULL,
             DISBURSE_YTD                 DECIMAL(15, 2) NOT NULL,
             PAYMENT_AMT                  DECIMAL(15, 2) NOT NULL,
             PAYMENT_YTD                  DECIMAL(15, 2) NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             PAYEE_TYPE_CD                CHAR(2) NOT NULL,
             ANALYSIS_CD                  CHAR(2) NOT NULL,
             STATUS_CD                    CHAR(1) NOT NULL,
             TYPE_CD                      CHAR(3) NOT NULL,
             REGION_CD                    CHAR(1) NOT NULL,
             REASON_CD                    CHAR(1) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.MORTGAGE_ESCROW                *
      *================================================================*
       01  DCLMORTGAGE-ESCROW.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-TAX-AMT                   PIC S9(13)V99 COMP-3.
           10  H-TAX-YTD                   PIC S9(13)V99 COMP-3.
           10  H-INSURANCE-AMT             PIC S9(13)V99 COMP-3.
           10  H-INSURANCE-YTD             PIC S9(13)V99 COMP-3.
           10  H-CUSHION-AMT               PIC S9(13)V99 COMP-3.
           10  H-CUSHION-YTD               PIC S9(13)V99 COMP-3.
           10  H-SHORTAGE-AMT              PIC S9(13)V99 COMP-3.
           10  H-SHORTAGE-YTD              PIC S9(13)V99 COMP-3.
           10  H-SURPLUS-AMT               PIC S9(13)V99 COMP-3.
           10  H-SURPLUS-YTD               PIC S9(13)V99 COMP-3.
           10  H-DISBURSE-AMT              PIC S9(13)V99 COMP-3.
           10  H-DISBURSE-YTD              PIC S9(13)V99 COMP-3.
           10  H-PAYMENT-AMT               PIC S9(13)V99 COMP-3.
           10  H-PAYMENT-YTD               PIC S9(13)V99 COMP-3.
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-PAYEE-TYPE-CD             PIC X(02).
           10  H-ANALYSIS-CD               PIC X(02).
           10  H-STATUS-CD                 PIC X(01).
           10  H-TYPE-CD                   PIC X(03).
           10  H-REGION-CD                 PIC X(01).
           10  H-REASON-CD                 PIC X(01).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 49      *
      *================================================================*
