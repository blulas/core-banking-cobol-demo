      *================================================================*
      * DCLGEN TABLE(CBS.STUDENT_LOANS)                                *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLSTL))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.STUDENT_LOANS TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             PRINCIPAL_AMT                DECIMAL(15, 2) NOT NULL,
             PRINCIPAL_YTD                DECIMAL(15, 2) NOT NULL,
             DISBURSE_AMT                 DECIMAL(15, 2) NOT NULL,
             DISBURSE_YTD                 DECIMAL(15, 2) NOT NULL,
             CAPINT_AMT                   DECIMAL(15, 2) NOT NULL,
             CAPINT_YTD                   DECIMAL(15, 2) NOT NULL,
             SUBSIDY_AMT                  DECIMAL(15, 2) NOT NULL,
             SUBSIDY_YTD                  DECIMAL(15, 2) NOT NULL,
             PAYMENT_AMT                  DECIMAL(15, 2) NOT NULL,
             PAYMENT_YTD                  DECIMAL(15, 2) NOT NULL,
             DEFERINT_AMT                 DECIMAL(15, 2) NOT NULL,
             DEFERINT_YTD                 DECIMAL(15, 2) NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             LOAN_TYPE_CD                 CHAR(2) NOT NULL,
             DEFER_CODE_CD                CHAR(2) NOT NULL,
             SOURCE_CD                    CHAR(3) NOT NULL,
             STATUS_CD                    CHAR(2) NOT NULL,
             REGION_CD                    CHAR(1) NOT NULL,
             CYCLE_CD                     CHAR(3) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.STUDENT_LOANS                  *
      *================================================================*
       01  DCLSTUDENT-LOANS.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-PRINCIPAL-AMT             PIC S9(13)V99 COMP-3.
           10  H-PRINCIPAL-YTD             PIC S9(13)V99 COMP-3.
           10  H-DISBURSE-AMT              PIC S9(13)V99 COMP-3.
           10  H-DISBURSE-YTD              PIC S9(13)V99 COMP-3.
           10  H-CAPINT-AMT                PIC S9(13)V99 COMP-3.
           10  H-CAPINT-YTD                PIC S9(13)V99 COMP-3.
           10  H-SUBSIDY-AMT               PIC S9(13)V99 COMP-3.
           10  H-SUBSIDY-YTD               PIC S9(13)V99 COMP-3.
           10  H-PAYMENT-AMT               PIC S9(13)V99 COMP-3.
           10  H-PAYMENT-YTD               PIC S9(13)V99 COMP-3.
           10  H-DEFERINT-AMT              PIC S9(13)V99 COMP-3.
           10  H-DEFERINT-YTD              PIC S9(13)V99 COMP-3.
           10  H-OPEN-DATE                 PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-LOAN-TYPE-CD              PIC X(02).
           10  H-DEFER-CODE-CD             PIC X(02).
           10  H-SOURCE-CD                 PIC X(03).
           10  H-STATUS-CD                 PIC X(02).
           10  H-REGION-CD                 PIC X(01).
           10  H-CYCLE-CD                  PIC X(03).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      *================================================================*
