      *================================================================*
      * DCLGEN TABLE(CBS.PERSONAL_TRUST)                               *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLTRS))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.PERSONAL_TRUST TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             PRINCIPAL_AMT                DECIMAL(15, 2) NOT NULL,
             PRINCIPAL_YTD                DECIMAL(15, 2) NOT NULL,
             INCOME_AMT                   DECIMAL(15, 2) NOT NULL,
             INCOME_YTD                   DECIMAL(15, 2) NOT NULL,
             DISTRIB_AMT                  DECIMAL(15, 2) NOT NULL,
             DISTRIB_YTD                  DECIMAL(15, 2) NOT NULL,
             TRUSTEE_FEE_AMT              DECIMAL(15, 2) NOT NULL,
             TRUSTEE_FEE_YTD              DECIMAL(15, 2) NOT NULL,
             MARKET_AMT                   DECIMAL(15, 2) NOT NULL,
             MARKET_YTD                   DECIMAL(15, 2) NOT NULL,
             TAX_AMT                      DECIMAL(15, 2) NOT NULL,
             TAX_YTD                      DECIMAL(15, 2) NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             TRUST_TYPE_CD                CHAR(3) NOT NULL,
             DIST_FREQ_CD                 CHAR(1) NOT NULL,
             REASON_CD                    CHAR(1) NOT NULL,
             REGION_CD                    CHAR(1) NOT NULL,
             STATUS_CD                    CHAR(2) NOT NULL,
             CYCLE_CD                     CHAR(2) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.PERSONAL_TRUST                 *
      *================================================================*
       01  DCLPERSONAL-TRUST.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-PRINCIPAL-AMT             PIC S9(13)V99 COMP-3.
           10  H-PRINCIPAL-YTD             PIC S9(13)V99 COMP-3.
           10  H-INCOME-AMT                PIC S9(13)V99 COMP-3.
           10  H-INCOME-YTD                PIC S9(13)V99 COMP-3.
           10  H-DISTRIB-AMT               PIC S9(13)V99 COMP-3.
           10  H-DISTRIB-YTD               PIC S9(13)V99 COMP-3.
           10  H-TRUSTEE-FEE-AMT           PIC S9(13)V99 COMP-3.
           10  H-TRUSTEE-FEE-YTD           PIC S9(13)V99 COMP-3.
           10  H-MARKET-AMT                PIC S9(13)V99 COMP-3.
           10  H-MARKET-YTD                PIC S9(13)V99 COMP-3.
           10  H-TAX-AMT                   PIC S9(13)V99 COMP-3.
           10  H-TAX-YTD                   PIC S9(13)V99 COMP-3.
           10  H-OPEN-DATE                 PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-TRUST-TYPE-CD             PIC X(03).
           10  H-DIST-FREQ-CD              PIC X(01).
           10  H-REASON-CD                 PIC X(01).
           10  H-REGION-CD                 PIC X(01).
           10  H-STATUS-CD                 PIC X(02).
           10  H-CYCLE-CD                  PIC X(02).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      *================================================================*
