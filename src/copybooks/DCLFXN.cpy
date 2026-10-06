      *================================================================*
      * DCLGEN TABLE(CBS.FOREIGN_BANKNOTES)                            *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLFXN))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.FOREIGN_BANKNOTES TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             FOREIGN_AMT                  DECIMAL(15, 2) NOT NULL,
             FOREIGN_YTD                  DECIMAL(15, 2) NOT NULL,
             USD_AMT                      DECIMAL(15, 2) NOT NULL,
             USD_YTD                      DECIMAL(15, 2) NOT NULL,
             MARGIN_AMT                   DECIMAL(15, 2) NOT NULL,
             MARGIN_YTD                   DECIMAL(15, 2) NOT NULL,
             BUYBACK_AMT                  DECIMAL(15, 2) NOT NULL,
             BUYBACK_YTD                  DECIMAL(15, 2) NOT NULL,
             SHIPPING_AMT                 DECIMAL(15, 2) NOT NULL,
             SHIPPING_YTD                 DECIMAL(15, 2) NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             CURRENCY_CD                  CHAR(1) NOT NULL,
             ORDER_TYPE_CD                CHAR(3) NOT NULL,
             TYPE_CD                      CHAR(3) NOT NULL,
             BRANCH_CD                    CHAR(3) NOT NULL,
             STATUS_CD                    CHAR(3) NOT NULL,
             SOURCE_CD                    CHAR(2) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.FOREIGN_BANKNOTES              *
      *================================================================*
       01  DCLFOREIGN-BANKNOTES.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-FOREIGN-AMT               PIC S9(13)V99 COMP-3.
           10  H-FOREIGN-YTD               PIC S9(13)V99 COMP-3.
           10  H-USD-AMT                   PIC S9(13)V99 COMP-3.
           10  H-USD-YTD                   PIC S9(13)V99 COMP-3.
           10  H-MARGIN-AMT                PIC S9(13)V99 COMP-3.
           10  H-MARGIN-YTD                PIC S9(13)V99 COMP-3.
           10  H-BUYBACK-AMT               PIC S9(13)V99 COMP-3.
           10  H-BUYBACK-YTD               PIC S9(13)V99 COMP-3.
           10  H-SHIPPING-AMT              PIC S9(13)V99 COMP-3.
           10  H-SHIPPING-YTD              PIC S9(13)V99 COMP-3.
           10  H-CLOSE-DATE                PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-CURRENCY-CD               PIC X(01).
           10  H-ORDER-TYPE-CD             PIC X(03).
           10  H-TYPE-CD                   PIC X(03).
           10  H-BRANCH-CD                 PIC X(03).
           10  H-STATUS-CD                 PIC X(03).
           10  H-SOURCE-CD                 PIC X(02).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 45      *
      *================================================================*
