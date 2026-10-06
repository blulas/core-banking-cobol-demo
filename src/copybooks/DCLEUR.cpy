      *================================================================*
      * DCLGEN TABLE(CBS.EURO_CONVERSION)                              *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLEUR))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.EURO_CONVERSION TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             NATIONAL_AMT                 DECIMAL(15, 2) NOT NULL,
             NATIONAL_YTD                 DECIMAL(15, 2) NOT NULL,
             EURO_AMT                     DECIMAL(15, 2) NOT NULL,
             EURO_YTD                     DECIMAL(15, 2) NOT NULL,
             ROUNDING_AMT                 DECIMAL(15, 2) NOT NULL,
             ROUNDING_YTD                 DECIMAL(15, 2) NOT NULL,
             TRIANGULAR_AMT               DECIMAL(15, 2) NOT NULL,
             TRIANGULAR_YTD               DECIMAL(15, 2) NOT NULL,
             CONVERTED_AMT                DECIMAL(15, 2) NOT NULL,
             CONVERTED_YTD                DECIMAL(15, 2) NOT NULL,
             RESIDUAL_AMT                 DECIMAL(15, 2) NOT NULL,
             RESIDUAL_YTD                 DECIMAL(15, 2) NOT NULL,
             FEE_AMT                      DECIMAL(15, 2) NOT NULL,
             FEE_YTD                      DECIMAL(15, 2) NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             CURRENCY_CD                  CHAR(2) NOT NULL,
             CONV_METHOD_CD               CHAR(2) NOT NULL,
             STATUS_CD                    CHAR(1) NOT NULL,
             TYPE_CD                      CHAR(3) NOT NULL,
             CHANNEL_CD                   CHAR(3) NOT NULL,
             REGION_CD                    CHAR(3) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.EURO_CONVERSION                *
      *================================================================*
       01  DCLEURO-CONVERSION.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-NATIONAL-AMT              PIC S9(13)V99 COMP-3.
           10  H-NATIONAL-YTD              PIC S9(13)V99 COMP-3.
           10  H-EURO-AMT                  PIC S9(13)V99 COMP-3.
           10  H-EURO-YTD                  PIC S9(13)V99 COMP-3.
           10  H-ROUNDING-AMT              PIC S9(13)V99 COMP-3.
           10  H-ROUNDING-YTD              PIC S9(13)V99 COMP-3.
           10  H-TRIANGULAR-AMT            PIC S9(13)V99 COMP-3.
           10  H-TRIANGULAR-YTD            PIC S9(13)V99 COMP-3.
           10  H-CONVERTED-AMT             PIC S9(13)V99 COMP-3.
           10  H-CONVERTED-YTD             PIC S9(13)V99 COMP-3.
           10  H-RESIDUAL-AMT              PIC S9(13)V99 COMP-3.
           10  H-RESIDUAL-YTD              PIC S9(13)V99 COMP-3.
           10  H-FEE-AMT                   PIC S9(13)V99 COMP-3.
           10  H-FEE-YTD                   PIC S9(13)V99 COMP-3.
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-CURRENCY-CD               PIC X(02).
           10  H-CONV-METHOD-CD            PIC X(02).
           10  H-STATUS-CD                 PIC X(01).
           10  H-TYPE-CD                   PIC X(03).
           10  H-CHANNEL-CD                PIC X(03).
           10  H-REGION-CD                 PIC X(03).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 49      *
      *================================================================*
