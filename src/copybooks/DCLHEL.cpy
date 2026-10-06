      *================================================================*
      * DCLGEN TABLE(CBS.HOME_EQUITY_LINES)                            *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLHEL))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.HOME_EQUITY_LINES TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             LIMIT_AMT                    DECIMAL(15, 2) NOT NULL,
             LIMIT_YTD                    DECIMAL(15, 2) NOT NULL,
             DRAW_AMT                     DECIMAL(15, 2) NOT NULL,
             DRAW_YTD                     DECIMAL(15, 2) NOT NULL,
             BALANCE_AMT                  DECIMAL(15, 2) NOT NULL,
             BALANCE_YTD                  DECIMAL(15, 2) NOT NULL,
             INTEREST_AMT                 DECIMAL(15, 2) NOT NULL,
             INTEREST_YTD                 DECIMAL(15, 2) NOT NULL,
             MIN_PAY_AMT                  DECIMAL(15, 2) NOT NULL,
             MIN_PAY_YTD                  DECIMAL(15, 2) NOT NULL,
             ANNUAL_FEE_AMT               DECIMAL(15, 2) NOT NULL,
             ANNUAL_FEE_YTD               DECIMAL(15, 2) NOT NULL,
             APPRAISE_AMT                 DECIMAL(15, 2) NOT NULL,
             APPRAISE_YTD                 DECIMAL(15, 2) NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             LIEN_POS_CD                  CHAR(3) NOT NULL,
             DRAW_PERIOD_CD               CHAR(3) NOT NULL,
             TYPE_CD                      CHAR(2) NOT NULL,
             REGION_CD                    CHAR(3) NOT NULL,
             STATUS_CD                    CHAR(2) NOT NULL,
             REASON_CD                    CHAR(3) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.HOME_EQUITY_LINES              *
      *================================================================*
       01  DCLHOME-EQUITY-LINES.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-LIMIT-AMT                 PIC S9(13)V99 COMP-3.
           10  H-LIMIT-YTD                 PIC S9(13)V99 COMP-3.
           10  H-DRAW-AMT                  PIC S9(13)V99 COMP-3.
           10  H-DRAW-YTD                  PIC S9(13)V99 COMP-3.
           10  H-BALANCE-AMT               PIC S9(13)V99 COMP-3.
           10  H-BALANCE-YTD               PIC S9(13)V99 COMP-3.
           10  H-INTEREST-AMT              PIC S9(13)V99 COMP-3.
           10  H-INTEREST-YTD              PIC S9(13)V99 COMP-3.
           10  H-MIN-PAY-AMT               PIC S9(13)V99 COMP-3.
           10  H-MIN-PAY-YTD               PIC S9(13)V99 COMP-3.
           10  H-ANNUAL-FEE-AMT            PIC S9(13)V99 COMP-3.
           10  H-ANNUAL-FEE-YTD            PIC S9(13)V99 COMP-3.
           10  H-APPRAISE-AMT              PIC S9(13)V99 COMP-3.
           10  H-APPRAISE-YTD              PIC S9(13)V99 COMP-3.
           10  H-OPEN-DATE                 PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-LIEN-POS-CD               PIC X(03).
           10  H-DRAW-PERIOD-CD            PIC X(03).
           10  H-TYPE-CD                   PIC X(02).
           10  H-REGION-CD                 PIC X(03).
           10  H-STATUS-CD                 PIC X(02).
           10  H-REASON-CD                 PIC X(03).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 49      *
      *================================================================*
