      *================================================================*
      * DCLGEN TABLE(CBS.CHRISTMAS_CLUB_SAV)                           *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLXCL))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.CHRISTMAS_CLUB_SAV TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             GOAL_AMT                     DECIMAL(15, 2) NOT NULL,
             GOAL_YTD                     DECIMAL(15, 2) NOT NULL,
             WEEKLY_AMT                   DECIMAL(15, 2) NOT NULL,
             WEEKLY_YTD                   DECIMAL(15, 2) NOT NULL,
             PAYOUT_AMT                   DECIMAL(15, 2) NOT NULL,
             PAYOUT_YTD                   DECIMAL(15, 2) NOT NULL,
             BONUS_AMT                    DECIMAL(15, 2) NOT NULL,
             BONUS_YTD                    DECIMAL(15, 2) NOT NULL,
             PENALTY_AMT                  DECIMAL(15, 2) NOT NULL,
             PENALTY_YTD                  DECIMAL(15, 2) NOT NULL,
             ACCRUED_AMT                  DECIMAL(15, 2) NOT NULL,
             ACCRUED_YTD                  DECIMAL(15, 2) NOT NULL,
             CONTRIB_AMT                  DECIMAL(15, 2) NOT NULL,
             CONTRIB_YTD                  DECIMAL(15, 2) NOT NULL,
             MISSED_AMT                   DECIMAL(15, 2) NOT NULL,
             MISSED_YTD                   DECIMAL(15, 2) NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             PLAN_CD                      CHAR(2) NOT NULL,
             PAYOUT_METHOD_CD             CHAR(1) NOT NULL,
             TYPE_CD                      CHAR(1) NOT NULL,
             REASON_CD                    CHAR(2) NOT NULL,
             CYCLE_CD                     CHAR(2) NOT NULL,
             REGION_CD                    CHAR(3) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.CHRISTMAS_CLUB_SAV             *
      *================================================================*
       01  DCLCHRISTMAS-CLUB-SAV.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-GOAL-AMT                  PIC S9(13)V99 COMP-3.
           10  H-GOAL-YTD                  PIC S9(13)V99 COMP-3.
           10  H-WEEKLY-AMT                PIC S9(13)V99 COMP-3.
           10  H-WEEKLY-YTD                PIC S9(13)V99 COMP-3.
           10  H-PAYOUT-AMT                PIC S9(13)V99 COMP-3.
           10  H-PAYOUT-YTD                PIC S9(13)V99 COMP-3.
           10  H-BONUS-AMT                 PIC S9(13)V99 COMP-3.
           10  H-BONUS-YTD                 PIC S9(13)V99 COMP-3.
           10  H-PENALTY-AMT               PIC S9(13)V99 COMP-3.
           10  H-PENALTY-YTD               PIC S9(13)V99 COMP-3.
           10  H-ACCRUED-AMT               PIC S9(13)V99 COMP-3.
           10  H-ACCRUED-YTD               PIC S9(13)V99 COMP-3.
           10  H-CONTRIB-AMT               PIC S9(13)V99 COMP-3.
           10  H-CONTRIB-YTD               PIC S9(13)V99 COMP-3.
           10  H-MISSED-AMT                PIC S9(13)V99 COMP-3.
           10  H-MISSED-YTD                PIC S9(13)V99 COMP-3.
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-PLAN-CD                   PIC X(02).
           10  H-PAYOUT-METHOD-CD          PIC X(01).
           10  H-TYPE-CD                   PIC X(01).
           10  H-REASON-CD                 PIC X(02).
           10  H-CYCLE-CD                  PIC X(02).
           10  H-REGION-CD                 PIC X(03).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 51      *
      *================================================================*
