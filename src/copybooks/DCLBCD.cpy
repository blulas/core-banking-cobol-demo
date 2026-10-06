      *================================================================*
      * DCLGEN TABLE(CBS.BUMP-UP_CERTIFICAT)                           *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLBCD))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.BUMP-UP_CERTIFICAT TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             PRINCIPAL_AMT                DECIMAL(15, 2) NOT NULL,
             PRINCIPAL_YTD                DECIMAL(15, 2) NOT NULL,
             INTEREST_AMT                 DECIMAL(15, 2) NOT NULL,
             INTEREST_YTD                 DECIMAL(15, 2) NOT NULL,
             BUMP_AMT                     DECIMAL(15, 2) NOT NULL,
             BUMP_YTD                     DECIMAL(15, 2) NOT NULL,
             PENALTY_AMT                  DECIMAL(15, 2) NOT NULL,
             PENALTY_YTD                  DECIMAL(15, 2) NOT NULL,
             ADDON_AMT                    DECIMAL(15, 2) NOT NULL,
             ADDON_YTD                    DECIMAL(15, 2) NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             STEP_PLAN_CD                 CHAR(3) NOT NULL,
             ELECTION_CD                  CHAR(1) NOT NULL,
             CHANNEL_CD                   CHAR(2) NOT NULL,
             REGION_CD                    CHAR(3) NOT NULL,
             CYCLE_CD                     CHAR(1) NOT NULL,
             SOURCE_CD                    CHAR(2) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.BUMP-UP_CERTIFICAT             *
      *================================================================*
       01  DCLBUMP-UP-CERTIFICAT.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-PRINCIPAL-AMT             PIC S9(13)V99 COMP-3.
           10  H-PRINCIPAL-YTD             PIC S9(13)V99 COMP-3.
           10  H-INTEREST-AMT              PIC S9(13)V99 COMP-3.
           10  H-INTEREST-YTD              PIC S9(13)V99 COMP-3.
           10  H-BUMP-AMT                  PIC S9(13)V99 COMP-3.
           10  H-BUMP-YTD                  PIC S9(13)V99 COMP-3.
           10  H-PENALTY-AMT               PIC S9(13)V99 COMP-3.
           10  H-PENALTY-YTD               PIC S9(13)V99 COMP-3.
           10  H-ADDON-AMT                 PIC S9(13)V99 COMP-3.
           10  H-ADDON-YTD                 PIC S9(13)V99 COMP-3.
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-STEP-PLAN-CD              PIC X(03).
           10  H-ELECTION-CD               PIC X(01).
           10  H-CHANNEL-CD                PIC X(02).
           10  H-REGION-CD                 PIC X(03).
           10  H-CYCLE-CD                  PIC X(01).
           10  H-SOURCE-CD                 PIC X(02).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 45      *
      *================================================================*
