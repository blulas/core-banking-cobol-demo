      *================================================================*
      * DCLGEN TABLE(CBS.IRA_CUSTODY)                                  *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLIRA))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.IRA_CUSTODY TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             CONTRIB_AMT                  DECIMAL(15, 2) NOT NULL,
             CONTRIB_YTD                  DECIMAL(15, 2) NOT NULL,
             DISTRIB_AMT                  DECIMAL(15, 2) NOT NULL,
             DISTRIB_YTD                  DECIMAL(15, 2) NOT NULL,
             WITHHOLD_AMT                 DECIMAL(15, 2) NOT NULL,
             WITHHOLD_YTD                 DECIMAL(15, 2) NOT NULL,
             ROLLOVER_AMT                 DECIMAL(15, 2) NOT NULL,
             ROLLOVER_YTD                 DECIMAL(15, 2) NOT NULL,
             RMD_AMT                      DECIMAL(15, 2) NOT NULL,
             RMD_YTD                      DECIMAL(15, 2) NOT NULL,
             FMV_AMT                      DECIMAL(15, 2) NOT NULL,
             FMV_YTD                      DECIMAL(15, 2) NOT NULL,
             EXCESS_AMT                   DECIMAL(15, 2) NOT NULL,
             EXCESS_YTD                   DECIMAL(15, 2) NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             IRA_TYPE_CD                  CHAR(1) NOT NULL,
             DIST_CODE_CD                 CHAR(1) NOT NULL,
             CHANNEL_CD                   CHAR(2) NOT NULL,
             TYPE_CD                      CHAR(2) NOT NULL,
             REGION_CD                    CHAR(2) NOT NULL,
             REASON_CD                    CHAR(3) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.IRA_CUSTODY                    *
      *================================================================*
       01  DCLIRA-CUSTODY.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-CONTRIB-AMT               PIC S9(13)V99 COMP-3.
           10  H-CONTRIB-YTD               PIC S9(13)V99 COMP-3.
           10  H-DISTRIB-AMT               PIC S9(13)V99 COMP-3.
           10  H-DISTRIB-YTD               PIC S9(13)V99 COMP-3.
           10  H-WITHHOLD-AMT              PIC S9(13)V99 COMP-3.
           10  H-WITHHOLD-YTD              PIC S9(13)V99 COMP-3.
           10  H-ROLLOVER-AMT              PIC S9(13)V99 COMP-3.
           10  H-ROLLOVER-YTD              PIC S9(13)V99 COMP-3.
           10  H-RMD-AMT                   PIC S9(13)V99 COMP-3.
           10  H-RMD-YTD                   PIC S9(13)V99 COMP-3.
           10  H-FMV-AMT                   PIC S9(13)V99 COMP-3.
           10  H-FMV-YTD                   PIC S9(13)V99 COMP-3.
           10  H-EXCESS-AMT                PIC S9(13)V99 COMP-3.
           10  H-EXCESS-YTD                PIC S9(13)V99 COMP-3.
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-IRA-TYPE-CD               PIC X(01).
           10  H-DIST-CODE-CD              PIC X(01).
           10  H-CHANNEL-CD                PIC X(02).
           10  H-TYPE-CD                   PIC X(02).
           10  H-REGION-CD                 PIC X(02).
           10  H-REASON-CD                 PIC X(03).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 49      *
      *================================================================*
