      *================================================================*
      * DCLGEN TABLE(CBS.ACCOUNT_RECONCILEM)                           *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLARP))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.ACCOUNT_RECONCILEM TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             ISSUED_AMT                   DECIMAL(15, 2) NOT NULL,
             ISSUED_YTD                   DECIMAL(15, 2) NOT NULL,
             PAID_AMT                     DECIMAL(15, 2) NOT NULL,
             PAID_YTD                     DECIMAL(15, 2) NOT NULL,
             OUTSTAND_AMT                 DECIMAL(15, 2) NOT NULL,
             OUTSTAND_YTD                 DECIMAL(15, 2) NOT NULL,
             STOP_AMT                     DECIMAL(15, 2) NOT NULL,
             STOP_YTD                     DECIMAL(15, 2) NOT NULL,
             VOID_AMT                     DECIMAL(15, 2) NOT NULL,
             VOID_YTD                     DECIMAL(15, 2) NOT NULL,
             FEE_AMT                      DECIMAL(15, 2) NOT NULL,
             FEE_YTD                      DECIMAL(15, 2) NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             RECON_TYPE_CD                CHAR(3) NOT NULL,
             CYCLE_CD                     CHAR(2) NOT NULL,
             REGION_CD                    CHAR(2) NOT NULL,
             BRANCH_CD                    CHAR(2) NOT NULL,
             CHANNEL_CD                   CHAR(2) NOT NULL,
             SOURCE_CD                    CHAR(2) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.ACCOUNT_RECONCILEM             *
      *================================================================*
       01  DCLACCOUNT-RECONCILEM.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-ISSUED-AMT                PIC S9(13)V99 COMP-3.
           10  H-ISSUED-YTD                PIC S9(13)V99 COMP-3.
           10  H-PAID-AMT                  PIC S9(13)V99 COMP-3.
           10  H-PAID-YTD                  PIC S9(13)V99 COMP-3.
           10  H-OUTSTAND-AMT              PIC S9(13)V99 COMP-3.
           10  H-OUTSTAND-YTD              PIC S9(13)V99 COMP-3.
           10  H-STOP-AMT                  PIC S9(13)V99 COMP-3.
           10  H-STOP-YTD                  PIC S9(13)V99 COMP-3.
           10  H-VOID-AMT                  PIC S9(13)V99 COMP-3.
           10  H-VOID-YTD                  PIC S9(13)V99 COMP-3.
           10  H-FEE-AMT                   PIC S9(13)V99 COMP-3.
           10  H-FEE-YTD                   PIC S9(13)V99 COMP-3.
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-RECON-TYPE-CD             PIC X(03).
           10  H-CYCLE-CD                  PIC X(02).
           10  H-REGION-CD                 PIC X(02).
           10  H-BRANCH-CD                 PIC X(02).
           10  H-CHANNEL-CD                PIC X(02).
           10  H-SOURCE-CD                 PIC X(02).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      *================================================================*
