      *================================================================*
      * DCLGEN TABLE(CBS.TELEX_WIRE_MESSAGI)                           *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLTLX))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.TELEX_WIRE_MESSAGI TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             WIRE_AMT                     DECIMAL(15, 2) NOT NULL,
             WIRE_YTD                     DECIMAL(15, 2) NOT NULL,
             CHARGE_AMT                   DECIMAL(15, 2) NOT NULL,
             CHARGE_YTD                   DECIMAL(15, 2) NOT NULL,
             COVER_AMT                    DECIMAL(15, 2) NOT NULL,
             COVER_YTD                    DECIMAL(15, 2) NOT NULL,
             CORRESP_AMT                  DECIMAL(15, 2) NOT NULL,
             CORRESP_YTD                  DECIMAL(15, 2) NOT NULL,
             SETTLE_AMT                   DECIMAL(15, 2) NOT NULL,
             SETTLE_YTD                   DECIMAL(15, 2) NOT NULL,
             QUEUED_AMT                   DECIMAL(15, 2) NOT NULL,
             QUEUED_YTD                   DECIMAL(15, 2) NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             MSG_TYPE_CD                  CHAR(2) NOT NULL,
             TEST_KEY_CD                  CHAR(2) NOT NULL,
             REGION_CD                    CHAR(1) NOT NULL,
             TYPE_CD                      CHAR(1) NOT NULL,
             REASON_CD                    CHAR(3) NOT NULL,
             BRANCH_CD                    CHAR(2) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.TELEX_WIRE_MESSAGI             *
      *================================================================*
       01  DCLTELEX-WIRE-MESSAGI.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-WIRE-AMT                  PIC S9(13)V99 COMP-3.
           10  H-WIRE-YTD                  PIC S9(13)V99 COMP-3.
           10  H-CHARGE-AMT                PIC S9(13)V99 COMP-3.
           10  H-CHARGE-YTD                PIC S9(13)V99 COMP-3.
           10  H-COVER-AMT                 PIC S9(13)V99 COMP-3.
           10  H-COVER-YTD                 PIC S9(13)V99 COMP-3.
           10  H-CORRESP-AMT               PIC S9(13)V99 COMP-3.
           10  H-CORRESP-YTD               PIC S9(13)V99 COMP-3.
           10  H-SETTLE-AMT                PIC S9(13)V99 COMP-3.
           10  H-SETTLE-YTD                PIC S9(13)V99 COMP-3.
           10  H-QUEUED-AMT                PIC S9(13)V99 COMP-3.
           10  H-QUEUED-YTD                PIC S9(13)V99 COMP-3.
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-MSG-TYPE-CD               PIC X(02).
           10  H-TEST-KEY-CD               PIC X(02).
           10  H-REGION-CD                 PIC X(01).
           10  H-TYPE-CD                   PIC X(01).
           10  H-REASON-CD                 PIC X(03).
           10  H-BRANCH-CD                 PIC X(02).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      *================================================================*
