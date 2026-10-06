      *================================================================*
      * DCLGEN TABLE(CBS.POSITIVE_PAY)                                 *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLPPY))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.POSITIVE_PAY TABLE
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
             EXCEPT_AMT                   DECIMAL(15, 2) NOT NULL,
             EXCEPT_YTD                   DECIMAL(15, 2) NOT NULL,
             STALE_AMT                    DECIMAL(15, 2) NOT NULL,
             STALE_YTD                    DECIMAL(15, 2) NOT NULL,
             VOID_AMT                     DECIMAL(15, 2) NOT NULL,
             VOID_YTD                     DECIMAL(15, 2) NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             MATCH_RULE_CD                CHAR(1) NOT NULL,
             DECISION_CD                  CHAR(3) NOT NULL,
             REGION_CD                    CHAR(1) NOT NULL,
             STATUS_CD                    CHAR(2) NOT NULL,
             CYCLE_CD                     CHAR(1) NOT NULL,
             REASON_CD                    CHAR(1) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.POSITIVE_PAY                   *
      *================================================================*
       01  DCLPOSITIVE-PAY.
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
           10  H-EXCEPT-AMT                PIC S9(13)V99 COMP-3.
           10  H-EXCEPT-YTD                PIC S9(13)V99 COMP-3.
           10  H-STALE-AMT                 PIC S9(13)V99 COMP-3.
           10  H-STALE-YTD                 PIC S9(13)V99 COMP-3.
           10  H-VOID-AMT                  PIC S9(13)V99 COMP-3.
           10  H-VOID-YTD                  PIC S9(13)V99 COMP-3.
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-MATCH-RULE-CD             PIC X(01).
           10  H-DECISION-CD               PIC X(03).
           10  H-REGION-CD                 PIC X(01).
           10  H-STATUS-CD                 PIC X(02).
           10  H-CYCLE-CD                  PIC X(01).
           10  H-REASON-CD                 PIC X(01).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 45      *
      *================================================================*
