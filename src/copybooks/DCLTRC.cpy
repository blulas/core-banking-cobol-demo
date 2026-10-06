      *================================================================*
      * DCLGEN TABLE(CBS.TRAVELERS_CHEQUES)                            *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLTRC))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.TRAVELERS_CHEQUES TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             FACE_AMT                     DECIMAL(15, 2) NOT NULL,
             FACE_YTD                     DECIMAL(15, 2) NOT NULL,
             COMMISSION_AMT               DECIMAL(15, 2) NOT NULL,
             COMMISSION_YTD               DECIMAL(15, 2) NOT NULL,
             CONSIGN_AMT                  DECIMAL(15, 2) NOT NULL,
             CONSIGN_YTD                  DECIMAL(15, 2) NOT NULL,
             REMIT_AMT                    DECIMAL(15, 2) NOT NULL,
             REMIT_YTD                    DECIMAL(15, 2) NOT NULL,
             REFUND_AMT                   DECIMAL(15, 2) NOT NULL,
             REFUND_YTD                   DECIMAL(15, 2) NOT NULL,
             SHORTAGE_AMT                 DECIMAL(15, 2) NOT NULL,
             SHORTAGE_YTD                 DECIMAL(15, 2) NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             ISSUER_CD                    CHAR(2) NOT NULL,
             DENOM_CD                     CHAR(1) NOT NULL,
             BRANCH_CD                    CHAR(2) NOT NULL,
             STATUS_CD                    CHAR(2) NOT NULL,
             SOURCE_CD                    CHAR(2) NOT NULL,
             CYCLE_CD                     CHAR(3) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             EXCEPT_CNT                   INTEGER NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             RATE                         DECIMAL(9, 6) NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.TRAVELERS_CHEQUES              *
      *================================================================*
       01  DCLTRAVELERS-CHEQUES.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-FACE-AMT                  PIC S9(13)V99 COMP-3.
           10  H-FACE-YTD                  PIC S9(13)V99 COMP-3.
           10  H-COMMISSION-AMT            PIC S9(13)V99 COMP-3.
           10  H-COMMISSION-YTD            PIC S9(13)V99 COMP-3.
           10  H-CONSIGN-AMT               PIC S9(13)V99 COMP-3.
           10  H-CONSIGN-YTD               PIC S9(13)V99 COMP-3.
           10  H-REMIT-AMT                 PIC S9(13)V99 COMP-3.
           10  H-REMIT-YTD                 PIC S9(13)V99 COMP-3.
           10  H-REFUND-AMT                PIC S9(13)V99 COMP-3.
           10  H-REFUND-YTD                PIC S9(13)V99 COMP-3.
           10  H-SHORTAGE-AMT              PIC S9(13)V99 COMP-3.
           10  H-SHORTAGE-YTD              PIC S9(13)V99 COMP-3.
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-MATURITY-DATE             PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-ISSUER-CD                 PIC X(02).
           10  H-DENOM-CD                  PIC X(01).
           10  H-BRANCH-CD                 PIC X(02).
           10  H-STATUS-CD                 PIC X(02).
           10  H-SOURCE-CD                 PIC X(02).
           10  H-CYCLE-CD                  PIC X(03).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-EXCEPT-CNT                PIC S9(9) COMP.
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-RATE                      PIC S9(3)V9(6) COMP-3.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      *================================================================*
