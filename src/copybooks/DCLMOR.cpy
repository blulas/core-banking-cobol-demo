      *================================================================*
      * DCLGEN TABLE(CBS.MONEY_ORDERS)                                 *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLMOR))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.MONEY_ORDERS TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             FACE_AMT                     DECIMAL(15, 2) NOT NULL,
             FACE_YTD                     DECIMAL(15, 2) NOT NULL,
             FEE_AMT                      DECIMAL(15, 2) NOT NULL,
             FEE_YTD                      DECIMAL(15, 2) NOT NULL,
             PAID_AMT                     DECIMAL(15, 2) NOT NULL,
             PAID_YTD                     DECIMAL(15, 2) NOT NULL,
             OUTSTAND_AMT                 DECIMAL(15, 2) NOT NULL,
             OUTSTAND_YTD                 DECIMAL(15, 2) NOT NULL,
             ESCHEAT_AMT                  DECIMAL(15, 2) NOT NULL,
             ESCHEAT_YTD                  DECIMAL(15, 2) NOT NULL,
             VOIDED_AMT                   DECIMAL(15, 2) NOT NULL,
             VOIDED_YTD                   DECIMAL(15, 2) NOT NULL,
             LAST_PAY_DATE                DATE NOT NULL,
             LAST_STMT_DATE               DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             NOTICE_DATE                  DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             ITEM_TYPE_CD                 CHAR(1) NOT NULL,
             PAYEE_TYPE_CD                CHAR(2) NOT NULL,
             REASON_CD                    CHAR(1) NOT NULL,
             TYPE_CD                      CHAR(3) NOT NULL,
             CHANNEL_CD                   CHAR(2) NOT NULL,
             CYCLE_CD                     CHAR(3) NOT NULL,
             PREFERRED_FLAG               CHAR(12) NOT NULL,
             OVERRIDE_FLAG                CHAR(12) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             STOP_FLAG                    CHAR(12) NOT NULL,
             NOTICE_CNT                   INTEGER NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             DISCOUNT                     DECIMAL(9, 6) NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.MONEY_ORDERS                   *
      *================================================================*
       01  DCLMONEY-ORDERS.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-FACE-AMT                  PIC S9(13)V99 COMP-3.
           10  H-FACE-YTD                  PIC S9(13)V99 COMP-3.
           10  H-FEE-AMT                   PIC S9(13)V99 COMP-3.
           10  H-FEE-YTD                   PIC S9(13)V99 COMP-3.
           10  H-PAID-AMT                  PIC S9(13)V99 COMP-3.
           10  H-PAID-YTD                  PIC S9(13)V99 COMP-3.
           10  H-OUTSTAND-AMT              PIC S9(13)V99 COMP-3.
           10  H-OUTSTAND-YTD              PIC S9(13)V99 COMP-3.
           10  H-ESCHEAT-AMT               PIC S9(13)V99 COMP-3.
           10  H-ESCHEAT-YTD               PIC S9(13)V99 COMP-3.
           10  H-VOIDED-AMT                PIC S9(13)V99 COMP-3.
           10  H-VOIDED-YTD                PIC S9(13)V99 COMP-3.
           10  H-LAST-PAY-DATE             PIC X(10).
           10  H-LAST-STMT-DATE            PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-NOTICE-DATE               PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-ITEM-TYPE-CD              PIC X(01).
           10  H-PAYEE-TYPE-CD             PIC X(02).
           10  H-REASON-CD                 PIC X(01).
           10  H-TYPE-CD                   PIC X(03).
           10  H-CHANNEL-CD                PIC X(02).
           10  H-CYCLE-CD                  PIC X(03).
           10  H-PREFERRED-FLAG            PIC X(12).
           10  H-OVERRIDE-FLAG             PIC X(12).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-STOP-FLAG                 PIC X(12).
           10  H-NOTICE-CNT                PIC S9(9) COMP.
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-DISCOUNT                  PIC S9(3)V9(6) COMP-3.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      *================================================================*
