      *================================================================*
      * DCLGEN TABLE(CBS.PASSBOOK_PRINTING)                            *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLPBK))                       *
      *        LANGUAGE(COBOL) QUOTE                                   *
      *================================================================*
           EXEC SQL DECLARE CBS.PASSBOOK_PRINTING TABLE
           ( KEY                          CHAR(12) NOT NULL,
             CUST_ID                      CHAR(10) NOT NULL,
             NAME                         CHAR(30) NOT NULL,
             DESC                         CHAR(30) NOT NULL,
             REF_NO                       CHAR(16) NOT NULL,
             ADDR                         CHAR(30) NOT NULL,
             CONTACT                      CHAR(20) NOT NULL,
             LINE_AMT                     DECIMAL(15, 2) NOT NULL,
             LINE_YTD                     DECIMAL(15, 2) NOT NULL,
             UNPOSTED_AMT                 DECIMAL(15, 2) NOT NULL,
             UNPOSTED_YTD                 DECIMAL(15, 2) NOT NULL,
             PRINTED_AMT                  DECIMAL(15, 2) NOT NULL,
             PRINTED_YTD                  DECIMAL(15, 2) NOT NULL,
             CARRYFWD_AMT                 DECIMAL(15, 2) NOT NULL,
             CARRYFWD_YTD                 DECIMAL(15, 2) NOT NULL,
             INTEREST_AMT                 DECIMAL(15, 2) NOT NULL,
             INTEREST_YTD                 DECIMAL(15, 2) NOT NULL,
             BALANCE_AMT                  DECIMAL(15, 2) NOT NULL,
             BALANCE_YTD                  DECIMAL(15, 2) NOT NULL,
             MATURITY_DATE                DATE NOT NULL,
             CLOSE_DATE                   DATE NOT NULL,
             LAST_MAINT_DATE              DATE NOT NULL,
             EFFECTIVE_DATE               DATE NOT NULL,
             LAST_ACT_DATE                DATE NOT NULL,
             EXPIRY_DATE                  DATE NOT NULL,
             NEXT_DUE_DATE                DATE NOT NULL,
             NEXT_REVIEW_DATE             DATE NOT NULL,
             OPEN_DATE                    DATE NOT NULL,
             BOOK_TYPE_CD                 CHAR(2) NOT NULL,
             PRINTER_CD                   CHAR(2) NOT NULL,
             BRANCH_CD                    CHAR(2) NOT NULL,
             SOURCE_CD                    CHAR(2) NOT NULL,
             CHANNEL_CD                   CHAR(1) NOT NULL,
             REGION_CD                    CHAR(2) NOT NULL,
             EXEMPT_FLAG                  CHAR(12) NOT NULL,
             NOTICE_SENT_FLAG             CHAR(12) NOT NULL,
             AUTO_RENEW_FLAG              CHAR(12) NOT NULL,
             SUSPEND_FLAG                 CHAR(12) NOT NULL,
             HOLD_FLAG                    CHAR(12) NOT NULL,
             REVIEW_FLAG                  CHAR(12) NOT NULL,
             CYCLE_CNT                    INTEGER NOT NULL,
             RETRY_CNT                    INTEGER NOT NULL,
             PAYMENT_CNT                  INTEGER NOT NULL,
             ITEM_CNT                     INTEGER NOT NULL,
             PENALTY_PCT                  DECIMAL(9, 6) NOT NULL,
             MARGIN                       DECIMAL(9, 6) NOT NULL,
             FEE_PCT                      DECIMAL(9, 6) NOT NULL
           ) END-EXEC.
      *================================================================*
      * COBOL DECLARATION FOR TABLE CBS.PASSBOOK_PRINTING              *
      *================================================================*
       01  DCLPASSBOOK-PRINTING.
           10  H-KEY                       PIC X(12).
           10  H-CUST-ID                   PIC X(10).
           10  H-NAME                      PIC X(30).
           10  H-DESC                      PIC X(30).
           10  H-REF-NO                    PIC X(16).
           10  H-ADDR                      PIC X(30).
           10  H-CONTACT                   PIC X(20).
           10  H-LINE-AMT                  PIC S9(13)V99 COMP-3.
           10  H-LINE-YTD                  PIC S9(13)V99 COMP-3.
           10  H-UNPOSTED-AMT              PIC S9(13)V99 COMP-3.
           10  H-UNPOSTED-YTD              PIC S9(13)V99 COMP-3.
           10  H-PRINTED-AMT               PIC S9(13)V99 COMP-3.
           10  H-PRINTED-YTD               PIC S9(13)V99 COMP-3.
           10  H-CARRYFWD-AMT              PIC S9(13)V99 COMP-3.
           10  H-CARRYFWD-YTD              PIC S9(13)V99 COMP-3.
           10  H-INTEREST-AMT              PIC S9(13)V99 COMP-3.
           10  H-INTEREST-YTD              PIC S9(13)V99 COMP-3.
           10  H-BALANCE-AMT               PIC S9(13)V99 COMP-3.
           10  H-BALANCE-YTD               PIC S9(13)V99 COMP-3.
           10  H-MATURITY-DATE             PIC X(10).
           10  H-CLOSE-DATE                PIC X(10).
           10  H-LAST-MAINT-DATE           PIC X(10).
           10  H-EFFECTIVE-DATE            PIC X(10).
           10  H-LAST-ACT-DATE             PIC X(10).
           10  H-EXPIRY-DATE               PIC X(10).
           10  H-NEXT-DUE-DATE             PIC X(10).
           10  H-NEXT-REVIEW-DATE          PIC X(10).
           10  H-OPEN-DATE                 PIC X(10).
           10  H-BOOK-TYPE-CD              PIC X(02).
           10  H-PRINTER-CD                PIC X(02).
           10  H-BRANCH-CD                 PIC X(02).
           10  H-SOURCE-CD                 PIC X(02).
           10  H-CHANNEL-CD                PIC X(01).
           10  H-REGION-CD                 PIC X(02).
           10  H-EXEMPT-FLAG               PIC X(12).
           10  H-NOTICE-SENT-FLAG          PIC X(12).
           10  H-AUTO-RENEW-FLAG           PIC X(12).
           10  H-SUSPEND-FLAG              PIC X(12).
           10  H-HOLD-FLAG                 PIC X(12).
           10  H-REVIEW-FLAG               PIC X(12).
           10  H-CYCLE-CNT                 PIC S9(9) COMP.
           10  H-RETRY-CNT                 PIC S9(9) COMP.
           10  H-PAYMENT-CNT               PIC S9(9) COMP.
           10  H-ITEM-CNT                  PIC S9(9) COMP.
           10  H-PENALTY-PCT               PIC S9(3)V9(6) COMP-3.
           10  H-MARGIN                    PIC S9(3)V9(6) COMP-3.
           10  H-FEE-PCT                   PIC S9(3)V9(6) COMP-3.
      *================================================================*
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 47      *
      *================================================================*
