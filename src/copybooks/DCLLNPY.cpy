      ******************************************************************
      * DCLGEN TABLE(CBS.LOAN_PAYMENT_HIST)                            *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLLNPY))                      *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(LPY-)                                             *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE CBS.LOAN_PAYMENT_HIST TABLE
           ( LOAN_NUMBER                    CHAR(12) NOT NULL,
             PAYMENT_DATE                   DATE NOT NULL,
             TRAN_ID                        CHAR(16) NOT NULL,
             SOURCE_ACCT                    CHAR(12) NOT NULL,
             PAYMENT_AMT                    DECIMAL(15, 2) NOT NULL,
             APPLIED_FEES                   DECIMAL(9, 2) NOT NULL,
             APPLIED_INT                    DECIMAL(13, 2) NOT NULL,
             APPLIED_PRIN                   DECIMAL(15, 2) NOT NULL,
             APPLIED_SUSPENSE               DECIMAL(13, 2) NOT NULL,
             PRIN_BAL_AFTER                 DECIMAL(15, 2) NOT NULL,
             WATERFALL                      CHAR(1) NOT NULL,
             PAYOFF_FLAG                    CHAR(1) NOT NULL,
             CHANNEL                        CHAR(3) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE CBS.LOAN_PAYMENT_HIST              *
      ******************************************************************
       01  DCLLOAN-PAYMENT-HIST.
           10 LPY-LOAN-NUMBER      PIC X(12).
           10 LPY-PAYMENT-DATE     PIC X(10).
           10 LPY-TRAN-ID          PIC X(16).
           10 LPY-SOURCE-ACCT      PIC X(12).
           10 LPY-PAYMENT-AMT      PIC S9(13)V9(2) USAGE COMP-3.
           10 LPY-APPLIED-FEES     PIC S9(7)V9(2) USAGE COMP-3.
           10 LPY-APPLIED-INT      PIC S9(11)V9(2) USAGE COMP-3.
           10 LPY-APPLIED-PRIN     PIC S9(13)V9(2) USAGE COMP-3.
           10 LPY-APPLIED-SUSPENSE PIC S9(11)V9(2) USAGE COMP-3.
           10 LPY-PRIN-BAL-AFTER   PIC S9(13)V9(2) USAGE COMP-3.
           10 LPY-WATERFALL        PIC X(1).
           10 LPY-PAYOFF-FLAG      PIC X(1).
           10 LPY-CHANNEL          PIC X(3).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
