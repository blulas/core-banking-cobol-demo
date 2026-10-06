      ******************************************************************
      * DCLGEN TABLE(CBS.TRAN_HISTORY)                                 *
      *        LIBRARY(CBS.PROD.COPYLIB(DCLTRNH))                      *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE CBS.TRAN_HISTORY TABLE
           ( TRAN_ID                        CHAR(16) NOT NULL,
             ACCT_NUMBER                    CHAR(12) NOT NULL,
             EFFECTIVE_DATE                 DATE NOT NULL,
             POST_DATE                      DATE NOT NULL,
             TRAN_CODE                      CHAR(4) NOT NULL,
             DR_CR                          CHAR(1) NOT NULL,
             AMOUNT                         DECIMAL(15, 2) NOT NULL,
             RUNNING_BALANCE                DECIMAL(15, 2),
             CHANNEL                        CHAR(3) NOT NULL,
             DESCRIPTION                    VARCHAR(40),
             REFERENCE                      CHAR(20),
             COUNTERPARTY_NAME              VARCHAR(35),
             STATUS                         CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE CBS.TRAN_HISTORY                   *
      ******************************************************************
       01  DCLTRAN-HISTORY.
           10 TH-TRAN-ID           PIC X(16).
           10 TH-ACCT-NUMBER       PIC X(12).
           10 TH-EFFECTIVE-DATE    PIC X(10).
           10 TH-POST-DATE         PIC X(10).
           10 TH-TRAN-CODE         PIC X(4).
           10 TH-DR-CR             PIC X(1).
           10 TH-AMOUNT            PIC S9(13)V9(2) USAGE COMP-3.
           10 TH-RUNNING-BALANCE   PIC S9(13)V9(2) USAGE COMP-3.
           10 TH-CHANNEL           PIC X(3).
           10 TH-DESCRIPTION.
              49 TH-DESCRIPTION-LEN   PIC S9(4) USAGE COMP.
              49 TH-DESCRIPTION-TEXT  PIC X(40).
           10 TH-REFERENCE         PIC X(20).
           10 TH-COUNTERPARTY-NAME.
              49 TH-COUNTERPARTY-NAME-LEN  PIC S9(4) USAGE COMP.
              49 TH-COUNTERPARTY-NAME-TEXT PIC X(35).
           10 TH-STATUS            PIC X(1).
      ******************************************************************
      * INDICATOR VARIABLES                                            *
      ******************************************************************
       01  ITRAN-HISTORY.
           10 TH-RUNNING-BAL-IND   PIC S9(4) USAGE COMP.
           10 TH-DESCRIPTION-IND   PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
