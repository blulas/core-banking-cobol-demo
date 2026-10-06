      *================================================================*
      * COPYBOOK : CUTPARM                                             *
      * PURPOSE  : LINKAGE FOR CUTOFF - PAYMENT VALUE / SETTLEMENT     *
      *            DATE DETERMINATION                                  *
      * INPUT    : PAY TYPE, AMOUNT, REQUESTED DATE, TIME OF REQUEST   *
      *            (HHMM EASTERN), BUSINESS DATE AND THE BANKCTL       *
      *            CUTOFF TIMES (ZERO = MODULE DEFAULT)                *
      * OUTPUT   : PROCESS DATE  - DATE THE ACCOUNT IS DEBITED AND THE *
      *                            ITEM IS RELEASED TO THE NETWORK     *
      *            SETTLE DATE   - INTERBANK SETTLEMENT / NACHA        *
      *                            EFFECTIVE ENTRY DATE                *
      *            EFFECTIVE TYPE- MAY DOWNGRADE S (SDACH) TO A (ACH)  *
      * RETURN   : 00 AS REQUESTED  04 ROLLED / DOWNGRADED  12 ERROR   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL - WIRE CUTOFF                         *
      * 2016-09-23 DPS  SAME-DAY ACH                                   *
      *================================================================*
       01  CUTOFF-PARMS.
           05  CUT-PAY-TYPE                PIC X(01).
               88  CUT-BOOK                          VALUE 'B'.
               88  CUT-ACH                           VALUE 'A'.
               88  CUT-SDACH                         VALUE 'S'.
               88  CUT-WIRE                          VALUE 'W'.
               88  CUT-INTL-WIRE                     VALUE 'I'.
      *        ACH ONLY: C CREDIT ORIGINATION, D DEBIT ORIGINATION
           05  CUT-DR-CR                   PIC X(01).
           05  CUT-AMOUNT                  PIC S9(13)V99 COMP-3.
           05  CUT-REQUEST-DATE            PIC 9(08).
           05  CUT-REQUEST-TIME            PIC 9(04).
           05  CUT-BUSINESS-DATE           PIC 9(08).
           05  CUT-ACH-CUTOFF              PIC 9(04).
           05  CUT-SDACH-CUTOFF            PIC 9(04).
           05  CUT-WIRE-CUTOFF             PIC 9(04).
      *--- OUTPUT -----------------------------------------------------*
           05  CUT-EFFECTIVE-TYPE          PIC X(01).
           05  CUT-PROCESS-DATE            PIC 9(08).
           05  CUT-SETTLE-DATE             PIC 9(08).
           05  CUT-CUTOFF-APPLIED          PIC 9(04).
           05  CUT-ROLLED-FLAG             PIC X(01).
               88  CUT-WAS-ROLLED                    VALUE 'Y'.
      *        SAME  ON TIME          NBDR NON-BUSINESS DAY ROLL
      *        PCUT  PAST CUTOFF      SDLM SDACH OVER $1MM - TO ACH
      *        SDCT  SDACH CUTOFF MISSED - TO ACH
      *        BKDT  BACK-DATED REQUEST - MOVED TO BUSINESS DATE
           05  CUT-REASON-CODE             PIC X(04).
           05  CUT-REASON-TEXT             PIC X(40).
           05  CUT-RETURN-CODE             PIC 9(02).
