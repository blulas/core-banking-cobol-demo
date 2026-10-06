      *================================================================*
      * COPYBOOK : CTRPARM                                             *
      * PURPOSE  : LINKAGE FOR CTRRULE - CURRENCY TRANSACTION REPORT   *
      *            DETERMINATION FOR ONE PERSON FOR ONE BUSINESS DAY.  *
      *   CALLER PASSES EVERY CASH TRANSACTION CONDUCTED BY OR ON      *
      *   BEHALF OF THE CUSTOMER (ALL ACCOUNTS) FOR THE BUSINESS DAY.  *
      *   RETURN CODE 00 = EVALUATED (SEE CTP-CTR-REQUIRED)            *
      *               12 = INVALID INPUT (NO ITEMS / BAD DATE)         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-04-01 RJT  ORIGINAL                                       *
      * 2002-10-21 BHS  ITEM TABLE - AGGREGATE ACROSS ACCOUNTS         *
      * 2019-10-07 AKW  MSB INELIGIBLE FOR EXEMPTION                   *
      *================================================================*
       01  CTR-RULE-PARMS.
      *--- INPUT -----------------------------------------------------*
           05  CTP-BUSINESS-DATE           PIC 9(08).
           05  CTP-CUST-ID                 PIC X(10).
           05  CTP-CUST-TYPE               PIC X(01).
           05  CTP-CTR-EXEMPT-FLAG         PIC X(01).
           05  CTP-MSB-FLAG                PIC X(01).
           05  CTP-ITEM-COUNT              PIC 9(03).
           05  CTP-ITEM OCCURS 100 TIMES.
               10  CTP-ITEM-ACCT           PIC X(12).
               10  CTP-ITEM-DR-CR          PIC X(01).
               10  CTP-ITEM-AMOUNT         PIC S9(13)V99 COMP-3.
               10  CTP-ITEM-BRANCH         PIC X(05).
      *--- OUTPUT ----------------------------------------------------*
           05  CTP-CASH-IN-TOTAL           PIC S9(13)V99 COMP-3.
           05  CTP-CASH-OUT-TOTAL          PIC S9(13)V99 COMP-3.
           05  CTP-CASH-IN-COUNT           PIC 9(03).
           05  CTP-CASH-OUT-COUNT          PIC 9(03).
           05  CTP-CTR-REQUIRED            PIC X(01).
               88  CTP-CTR-IS-REQUIRED               VALUE 'Y'.
           05  CTP-CASH-IN-REPORTABLE      PIC X(01).
           05  CTP-CASH-OUT-REPORTABLE     PIC X(01).
           05  CTP-OVER-THRESHOLD          PIC X(01).
           05  CTP-AGGREGATED-FLAG         PIC X(01).
           05  CTP-MULTI-BRANCH-FLAG       PIC X(01).
           05  CTP-EXEMPT-STATUS           PIC X(01).
               88  CTP-NOT-EXEMPT                    VALUE 'N'.
               88  CTP-EXEMPT                        VALUE 'E'.
               88  CTP-EXEMPT-INELIGIBLE             VALUE 'I'.
           05  CTP-EXEMPT-REVIEW-FLAG      PIC X(01).
           05  CTP-EXEMPT-REVIEW-NOTE      PIC X(60).
           05  CTP-FILING-DUE-DATE         PIC 9(08).
           05  CTP-ACCOUNT-COUNT           PIC 9(02).
           05  CTP-DECISION-TEXT           PIC X(60).
           05  CTP-RETURN-CODE             PIC 9(02).
