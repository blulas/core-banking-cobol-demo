      *================================================================*
      * COPYBOOK : SLLIMP                                              *
      * PURPOSE  : LINKAGE FOR STUDENT LOANS RULE MODULE SLLIM         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-04-10 DPS  ORIGINAL                                       *
      * 1990-09-05 TWH  AUDIT FINDING - SUBSIDY CONTROL TOTALS PRB7560 *
      * 1991-05-19 JLM  SUBSIDY FEE SCHEDULE UPDATE CHG7684            *
      * 1995-04-03 EWS  AUDIT FINDING - DISBURSE CONTROL TOTALS INC1557*
      * 1995-02-05 JLM  PRINCIPAL ROUNDING - HALF UP INC3863           *
      * 1995-05-17 EWS  DEFERINT ROUNDING - HALF UP CHG6243            *
      * 1996-01-18 RJT  CAPINT CALCULATION CORRECTED REQ0932           *
      * 2002-02-18 GAF  PRINCIPAL NOTICE TEXT PER LEGAL REQ8173        *
      * 2002-03-14 MKL  DISBURSE CUTOFF MOVED REQ1370                  *
      * 2002-12-02 SKN  DISBURSE NOTICE TEXT PER LEGAL CHG8095         *
      * 2003-02-22 TWH  SUBSIDY LIMIT REVISED REQ3541                  *
      * 2005-12-20 TWH  CAPINT CALCULATION CORRECTED CHG5437           *
      *================================================================*
       01  SLLIM-PARMS.
           05  SLLIM-CAPINT-AMT            PIC S9(13)V99 COMP-3.
           05  SLLIM-DISBURSE-YTD          PIC S9(13)V99 COMP-3.
           05  SLLIM-SUBSIDY-YTD           PIC S9(13)V99 COMP-3.
           05  SLLIM-PAYMENT-YTD           PIC S9(13)V99 COMP-3.
           05  SLLIM-PRINCIPAL-YTD         PIC S9(13)V99 COMP-3.
           05  SLLIM-PRINCIPAL-AMT         PIC S9(13)V99 COMP-3.
           05  SLLIM-MATURITY-DATE         PIC 9(08).
           05  SLLIM-CLOSE-DATE            PIC 9(08).
           05  SLLIM-LAST-STMT-DATE        PIC 9(08).
           05  SLLIM-EXPIRY-DATE           PIC 9(08).
           05  SLLIM-LOAN-TYPE-CD          PIC X(02).
               88  SLLIM-LOAN-TYPE-CD-CS           VALUE 'CS'.
               88  SLLIM-LOAN-TYPE-CD-DV           VALUE 'DV'.
               88  SLLIM-LOAN-TYPE-CD-JL           VALUE 'JL'.
               88  SLLIM-LOAN-TYPE-CD-NE           VALUE 'NE'.
               88  SLLIM-LOAN-TYPE-CD-SH           VALUE 'SH'.
           05  SLLIM-SOURCE-CD             PIC X(03).
               88  SLLIM-SOURCE-CD-AST             VALUE 'AST'.
               88  SLLIM-SOURCE-CD-BCK             VALUE 'BCK'.
               88  SLLIM-SOURCE-CD-CJG             VALUE 'CJG'.
               88  SLLIM-SOURCE-CD-DSJ             VALUE 'DSJ'.
               88  SLLIM-SOURCE-CD-JVY             VALUE 'JVY'.
               88  SLLIM-SOURCE-CD-VEF             VALUE 'VEF'.
           05  SLLIM-REGION-CD             PIC X(01).
               88  SLLIM-REGION-CD-C               VALUE 'C'.
               88  SLLIM-REGION-CD-D               VALUE 'D'.
               88  SLLIM-REGION-CD-T               VALUE 'T'.
               88  SLLIM-REGION-CD-U               VALUE 'U'.
               88  SLLIM-REGION-CD-W               VALUE 'W'.
               88  SLLIM-REGION-CD-Y               VALUE 'Y'.
           05  SLLIM-CYCLE-CD              PIC X(03).
               88  SLLIM-CYCLE-CD-CFJ              VALUE 'CFJ'.
               88  SLLIM-CYCLE-CD-KHC              VALUE 'KHC'.
               88  SLLIM-CYCLE-CD-SEU              VALUE 'SEU'.
               88  SLLIM-CYCLE-CD-XKA              VALUE 'XKA'.
           05  SLLIM-NOTICE-SENT-FLAG      PIC X(01).
               88  SLLIM-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  SLLIM-HOLD-FLAG             PIC X(01).
               88  SLLIM-HOLD-FLAG-YES             VALUE 'Y'.
           05  SLLIM-REVIEW-FLAG           PIC X(01).
               88  SLLIM-REVIEW-FLAG-YES           VALUE 'Y'.
           05  SLLIM-PAYMENT-CNT           PIC 9(07).
           05  SLLIM-ITEM-CNT              PIC 9(07).
           05  SLLIM-DISCOUNT              PIC 9(03)V9(06).
           05  SLLIM-FEE-PCT               PIC 9(03)V9(06).
           05  SLLIM-CONTACT               PIC X(20).
           05  SLLIM-NAME                  PIC X(30).
           05  SLLIM-RETURN-CODE           PIC 9(02).
