      *================================================================*
      * COPYBOOK : EUVALP                                              *
      * PURPOSE  : LINKAGE FOR EURO CONVERSION RULE MODULE EUVAL       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-12-16 RJT  ORIGINAL                                       *
      * 2000-04-09 JLM  NEW FEE REPORT COLUMNS PRB9822                 *
      * 2001-09-19 RJT  FEE EXCEPTIONS TO OPS QUEUE REQ8448            *
      * 2002-05-19 DPS  ROUNDING RERUN PROTECTION INC4585              *
      * 2002-05-23 LMB  NATIONAL FEE SCHEDULE UPDATE INC2300           *
      * 2003-03-03 EWS  TRIANGULAR CALCULATION CORRECTED CHG3903       *
      * 2005-07-20 EWS  NATIONAL ROUNDING - HALF UP PRB8244            *
      * 2010-04-08 PQR  FEE RERUN PROTECTION REQ9938                   *
      *================================================================*
       01  EUVAL-PARMS.
           05  EUVAL-TRIANGULAR-YTD        PIC S9(13)V99 COMP-3.
           05  EUVAL-NATIONAL-YTD          PIC S9(13)V99 COMP-3.
           05  EUVAL-NATIONAL-AMT          PIC S9(13)V99 COMP-3.
           05  EUVAL-CONVERTED-AMT         PIC S9(13)V99 COMP-3.
           05  EUVAL-TRIANGULAR-AMT        PIC S9(13)V99 COMP-3.
           05  EUVAL-RESIDUAL-YTD          PIC S9(13)V99 COMP-3.
           05  EUVAL-LAST-MAINT-DATE       PIC 9(08).
           05  EUVAL-MATURITY-DATE         PIC 9(08).
           05  EUVAL-NEXT-REVIEW-DATE      PIC 9(08).
           05  EUVAL-NOTICE-DATE           PIC 9(08).
           05  EUVAL-CONV-METHOD-CD        PIC X(02).
               88  EUVAL-CONV-METHOD-CD-GL         VALUE 'GL'.
               88  EUVAL-CONV-METHOD-CD-JA         VALUE 'JA'.
               88  EUVAL-CONV-METHOD-CD-LD         VALUE 'LD'.
               88  EUVAL-CONV-METHOD-CD-MR         VALUE 'MR'.
               88  EUVAL-CONV-METHOD-CD-VC         VALUE 'VC'.
           05  EUVAL-REGION-CD             PIC X(03).
               88  EUVAL-REGION-CD-DTL             VALUE 'DTL'.
               88  EUVAL-REGION-CD-HYD             VALUE 'HYD'.
               88  EUVAL-REGION-CD-JUL             VALUE 'JUL'.
               88  EUVAL-REGION-CD-WSE             VALUE 'WSE'.
               88  EUVAL-REGION-CD-YAU             VALUE 'YAU'.
               88  EUVAL-REGION-CD-YJB             VALUE 'YJB'.
           05  EUVAL-CURRENCY-CD           PIC X(02).
               88  EUVAL-CURRENCY-CD-EW            VALUE 'EW'.
               88  EUVAL-CURRENCY-CD-FJ            VALUE 'FJ'.
               88  EUVAL-CURRENCY-CD-JC            VALUE 'JC'.
           05  EUVAL-STATUS-CD             PIC X(01).
           05  EUVAL-PREFERRED-FLAG        PIC X(01).
               88  EUVAL-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  EUVAL-OVERRIDE-FLAG         PIC X(01).
               88  EUVAL-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  EUVAL-EXEMPT-FLAG           PIC X(01).
               88  EUVAL-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  EUVAL-ITEM-CNT              PIC 9(07).
           05  EUVAL-PAYMENT-CNT           PIC 9(07).
           05  EUVAL-PENALTY-PCT           PIC 9(03)V9(06).
           05  EUVAL-FEE-PCT               PIC 9(03)V9(06).
           05  EUVAL-DESC                  PIC X(30).
           05  EUVAL-CONTACT               PIC X(20).
           05  EUVAL-RETURN-CODE           PIC 9(02).
