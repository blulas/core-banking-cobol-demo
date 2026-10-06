      *================================================================*
      * COPYBOOK : ESVALP                                              *
      * PURPOSE  : LINKAGE FOR MORTGAGE ESCROW RULE MODULE ESVAL       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-10-15 JLM  ORIGINAL                                       *
      * 1995-06-27 CRP  AUDIT FINDING - DISBURSE CONTROL TOTALS INC0374*
      * 1995-09-02 RJT  AUDIT FINDING - SHORTAGE CONTROL TOTALS CHG5613*
      * 2000-03-13 TWH  AUDIT FINDING - SHORTAGE CONTROL TOTALS INC1899*
      * 2004-02-10 LMB  INSURANCE NOTICE TEXT PER LEGAL REQ8154        *
      * 2005-05-28 SKN  NEW DISBURSE REPORT COLUMNS INC5447            *
      * 2009-01-01 LMB  PAYMENT EXCEPTIONS TO OPS QUEUE REQ1524        *
      * 2010-09-22 CRP  PAYMENT STATUS CODES ADDED CHG0719             *
      *================================================================*
       01  ESVAL-PARMS.
           05  ESVAL-CUSHION-YTD           PIC S9(13)V99 COMP-3.
           05  ESVAL-SHORTAGE-YTD          PIC S9(13)V99 COMP-3.
           05  ESVAL-SURPLUS-YTD           PIC S9(13)V99 COMP-3.
           05  ESVAL-INSURANCE-AMT         PIC S9(13)V99 COMP-3.
           05  ESVAL-PAYMENT-YTD           PIC S9(13)V99 COMP-3.
           05  ESVAL-DISBURSE-YTD          PIC S9(13)V99 COMP-3.
           05  ESVAL-NEXT-DUE-DATE         PIC 9(08).
           05  ESVAL-CLOSE-DATE            PIC 9(08).
           05  ESVAL-EFFECTIVE-DATE        PIC 9(08).
           05  ESVAL-NOTICE-DATE           PIC 9(08).
           05  ESVAL-REGION-CD             PIC X(01).
               88  ESVAL-REGION-CD-B               VALUE 'B'.
               88  ESVAL-REGION-CD-G               VALUE 'G'.
               88  ESVAL-REGION-CD-Y               VALUE 'Y'.
           05  ESVAL-ANALYSIS-CD           PIC X(02).
               88  ESVAL-ANALYSIS-CD-GD            VALUE 'GD'.
               88  ESVAL-ANALYSIS-CD-LY            VALUE 'LY'.
               88  ESVAL-ANALYSIS-CD-XJ            VALUE 'XJ'.
           05  ESVAL-STATUS-CD             PIC X(01).
               88  ESVAL-STATUS-CD-A               VALUE 'A'.
               88  ESVAL-STATUS-CD-E               VALUE 'E'.
               88  ESVAL-STATUS-CD-R               VALUE 'R'.
               88  ESVAL-STATUS-CD-U               VALUE 'U'.
               88  ESVAL-STATUS-CD-Z               VALUE 'Z'.
           05  ESVAL-REASON-CD             PIC X(01).
               88  ESVAL-REASON-CD-C               VALUE 'C'.
               88  ESVAL-REASON-CD-F               VALUE 'F'.
               88  ESVAL-REASON-CD-K               VALUE 'K'.
               88  ESVAL-REASON-CD-L               VALUE 'L'.
               88  ESVAL-REASON-CD-S               VALUE 'S'.
               88  ESVAL-REASON-CD-X               VALUE 'X'.
           05  ESVAL-SUSPEND-FLAG          PIC X(01).
               88  ESVAL-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  ESVAL-HOLD-FLAG             PIC X(01).
               88  ESVAL-HOLD-FLAG-YES             VALUE 'Y'.
           05  ESVAL-PREFERRED-FLAG        PIC X(01).
               88  ESVAL-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  ESVAL-CYCLE-CNT             PIC 9(07).
           05  ESVAL-RETRY-CNT             PIC 9(07).
           05  ESVAL-DISCOUNT              PIC 9(03)V9(06).
           05  ESVAL-RATE                  PIC 9(03)V9(06).
           05  ESVAL-CONTACT               PIC X(20).
           05  ESVAL-CUST-ID               PIC X(10).
           05  ESVAL-RETURN-CODE           PIC 9(02).
