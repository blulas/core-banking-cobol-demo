      *================================================================*
      * COPYBOOK : SLCALP                                              *
      * PURPOSE  : LINKAGE FOR STUDENT LOANS RULE MODULE SLCAL         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-12-07 PQR  ORIGINAL                                       *
      * 1992-01-09 SKN  PAYMENT CALCULATION CORRECTED INC0397          *
      * 1995-03-18 RJT  CAPINT CALCULATION CORRECTED REQ6785           *
      * 1995-10-24 GAF  DEFERINT LIMIT REVISED INC7944                 *
      * 1997-05-08 PQR  DEFERINT STATUS CODES ADDED INC7042            *
      * 1998-01-18 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-05-25 BHS  PERFORMANCE - DEFERINT READ AHEAD CHG5741      *
      * 2003-12-14 DPS  AUDIT FINDING - DISBURSE CONTROL TOTALS INC3816*
      * 2006-01-19 GAF  AUDIT FINDING - DEFERINT CONTROL TOTALS INC8576*
      *================================================================*
       01  SLCAL-PARMS.
           05  SLCAL-CAPINT-AMT            PIC S9(13)V99 COMP-3.
           05  SLCAL-SUBSIDY-YTD           PIC S9(13)V99 COMP-3.
           05  SLCAL-PAYMENT-AMT           PIC S9(13)V99 COMP-3.
           05  SLCAL-PRINCIPAL-AMT         PIC S9(13)V99 COMP-3.
           05  SLCAL-DEFERINT-YTD          PIC S9(13)V99 COMP-3.
           05  SLCAL-DISBURSE-YTD          PIC S9(13)V99 COMP-3.
           05  SLCAL-OPEN-DATE             PIC 9(08).
           05  SLCAL-EXPIRY-DATE           PIC 9(08).
           05  SLCAL-LAST-ACT-DATE         PIC 9(08).
           05  SLCAL-LAST-PAY-DATE         PIC 9(08).
           05  SLCAL-CYCLE-CD              PIC X(03).
               88  SLCAL-CYCLE-CD-CFJ              VALUE 'CFJ'.
               88  SLCAL-CYCLE-CD-KHC              VALUE 'KHC'.
               88  SLCAL-CYCLE-CD-SEU              VALUE 'SEU'.
               88  SLCAL-CYCLE-CD-XKA              VALUE 'XKA'.
           05  SLCAL-DEFER-CODE-CD         PIC X(02).
               88  SLCAL-DEFER-CODE-CD-AH          VALUE 'AH'.
               88  SLCAL-DEFER-CODE-CD-CZ          VALUE 'CZ'.
               88  SLCAL-DEFER-CODE-CD-GM          VALUE 'GM'.
               88  SLCAL-DEFER-CODE-CD-ND          VALUE 'ND'.
               88  SLCAL-DEFER-CODE-CD-UC          VALUE 'UC'.
           05  SLCAL-SOURCE-CD             PIC X(03).
               88  SLCAL-SOURCE-CD-AST             VALUE 'AST'.
               88  SLCAL-SOURCE-CD-BCK             VALUE 'BCK'.
               88  SLCAL-SOURCE-CD-CJG             VALUE 'CJG'.
               88  SLCAL-SOURCE-CD-DSJ             VALUE 'DSJ'.
               88  SLCAL-SOURCE-CD-JVY             VALUE 'JVY'.
               88  SLCAL-SOURCE-CD-VEF             VALUE 'VEF'.
           05  SLCAL-REGION-CD             PIC X(01).
               88  SLCAL-REGION-CD-C               VALUE 'C'.
               88  SLCAL-REGION-CD-D               VALUE 'D'.
               88  SLCAL-REGION-CD-T               VALUE 'T'.
               88  SLCAL-REGION-CD-U               VALUE 'U'.
               88  SLCAL-REGION-CD-W               VALUE 'W'.
               88  SLCAL-REGION-CD-Y               VALUE 'Y'.
           05  SLCAL-REVIEW-FLAG           PIC X(01).
               88  SLCAL-REVIEW-FLAG-YES           VALUE 'Y'.
           05  SLCAL-NOTICE-SENT-FLAG      PIC X(01).
               88  SLCAL-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  SLCAL-OVERRIDE-FLAG         PIC X(01).
               88  SLCAL-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  SLCAL-EXCEPT-CNT            PIC 9(07).
           05  SLCAL-PAYMENT-CNT           PIC 9(07).
           05  SLCAL-PENALTY-PCT           PIC 9(03)V9(06).
           05  SLCAL-FEE-PCT               PIC 9(03)V9(06).
           05  SLCAL-NAME                  PIC X(30).
           05  SLCAL-ADDR                  PIC X(30).
           05  SLCAL-RETURN-CODE           PIC 9(02).
