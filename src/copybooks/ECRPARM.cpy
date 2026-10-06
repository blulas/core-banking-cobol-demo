      *================================================================*
      * COPYBOOK : ECRPARM                                             *
      * PURPOSE  : LINKAGE FOR ECRCALC - EARNINGS CREDIT ALLOWANCE     *
      *            FOR ACCOUNT ANALYSIS                                *
      *================================================================*
       01  ECR-CALC-PARMS.
           05  ECR-AVG-COLLECTED-BAL       PIC S9(13)V99 COMP-3.
           05  ECR-BASE-RATE               PIC 9(03)V9(06).
           05  ECR-FED-FUNDS-RATE          PIC 9(03)V9(06).
           05  ECR-DAYS-IN-PERIOD          PIC 9(03).
           05  ECR-MSB-FLAG                PIC X(01).
               88  ECR-CUST-IS-MSB                   VALUE 'Y'.
      *--- OUTPUTS ----------------------------------------------------*
           05  ECR-TIER-CODE               PIC X(01).
           05  ECR-TIER-ADJ                PIC 9(03)V9(06).
           05  ECR-RATE                    PIC 9(03)V9(06).
           05  ECR-RESERVE-PCT             PIC 9(03)V9(06).
           05  ECR-INVESTABLE-BAL          PIC S9(13)V99 COMP-3.
           05  ECR-DAY-BASIS               PIC 9(03).
           05  ECR-EARNINGS-CREDIT         PIC S9(11)V99 COMP-3.
           05  ECR-REASON-CODE             PIC X(04).
           05  ECR-RETURN-CODE             PIC 9(02).
