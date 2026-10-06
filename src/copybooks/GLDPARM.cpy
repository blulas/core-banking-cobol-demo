      *================================================================*
      * COPYBOOK : GLDPARM                                             *
      * PURPOSE  : LINKAGE FOR GLDERIVE - GL ACCOUNT DERIVATION        *
      *   FUNCTION T = DERIVE DEBIT/CREDIT GL FOR A POSTED TRANSACTION *
      *   FUNCTION C = DEPOSIT LIABILITY GL FOR AN ACCOUNT CATEGORY    *
      *                (SUB-LEDGER MAPPING, USED BY RECONCILIATION)    *
      *================================================================*
       01  GL-DERIVE-PARMS.
           05  GLD-FUNCTION                PIC X(01).
               88  GLD-FN-TRANSACTION                VALUE 'T'.
               88  GLD-FN-CATEGORY                   VALUE 'C'.
           05  GLD-TRAN-CODE               PIC X(04).
           05  GLD-DR-CR                   PIC X(01).
           05  GLD-PRODUCT-CODE            PIC X(04).
           05  GLD-ACCT-CATEGORY           PIC X(02).
           05  GLD-OWNERSHIP-TYPE          PIC X(01).
           05  GLD-COST-CENTER             PIC X(05).
      *    TRAN-GL-ACCOUNT FROM THE POSTING PROGRAM (OPTIONAL)
           05  GLD-OVERRIDE-GL             PIC X(10).
      *--- OUTPUTS ----------------------------------------------------*
           05  GLD-CUST-GL                 PIC X(10).
           05  GLD-OFFSET-GL               PIC X(10).
           05  GLD-OFFSET-CC               PIC X(05).
           05  GLD-DEBIT-GL                PIC X(10).
           05  GLD-DEBIT-CC                PIC X(05).
           05  GLD-CREDIT-GL               PIC X(10).
           05  GLD-CREDIT-CC               PIC X(05).
           05  GLD-SUSPENSE-FLAG           PIC X(01).
               88  GLD-TO-SUSPENSE                   VALUE 'Y'.
           05  GLD-REASON-CODE             PIC X(04).
           05  GLD-RETURN-CODE             PIC 9(02).
