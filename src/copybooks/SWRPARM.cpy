      *================================================================*
      * COPYBOOK : SWRPARM                                             *
      * PURPOSE  : LINKAGE FOR SWEEPRUL - SWEEP AMOUNT AND DIRECTION   *
      *   CALLER PASSES THE SWEEP DEFINITION VALUES AND THE CURRENT    *
      *   BALANCES OF THE SOURCE AND TARGET (PARENT/INVESTMENT) ACCTS. *
      *   DIRECTION O = SOURCE -> TARGET (CONCENTRATE / INVEST)        *
      *             I = TARGET -> SOURCE (FUND / REDEEM)               *
      *             N = NO SWEEP                                       *
      *================================================================*
       01  SWEEP-RULE-PARMS.
           05  SWR-SWEEP-TYPE              PIC X(01).
               88  SWR-TYPE-ZBA                      VALUE 'Z'.
               88  SWR-TYPE-TARGET                   VALUE 'T'.
               88  SWR-TYPE-RANGE                    VALUE 'R'.
           05  SWR-SRC-LEDGER-BAL          PIC S9(13)V99 COMP-3.
           05  SWR-SRC-AVAIL-BAL           PIC S9(13)V99 COMP-3.
           05  SWR-SRC-HOLD-AMT            PIC S9(13)V99 COMP-3.
           05  SWR-TGT-PRODUCT-CODE        PIC X(04).
           05  SWR-TGT-AVAIL-BAL           PIC S9(13)V99 COMP-3.
           05  SWR-TARGET-BAL              PIC S9(13)V99 COMP-3.
           05  SWR-MIN-BAL                 PIC S9(13)V99 COMP-3.
           05  SWR-MAX-BAL                 PIC S9(13)V99 COMP-3.
           05  SWR-INCREMENT               PIC S9(09)V99 COMP-3.
           05  SWR-MIN-TRANSFER            PIC S9(09)V99 COMP-3.
      *--- OUTPUTS ----------------------------------------------------*
           05  SWR-SWEEP-AMOUNT            PIC S9(13)V99 COMP-3.
           05  SWR-REQUIRED-AMT            PIC S9(13)V99 COMP-3.
           05  SWR-EFF-INCREMENT           PIC S9(09)V99 COMP-3.
           05  SWR-EFF-MIN-TRANSFER        PIC S9(09)V99 COMP-3.
           05  SWR-DIRECTION               PIC X(01).
               88  SWR-DIR-OUT                       VALUE 'O'.
               88  SWR-DIR-IN                        VALUE 'I'.
               88  SWR-DIR-NONE                      VALUE 'N'.
           05  SWR-PARTIAL-FLAG            PIC X(01).
               88  SWR-PARTIAL                       VALUE 'Y'.
           05  SWR-ALERT-FLAG              PIC X(01).
               88  SWR-RAISE-ALERT                   VALUE 'Y'.
           05  SWR-REASON-CODE             PIC X(04).
           05  SWR-REASON-TEXT             PIC X(40).
           05  SWR-RETURN-CODE             PIC 9(02).
