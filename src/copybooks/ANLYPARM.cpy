      *================================================================*
      * COPYBOOK : ANLYPARM                                            *
      * PURPOSE  : LINKAGE FOR ANLYSCAL - ACCOUNT ANALYSIS CHARGE      *
      *   SERVICE INDEX (FIXED ORDER, SEE ANLYSCAL PRICE TABLE)        *
      *     1 ACH ITEMS          4 CHECKS PAID        7 VIRTUAL ACCTS  *
      *     2 WIRES OUT          5 DEPOSITS           8 ZBA ACCOUNTS   *
      *     3 WIRES IN           6 ACCOUNT MAINT      9 SWEEPS         *
      *   CARRY-FORWARD BUCKET 1 = LAST MONTH ... 3 = OLDEST           *
      *================================================================*
       01  ANALYSIS-CALC-PARMS.
           05  ANL-SEGMENT                 PIC X(02).
           05  ANL-MSB-FLAG                PIC X(01).
           05  ANL-EARNINGS-CREDIT         PIC S9(11)V99 COMP-3.
           05  ANL-SERVICE OCCURS 9 TIMES.
               10  ANL-SVC-CODE            PIC X(04).
               10  ANL-SVC-VOLUME          PIC 9(07).
               10  ANL-SVC-UNIT-PRICE      PIC 9(05)V9(04).
               10  ANL-SVC-CHARGE          PIC S9(09)V99 COMP-3.
           05  ANL-CF-IN                   PIC S9(11)V99 COMP-3
                                           OCCURS 3 TIMES.
      *--- OUTPUTS ----------------------------------------------------*
           05  ANL-TOTAL-SVC-CHARGE        PIC S9(11)V99 COMP-3.
           05  ANL-CF-APPLIED              PIC S9(11)V99 COMP-3.
           05  ANL-CREDIT-APPLIED          PIC S9(11)V99 COMP-3.
           05  ANL-NET-CHARGE              PIC S9(11)V99 COMP-3.
           05  ANL-EXCESS-CREDIT           PIC S9(11)V99 COMP-3.
           05  ANL-CREDIT-FORFEITED        PIC S9(11)V99 COMP-3.
           05  ANL-CF-OUT                  PIC S9(11)V99 COMP-3
                                           OCCURS 3 TIMES.
           05  ANL-CF-EXPIRED              PIC S9(11)V99 COMP-3.
           05  ANL-WAIVED-FLAG             PIC X(01).
               88  ANL-CHARGE-WAIVED                 VALUE 'Y'.
           05  ANL-RETURN-CODE             PIC 9(02).
