      *================================================================*
      * COPYBOOK : POOLPARM                                            *
      * PURPOSE  : LINKAGE FOR POOLALOC - NOTIONAL POOL INTEREST       *
      *   MEMBER TABLE: HEADER ACCOUNT PLUS UP TO 20 MEMBERS. THE      *
      *   HEADER IS IDENTIFIED BY PLA-MBR-HEADER-FLAG = 'Y'.           *
      *================================================================*
       01  POOL-ALLOC-PARMS.
           05  PLA-ALLOC-METHOD            PIC X(01).
               88  PLA-PROPORTIONAL                  VALUE 'P'.
               88  PLA-TO-HEADER                     VALUE 'H'.
           05  PLA-REF-CREDIT-RATE         PIC 9(03)V9(06).
           05  PLA-REF-DEBIT-RATE          PIC 9(03)V9(06).
           05  PLA-CREDIT-SPREAD           PIC 9(03)V9(06).
           05  PLA-DEBIT-SPREAD            PIC 9(03)V9(06).
           05  PLA-POOL-LIMIT              PIC S9(13)V99 COMP-3.
           05  PLA-DAYS                    PIC 9(03).
           05  PLA-MEMBER-COUNT            PIC 9(02).
           05  PLA-MEMBER OCCURS 21 TIMES.
               10  PLA-MBR-ACCT            PIC X(12).
               10  PLA-MBR-HEADER-FLAG     PIC X(01).
               10  PLA-MBR-AVG-BAL         PIC S9(13)V99 COMP-3.
               10  PLA-MBR-CREDIT-INT      PIC S9(11)V99 COMP-3.
               10  PLA-MBR-DEBIT-INT       PIC S9(11)V99 COMP-3.
      *--- OUTPUTS ----------------------------------------------------*
           05  PLA-CREDIT-RATE             PIC 9(03)V9(06).
           05  PLA-DEBIT-RATE              PIC 9(03)V9(06).
           05  PLA-GROSS-CREDIT-POS        PIC S9(13)V99 COMP-3.
           05  PLA-GROSS-DEBIT-POS         PIC S9(13)V99 COMP-3.
           05  PLA-NET-POSITION            PIC S9(13)V99 COMP-3.
           05  PLA-TOTAL-CREDIT-INT        PIC S9(11)V99 COMP-3.
           05  PLA-TOTAL-DEBIT-INT         PIC S9(11)V99 COMP-3.
           05  PLA-LIMIT-BREACH-FLAG       PIC X(01).
               88  PLA-LIMIT-BREACHED                VALUE 'Y'.
           05  PLA-REASON-CODE             PIC X(04).
           05  PLA-RETURN-CODE             PIC 9(02).
