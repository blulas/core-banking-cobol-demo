      *================================================================*
      * COPYBOOK : TLRREC                                              *
      * PURPOSE  : TELLER / OPERATOR MASTER RECORD - ROLE, BRANCH AND  *
      *            CASH DRAWER POSITION. USED BY TELLTRN AND HOLDMNT.  *
      * FILE     : TELLMST  (VSAM KSDS, KEY = TLR-USER-ID, CICS FCT)   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-10-02 RJT  ORIGINAL                                       *
      * 2006-05-22 BHS  OVERRIDE LIMIT FOR SUPERVISORS                 *
      * 2008-02-11 BHS  OPS ROLE (BACK OFFICE - NO DRAWER)             *
      *================================================================*
       01  TELLER-RECORD.
           05  TLR-USER-ID                 PIC X(08).
           05  TLR-NAME                    PIC X(30).
           05  TLR-ROLE                    PIC X(03).
               88  TLR-ROLE-TELLER                   VALUE 'TLR'.
               88  TLR-ROLE-SENIOR-TELLER            VALUE 'SRT'.
               88  TLR-ROLE-SUPERVISOR               VALUE 'SUP'.
               88  TLR-ROLE-MANAGER                  VALUE 'MGR'.
               88  TLR-ROLE-OPERATIONS               VALUE 'OPS'.
           05  TLR-BRANCH                  PIC X(05).
           05  TLR-STATUS                  PIC X(01).
               88  TLR-ACTIVE                        VALUE 'A'.
               88  TLR-SUSPENDED                     VALUE 'S'.
           05  TLR-DRAWER-ID               PIC X(04).
           05  TLR-CASH-IN-TODAY           PIC S9(11)V99 COMP-3.
           05  TLR-CASH-OUT-TODAY          PIC S9(11)V99 COMP-3.
      *    MAXIMUM SINGLE WITHDRAWAL THIS USER MAY APPROVE AS OVERRIDE
           05  TLR-OVERRIDE-LIMIT          PIC S9(11)V99 COMP-3.
           05  TLR-LAST-SIGNON-DATE        PIC 9(08).
           05  FILLER                      PIC X(30).
