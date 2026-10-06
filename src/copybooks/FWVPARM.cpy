      *================================================================*
      * COPYBOOK : FWVPARM                                             *
      * PURPOSE  : LINKAGE FOR FEEWAIVE - MONTHLY MAINTENANCE FEE      *
      *            WAIVER ELIGIBILITY                                  *
      *   OUTPUT FWV-WAIVER-CODE IS STORED IN ACCT-FEE-WAIVER-CODE:    *
      *     NW FIRST STATEMENT CYCLE     PW PRIVATE WEALTH SEGMENT     *
      *     MB MINIMUM DAILY BALANCE     AB AVERAGE DAILY BALANCE      *
      *     DD DIRECT DEPOSIT            EM EMPLOYEE                   *
      *     SR SENIOR (AGE)              ST STUDENT (AGE)              *
      *     RB RELATIONSHIP BALANCE      LK LINKED CHECKING            *
      *     NF PRODUCT HAS NO FEE        SPACES = NOT WAIVED           *
      *   RETURN CODES: 00 WAIVED  08 NOT WAIVED (FEE APPLIES)         *
      *                 12 BAD INPUT                                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-09-01 RJT  ORIGINAL                                       *
      * 2006-02-13 BHS  DIRECT DEPOSIT WAIVER                          *
      * 2012-05-07 DPS  SENIOR / STUDENT WAIVERS - CHG0733             *
      * 2019-08-19 AKW  PRIVATE WEALTH AND RELATIONSHIP WAIVERS        *
      *================================================================*
       01  FEEWAIVE-PARMS.
           05  FWV-PRODUCT-CODE            PIC X(04).
           05  FWV-BUSINESS-DATE           PIC 9(08).
           05  FWV-OPEN-DATE               PIC 9(08).
      *    LAST STATEMENT DATE - START OF THE CYCLE BEING CHARGED
           05  FWV-CYCLE-START-DATE        PIC 9(08).
           05  FWV-MIN-BAL-CYC             PIC S9(13)V99 COMP-3.
           05  FWV-AVG-BAL-CYC             PIC S9(13)V99 COMP-3.
           05  FWV-CURRENT-BAL             PIC S9(13)V99 COMP-3.
           05  FWV-DIRECT-DEP-AMT          PIC S9(13)V99 COMP-3.
           05  FWV-REL-BALANCE             PIC S9(13)V99 COMP-3.
           05  FWV-CUST-SEGMENT            PIC X(02).
           05  FWV-CUST-BIRTH-DATE         PIC 9(08).
           05  FWV-EMPLOYEE-FLAG           PIC X(01).
           05  FWV-HAS-DDAP-FLAG           PIC X(01).
      *--- OUTPUT ----------------------------------------------------*
           05  FWV-CUST-AGE                PIC 9(03).
           05  FWV-WAIVE-FLAG              PIC X(01).
               88  FWV-FEE-WAIVED                    VALUE 'Y'.
           05  FWV-WAIVER-CODE             PIC X(02).
           05  FWV-RETURN-CODE             PIC 9(02).
