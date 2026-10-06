      *================================================================*
      * COPYBOOK : HOLDREC                                             *
      * PURPOSE  : ACCOUNT HOLD / LIEN RECORD                          *
      * FILE     : HOLDMST  (KSDS, KEY = HOLD-KEY)                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-04-19 RJT  ORIGINAL                                       *
      * 2011-07-21 DPS  REG CC NEXT-DAY / EXCEPTION HOLD TYPES         *
      *================================================================*
       01  HOLD-RECORD.
           05  HOLD-KEY.
               10  HOLD-ACCT-NUMBER        PIC X(12).
               10  HOLD-SEQ                PIC 9(04).
           05  HOLD-TYPE                   PIC X(02).
               88  HOLD-CHECK-DEPOSIT                VALUE 'CK'.
               88  HOLD-CHECK-EXCEPTION              VALUE 'CX'.
               88  HOLD-LEGAL                        VALUE 'LG'.
               88  HOLD-ADMINISTRATIVE               VALUE 'AD'.
               88  HOLD-LIEN                         VALUE 'LN'.
               88  HOLD-FRAUD                        VALUE 'FR'.
               88  HOLD-CARD-AUTH                    VALUE 'AU'.
           05  HOLD-AMOUNT                 PIC S9(13)V99 COMP-3.
           05  HOLD-PLACED-DATE            PIC 9(08).
      *    99999999 = INDEFINITE (UNTIL MANUALLY RELEASED)
           05  HOLD-RELEASE-DATE           PIC 9(08).
           05  HOLD-STATUS                 PIC X(01).
               88  HOLD-ACTIVE                       VALUE 'A'.
               88  HOLD-RELEASED                     VALUE 'R'.
               88  HOLD-EXPIRED                      VALUE 'E'.
           05  HOLD-REASON                 PIC X(30).
           05  HOLD-TRAN-REF               PIC X(20).
           05  HOLD-PLACED-BY              PIC X(08).
           05  FILLER                      PIC X(20).
