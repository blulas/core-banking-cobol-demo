      *================================================================*
      * COPYBOOK : OFACSUP                                             *
      * PURPOSE  : OFAC FALSE-POSITIVE SUPPRESSION RECORD. WRITTEN BY  *
      *            CICS TRANSACTION CB61 (OFACREV) WHEN AN ANALYST     *
      *            CLEARS A POTENTIAL MATCH, SO THE SAME CUSTOMER /    *
      *            SDN ENTRY PAIR IS NOT RE-ALERTED UNTIL THE          *
      *            SUPPRESSION EXPIRES (365 DAYS - OFAC POLICY 4.3).   *
      * FILE     : OFACSUP (VSAM KSDS, KEY = OSP-KEY, LRECL 150)       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2014-09-15 DPS  ORIGINAL - CHG1260 FALSE POSITIVE FATIGUE      *
      *================================================================*
       01  OFAC-SUPPRESS-RECORD.
           05  OSP-KEY.
               10  OSP-CUST-ID             PIC X(10).
               10  OSP-SDN-ENT-NUM         PIC 9(06).
           05  OSP-SDN-NAME                PIC X(60).
           05  OSP-MATCH-SCORE             PIC 9(03).
           05  OSP-CLEARED-DATE            PIC 9(08).
           05  OSP-CLEARED-USER            PIC X(08).
           05  OSP-SUPPRESS-UNTIL          PIC 9(08).
           05  OSP-REASON                  PIC X(40).
           05  FILLER                      PIC X(07).
