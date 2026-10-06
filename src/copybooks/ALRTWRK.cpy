      *================================================================*
      * COPYBOOK : ALRTWRK                                             *
      * PURPOSE  : ALERT WORK QUEUE RECORD - ONLINE COPY OF ALERTS     *
      *            WITH THE ANALYST DISPOSITION AREA.                  *
      * FILE     : ALERTKS (VSAM KSDS, KEY = AWQ-KEY, LRECL 300)       *
      *            LOADED BY JOB CBSCOMPL STEPS CMP050 / CMP060 (SORT  *
      *            OUTREC: DATE + SEQNUM + ALERTREC + 120 BLANKS).     *
      *            UPDATED BY CICS TRANSACTION CB60 (ALRTWORK).        *
      *   DISPOSITION  SPACE OPEN, C CLOSED - NO ACTION,               *
      *                E ESCALATED TO BSA OFFICER, S SAR FILED         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2013-05-13 DPS  ORIGINAL                                       *
      * 2017-02-20 AKW  CHG1544 SAR DUE DATE, SUSPECT-KNOWN FLAG       *
      *================================================================*
       01  ALERT-WORK-RECORD.
           05  AWQ-KEY.
               10  AWQ-KEY-DATE            PIC 9(08).
               10  AWQ-KEY-SEQ             PIC 9(06).
      *--- COPY OF ALERTREC (166 BYTES) ------------------------------*
           05  AWQ-ALERT.
               10  AWQ-ALRT-DATE           PIC 9(08).
               10  AWQ-ALRT-TIME           PIC 9(06).
               10  AWQ-ALRT-TYPE           PIC X(04).
               10  AWQ-ALRT-SEVERITY       PIC X(01).
               10  AWQ-ALRT-CUST-ID        PIC X(10).
               10  AWQ-ALRT-ACCT-NUMBER    PIC X(12).
               10  AWQ-ALRT-AMOUNT         PIC S9(13)V99 COMP-3.
               10  AWQ-ALRT-RULE-ID        PIC X(08).
               10  AWQ-ALRT-DESCRIPTION    PIC X(80).
               10  AWQ-ALRT-STATUS         PIC X(01).
               10  AWQ-ALRT-SOURCE-PGM     PIC X(08).
               10  FILLER                  PIC X(20).
      *--- DISPOSITION AREA (120 BYTES) ------------------------------*
           05  AWQ-DISPOSITION.
               10  AWQ-DISP-CODE           PIC X(01).
                   88  AWQ-DISP-OPEN                 VALUE SPACE.
                   88  AWQ-DISP-CLOSED               VALUE 'C'.
                   88  AWQ-DISP-ESCALATED            VALUE 'E'.
                   88  AWQ-DISP-SAR-FILED            VALUE 'S'.
               10  AWQ-DISP-USER           PIC X(08).
               10  AWQ-DISP-DATE           PIC 9(08).
               10  AWQ-SUSPECT-KNOWN       PIC X(01).
                   88  AWQ-SUSPECT-IDENTIFIED        VALUE 'Y'.
                   88  AWQ-NO-SUSPECT                VALUE 'N'.
               10  AWQ-SAR-DUE-DATE        PIC 9(08).
               10  AWQ-SAR-BSA-ID          PIC X(14).
               10  AWQ-ESCALATED-TO        PIC X(08).
               10  AWQ-NOTES               PIC X(60).
               10  FILLER                  PIC X(12).
