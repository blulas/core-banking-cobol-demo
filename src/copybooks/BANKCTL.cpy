      *================================================================*
      * COPYBOOK : BANKCTL                                             *
      * PURPOSE  : BANK CONTROL RECORD - BUSINESS DATE, CYCLE STATUS,  *
      *            CUTOFF TIMES AND REFERENCE RATES.                   *
      * FILE     : BANKCTL (LINE SEQUENTIAL, ONE RECORD, DISPLAY ONLY) *
      * LENGTH   : 121                                                 *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-02-14 RJT  ORIGINAL                                       *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2010-06-21 DPS  ADDED SAME-DAY ACH CUTOFF (FUTURE USE)         *
      * 2016-09-23 DPS  ACTIVATED SDACH CUTOFF                         *
      * 2022-01-10 AKW  ADDED SOFR RATE (LIBOR CESSATION)              *
      *================================================================*
       01  BANK-CONTROL-RECORD.
           05  CTL-RECORD-ID               PIC X(08).
           05  CTL-BANK-ID                 PIC X(04).
           05  CTL-BUSINESS-DATE           PIC 9(08).
           05  CTL-PREV-BUS-DATE           PIC 9(08).
           05  CTL-NEXT-BUS-DATE           PIC 9(08).
           05  CTL-CYCLE-STATUS            PIC X(01).
               88  CTL-ONLINE-OPEN                   VALUE 'O'.
               88  CTL-EOD-RUNNING                   VALUE 'E'.
               88  CTL-EOD-COMPLETE                  VALUE 'C'.
           05  CTL-MONTH-END-FLAG          PIC X(01).
               88  CTL-IS-MONTH-END                  VALUE 'Y'.
           05  CTL-QUARTER-END-FLAG        PIC X(01).
               88  CTL-IS-QUARTER-END                VALUE 'Y'.
           05  CTL-YEAR-END-FLAG           PIC X(01).
               88  CTL-IS-YEAR-END                   VALUE 'Y'.
      *    CUTOFF TIMES ARE HHMM EASTERN
           05  CTL-ACH-CUTOFF-TIME         PIC 9(04).
           05  CTL-SDACH-CUTOFF-TIME       PIC 9(04).
           05  CTL-WIRE-CUTOFF-TIME        PIC 9(04).
      *    RATES ARE PERCENT, E.G. 004.330000 = 4.33%
           05  CTL-FED-FUNDS-RATE          PIC 9(03)V9(06).
           05  CTL-PRIME-RATE              PIC 9(03)V9(06).
           05  CTL-SOFR-RATE               PIC 9(03)V9(06).
           05  CTL-ECR-BASE-RATE           PIC 9(03)V9(06).
           05  CTL-LAST-TRAN-SEQ           PIC 9(12).
           05  CTL-RUN-MODE                PIC X(01).
               88  CTL-PRODUCTION                    VALUE 'P'.
               88  CTL-TEST                          VALUE 'T'.
           05  FILLER                      PIC X(20).
