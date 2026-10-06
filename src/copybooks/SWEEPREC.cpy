      *================================================================*
      * COPYBOOK : SWEEPREC                                            *
      * PURPOSE  : SWEEP / ZERO BALANCE ACCOUNT (ZBA) DEFINITION       *
      * FILE     : SWEEPMST (KSDS, KEY = SWP-ID)                       *
      *   TYPE Z - ZBA. SUB-ACCOUNT SWEPT TO ZERO AGAINST PARENT.      *
      *            POSITIVE BALANCE CONCENTRATED TO PARENT, NEGATIVE   *
      *            BALANCE FUNDED FROM PARENT.                         *
      *   TYPE T - TARGET BALANCE. EXCESS OVER TARGET SWEPT TO THE     *
      *            INVESTMENT ACCOUNT, DEFICIT FUNDED BACK.            *
      *   TYPE R - RANGE. SWEEP ONLY WHEN OUTSIDE MIN/MAX BAND, BACK   *
      *            TO THE MIDPOINT OF THE BAND.                        *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2001-10-01 BHS  ORIGINAL - TXB CASH CONCENTRATION              *
      * 2004-05-17 BHS  RANGE (BAND) SWEEPS                            *
      * 2011-04-12 DPS  PRIORITY, MIN TRANSFER, INCREMENT (CHG1544)    *
      * 2017-08-02 AKW  LAST SWEEP DIRECTION FOR CASH POSITION INQ     *
      *================================================================*
       01  SWEEP-RECORD.
           05  SWP-ID                      PIC X(08).
           05  SWP-TYPE                    PIC X(01).
               88  SWP-ZBA                           VALUE 'Z'.
               88  SWP-TARGET                        VALUE 'T'.
               88  SWP-RANGE                         VALUE 'R'.
           05  SWP-CUST-ID                 PIC X(10).
      *    SOURCE = OPERATING / SUB ACCOUNT. TARGET = PARENT (ZBA) OR
      *    INVESTMENT ACCOUNT (TARGET/RANGE).
           05  SWP-SOURCE-ACCT             PIC X(12).
           05  SWP-TARGET-ACCT             PIC X(12).
           05  SWP-TARGET-BAL              PIC S9(13)V99 COMP-3.
           05  SWP-MIN-BAL                 PIC S9(13)V99 COMP-3.
           05  SWP-MAX-BAL                 PIC S9(13)V99 COMP-3.
      *    TRANSFERS ARE MADE IN MULTIPLES OF THE INCREMENT
           05  SWP-INCREMENT               PIC S9(09)V99 COMP-3.
      *    ZERO = USE RULE DEFAULT (SEE SWEEPRUL)
           05  SWP-MIN-TRANSFER            PIC S9(09)V99 COMP-3.
           05  SWP-FREQUENCY               PIC X(01).
               88  SWP-FREQ-DAILY                    VALUE 'D'.
               88  SWP-FREQ-WEEKLY                   VALUE 'W'.
               88  SWP-FREQ-MONTHLY                  VALUE 'M'.
           05  SWP-STATUS                  PIC X(01).
               88  SWP-ACTIVE                        VALUE 'A'.
               88  SWP-SUSPENDED                     VALUE 'S'.
               88  SWP-CANCELLED                     VALUE 'C'.
           05  SWP-EFF-DATE                PIC 9(08).
      *    ZERO = NO EXPIRY
           05  SWP-EXP-DATE                PIC 9(08).
      *    01 PROCESSES FIRST (ZBA BEFORE TARGET SO PARENT IS FUNDED)
           05  SWP-PRIORITY                PIC 9(02).
           05  SWP-LAST-SWEEP-DATE         PIC 9(08).
           05  SWP-LAST-SWEEP-AMT          PIC S9(13)V99 COMP-3.
           05  SWP-LAST-SWEEP-DIR          PIC X(01).
               88  SWP-LAST-OUT                      VALUE 'O'.
               88  SWP-LAST-IN                       VALUE 'I'.
               88  SWP-LAST-NONE                     VALUE 'N'.
           05  SWP-SWEEP-COUNT-YTD         PIC 9(05).
           05  SWP-LAST-MAINT-DATE         PIC 9(08).
           05  SWP-LAST-MAINT-USER         PIC X(08).
           05  FILLER                      PIC X(30).
