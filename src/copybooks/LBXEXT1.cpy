      *================================================================*
      * COPYBOOK : LBXEXT1                                             *
      * PURPOSE  : LOCKBOX REMITTANCE HISTORY RECORD LAYOUT            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-07-14 PQR  ORIGINAL                                       *
      * 1995-12-08 EWS  CASH-ON-HAND LIMIT REVISED REQ2614             *
      * 1996-02-20 SKN  FLOAT CUTOFF MOVED PRB7808                     *
      * 1999-01-16 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-02-03 DPS  CASH-ON-HAND RERUN PROTECTION INC7777          *
      * 2003-05-27 GAF  CASH-ON-HAND CALCULATION CORRECTED PRB5847     *
      * 2005-08-03 DPS  REMIT STATUS CODES ADDED REQ1548               *
      * 2008-05-15 SKN  EXCEPT RERUN PROTECTION PRB7565                *
      * 2008-10-11 DPS  PERFORMANCE - CASH-ON-HAND READ AHEAD REQ4484  *
      * 2011-02-12 JLM  EXCEPT FEE SCHEDULE UPDATE REQ5620             *
      * 2012-07-22 TWH  NEW REMIT REPORT COLUMNS INC6447               *
      *================================================================*
       01  LBX-H1-REC.
           05  LBXH1-ADDR                  PIC X(30).
           05  LBXH1-REF-NO                PIC X(16).
           05  LBXH1-KEY                   PIC X(12).
           05  LBXH1-NAME                  PIC X(30).
           05  LBXH1-DESC                  PIC X(30).
           05  LBXH1-CUST-ID               PIC X(10).
           05  LBXH1-CONTACT               PIC X(20).
           05  LBX-H1-AMOUNTS.
               10  LBXH1-EXCEPT-AMT        PIC S9(13)V99 COMP-3.
               10  LBXH1-DEPOSIT-YTD       PIC S9(13)V99 COMP-3.
               10  LBXH1-FLOAT-YTD         PIC S9(13)V99 COMP-3.
               10  LBXH1-CASH-ON-HAND-YTD  PIC S9(13)V99 COMP-3.
               10  LBXH1-FEE-YTD           PIC S9(13)V99 COMP-3.
               10  LBXH1-REMIT-YTD         PIC S9(13)V99 COMP-3.
               10  LBXH1-FEE-AMT           PIC S9(13)V99 COMP-3.
               10  LBXH1-EXCEPT-YTD        PIC S9(13)V99 COMP-3.
               10  LBXH1-CHECK-YTD         PIC S9(13)V99 COMP-3.
               10  LBXH1-REMIT-AMT         PIC S9(13)V99 COMP-3.
               10  LBXH1-FLOAT-AMT         PIC S9(13)V99 COMP-3.
               10  LBXH1-DEPOSIT-AMT       PIC S9(13)V99 COMP-3.
           05  LBX-H1-DATES.
               10  LBXH1-NEXT-DUE-DATE     PIC 9(08).
               10  LBXH1-NOTICE-DATE       PIC 9(08).
               10  LBXH1-MATURITY-DATE     PIC 9(08).
               10  LBXH1-CLOSE-DATE        PIC 9(08).
               10  LBXH1-EFFECTIVE-DATE    PIC 9(08).
               10  LBXH1-LAST-PAY-DATE     PIC 9(08).
               10  LBXH1-LAST-STMT-DATE    PIC 9(08).
               10  LBXH1-OPEN-DATE         PIC 9(08).
           05  LBX-H1-CODES.
               10  LBXH1-CYCLE-CD          PIC X(03).
                   88  LBXH1-CYCLE-CD-EDV          VALUE 'EDV'.
                   88  LBXH1-CYCLE-CD-EPM          VALUE 'EPM'.
                   88  LBXH1-CYCLE-CD-FZY          VALUE 'FZY'.
                   88  LBXH1-CYCLE-CD-XUU          VALUE 'XUU'.
                   88  LBXH1-CYCLE-CD-YBH          VALUE 'YBH'.
               10  LBXH1-REGION-CD         PIC X(03).
                   88  LBXH1-REGION-CD-DVP         VALUE 'DVP'.
                   88  LBXH1-REGION-CD-FMZ         VALUE 'FMZ'.
                   88  LBXH1-REGION-CD-KSL         VALUE 'KSL'.
                   88  LBXH1-REGION-CD-NYK         VALUE 'NYK'.
               10  LBXH1-HOLD-FLAG         PIC X(01).
                   88  LBXH1-HOLD-FLAG-YES         VALUE 'Y'.
               10  LBXH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  LBXH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  LBXH1-BATCH-TYPE-CD     PIC X(01).
               10  LBXH1-CHANNEL-CD        PIC X(01).
                   88  LBXH1-CHANNEL-CD-B          VALUE 'B'.
                   88  LBXH1-CHANNEL-CD-H          VALUE 'H'.
                   88  LBXH1-CHANNEL-CD-Z          VALUE 'Z'.
               10  LBXH1-STOP-FLAG         PIC X(01).
                   88  LBXH1-STOP-FLAG-YES         VALUE 'Y'.
               10  LBXH1-LOCKBOX-CD        PIC X(02).
                   88  LBXH1-LOCKBOX-CD-DX         VALUE 'DX'.
                   88  LBXH1-LOCKBOX-CD-JM         VALUE 'JM'.
                   88  LBXH1-LOCKBOX-CD-WW         VALUE 'WW'.
                   88  LBXH1-LOCKBOX-CD-XE         VALUE 'XE'.
               10  LBXH1-PREFERRED-FLAG    PIC X(01).
                   88  LBXH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  LBXH1-EXEMPT-FLAG       PIC X(01).
                   88  LBXH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  LBXH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  LBXH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
           05  LBX-H1-COUNTERS.
               10  LBXH1-DISCOUNT          PIC 9(03)V9(06).
               10  LBXH1-ITEM-CNT          PIC 9(07).
               10  LBXH1-EXCEPT-CNT        PIC 9(07).
               10  LBXH1-RETRY-CNT         PIC 9(07).
               10  LBXH1-FEE-PCT           PIC 9(03)V9(06).
               10  LBXH1-PAYMENT-CNT       PIC 9(07).
               10  LBXH1-PENALTY-PCT       PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
