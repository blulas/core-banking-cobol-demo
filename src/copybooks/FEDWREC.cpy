      *================================================================*
      * COPYBOOK : FEDWREC                                             *
      * PURPOSE  : OUTBOUND FEDWIRE FUNDS TRANSFER MESSAGE - SIMPLIFIED*
      *            FAIM-STYLE TAGGED LINES. ONE LINE PER TAG; EACH     *
      *            MESSAGE STARTS WITH {1500}. ELEMENTS WITHIN A TAG   *
      *            ARE '*' DELIMITED. FILE IS PICKED UP BY THE FEDLINE *
      *            ADVANTAGE GATEWAY (JOB CBSPAYPR STEP FWXMIT).       *
      * FILE     : FEDWOUT (LINE SEQUENTIAL, 100 BYTES)                *
      *   {1500} SENDER SUPPLIED INFO  (FORMAT VERSION, TEST/PROD)     *
      *   {1510} TYPE/SUBTYPE          1000 = BASIC FUNDS TRANSFER     *
      *   {1520} IMAD                  CCYYMMDD + LTERM(8) + SEQ(6)    *
      *   {2000} AMOUNT                12 DIGITS, IMPLIED CENTS        *
      *   {3100} SENDER DI             ABA(9) + SHORT NAME(18)         *
      *   {3400} RECEIVER DI           ABA(9) + SHORT NAME(18)         *
      *   {3600} BUSINESS FUNCTION     CTR = CUSTOMER TRANSFER         *
      *   {4100} BENEFICIARY FI        INTL ONLY - B + BIC             *
      *   {4200} BENEFICIARY           ID CODE + ID * NAME * ADDRESS   *
      *   {5000} ORIGINATOR            ID CODE + ID * NAME * ADDRESS   *
      *   {6000} ORIG TO BENE INFO     4 LINES OF 35                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL - FEDLINE PC FORMAT                   *
      * 2004-11-15 BHS  CONVERTED TO FAIM TAGS                         *
      * 2009-04-20 DPS  {4100} BENEFICIARY FI FOR INTERNATIONAL        *
      *================================================================*
       01  FEDWIRE-LINE.
           05  FW-TAG                      PIC X(06).
               88  FW-TAG-SENDER-INFO                VALUE '{1500}'.
               88  FW-TAG-TYPE-SUBTYPE               VALUE '{1510}'.
               88  FW-TAG-IMAD                       VALUE '{1520}'.
               88  FW-TAG-AMOUNT                     VALUE '{2000}'.
               88  FW-TAG-SENDER-DI                  VALUE '{3100}'.
               88  FW-TAG-RECEIVER-DI                VALUE '{3400}'.
               88  FW-TAG-BUS-FUNCTION               VALUE '{3600}'.
               88  FW-TAG-BENE-FI                    VALUE '{4100}'.
               88  FW-TAG-BENEFICIARY                VALUE '{4200}'.
               88  FW-TAG-ORIGINATOR                 VALUE '{5000}'.
               88  FW-TAG-OBI                        VALUE '{6000}'.
           05  FW-DATA                     PIC X(94).
      *--- TAG BODIES (BUILT IN WORKING STORAGE, MOVED TO FW-DATA) ----*
       01  FW-1500-BODY.
           05  FW-1500-FORMAT-VERSION      PIC X(02) VALUE '30'.
           05  FW-1500-USER-REQ-CORR       PIC X(08).
           05  FW-1500-TEST-PROD           PIC X(01) VALUE 'P'.
           05  FW-1500-DUP-IND             PIC X(01) VALUE SPACE.
       01  FW-1510-BODY.
           05  FW-1510-TYPE                PIC X(02) VALUE '10'.
           05  FW-1510-SUBTYPE             PIC X(02) VALUE '00'.
       01  FW-1520-BODY.
           05  FW-1520-CYCLE-DATE          PIC 9(08).
           05  FW-1520-LTERM               PIC X(08).
           05  FW-1520-SEQ                 PIC 9(06).
       01  FW-2000-BODY.
           05  FW-2000-AMOUNT              PIC 9(10)V99.
       01  FW-DI-BODY.
           05  FW-DI-ROUTING               PIC X(09).
           05  FW-DI-SHORT-NAME            PIC X(18).
