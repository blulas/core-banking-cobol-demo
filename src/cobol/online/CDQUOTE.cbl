      *================================================================*
      * PROGRAM  : CDQUOTE                                             *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CB21       MAPSET CDQUOS   MAP CDQUOM               *
      * PURPOSE  : CERTIFICATE OF DEPOSIT EARLY WITHDRAWAL QUOTE.      *
      *            SHOWS THE CD TERMS, THE PENALTY (DAYS, AMOUNT AND   *
      *            THE SPLIT BETWEEN INTEREST AND PRINCIPAL) AND THE   *
      *            NET PROCEEDS FOR A FULL OR PARTIAL WITHDRAWAL ON A  *
      *            GIVEN DATE. PENALTY PRICED BY CDPENLTY.             *
      *            QUOTE ONLY - NOTHING IS POSTED. THE WITHDRAWAL IS   *
      *            PROCESSED BY BRANCH OPERATIONS (CDCLOSE).           *
      *            ENTER - QUOTE   PF3 - MENU   PF12 - CANCEL          *
      * FILES    : ACCTMST, CUSTMST, PRODMST, BANKCTL (ALL READ ONLY)  *
      * LINKS    : CDPENLTY                                            *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1997-06-02 RJT  ORIGINAL                                       *
      * 1998-10-19 MKL  Y2K - WITHDRAWAL DATE CCYYMMDD                 *
      * 2004-11-15 BHS  GRACE PERIOD AFTER RENEWAL - DISPLAY AND NO    *
      *                 PENALTY QUOTE (CHG0301)                        *
      * 2009-04-20 DPS  IRA AGE WAIVER REMOVED - CHG0987               *
      * 2013-08-05 DPS  PRINCIPAL INVASION LINE (CD36) - CHG0981       *
      * 2016-02-08 DPS  RESP CHECKING - REMOVED HANDLE CONDITION       *
      * 2017-02-08 AKW  NO-PENALTY CD (CDNP) NOTE                      *
      * 2022-06-13 JQP  DECEASED OWNER - JOINT OWNER CHECKED TOO       *
      *                 (INC40877)                                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDQUOTE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM                      PIC X(08) VALUE 'CDQUOTE'.
       01  WS-TRANID                       PIC X(04) VALUE 'CB21'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-RESP2                        PIC S9(08) COMP.
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.

      *    POST-RENEWAL GRACE PERIOD (CALENDAR DAYS) - DEPOSIT
      *    AGREEMENT SECTION 7.4
       01  WS-GRACE-DAYS                   PIC S9(05) VALUE 7.

       01  WS-WORK.
           05  WS-ERROR-FLAG               PIC X(01).
               88  WS-ERROR                          VALUE 'Y'.
           05  WS-MESSAGE                  PIC X(79).
           05  WS-WD-AMT                   PIC S9(13)V99 COMP-3.
           05  WS-WD-DATE                  PIC 9(08).
           05  WS-TERM-START               PIC 9(08).
           05  WS-FUND-DATE                PIC 9(08).
           05  WS-GRACE-END                PIC 9(08).
           05  WS-DECEASED                 PIC X(01).
           05  WS-RENEWED-FLAG             PIC X(01).
               88  WS-RENEWED                        VALUE 'Y'.
           05  WS-AMT-ED                   PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05  WS-RATE-ED                  PIC ZZ9.999999.
           05  WS-DAYS-ED                  PIC ZZ9.
           05  WS-DATE-ED                  PIC 9999/99/99.

       COPY ACCTREC.
       COPY CUSTREC.
       COPY PRODREC.
       COPY BANKCTL.
       COPY CDPPARM.
       COPY DATEPARM.
       COPY CDQUOS.
       COPY DFHAID.
       COPY DFHBMSCA.

       01  WS-COMMAREA.
           05  CA-STATE                    PIC X(01).
               88  CA-FIRST-TIME                     VALUE SPACE.
               88  CA-QUOTED                         VALUE 'Q'.
           05  CA-ACCT                     PIC X(12).

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(13).

       PROCEDURE DIVISION.
       0000-MAIN.
           IF EIBCALEN = 0
               INITIALIZE WS-COMMAREA
               PERFORM 1000-SEND-INITIAL
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               EVALUATE EIBAID
                   WHEN DFHPF3
                       EXEC CICS XCTL PROGRAM('CBMENU') END-EXEC
                   WHEN DFHPF12
                       INITIALIZE WS-COMMAREA
                       PERFORM 1000-SEND-INITIAL
                   WHEN DFHENTER
                       PERFORM 2000-QUOTE
                   WHEN OTHER
                       MOVE 'INVALID KEY PRESSED' TO WS-MESSAGE
                       PERFORM 8000-SEND-DATAONLY
               END-EVALUATE
           END-IF
           EXEC CICS RETURN TRANSID(WS-TRANID)
                COMMAREA(WS-COMMAREA) LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

       1000-SEND-INITIAL.
           MOVE LOW-VALUES TO CDQUOMO
           PERFORM 8100-READ-CONTROL
           MOVE CTL-BUSINESS-DATE TO WS-DATE-ED
           MOVE WS-DATE-ED TO CURDATEO
           MOVE 'ENTER CD ACCOUNT, AMOUNT (BLANK = FULL) AND DATE'
             TO MSGO
           EXEC CICS SEND MAP('CDQUOM') MAPSET('CDQUOS')
                FROM(CDQUOMO) ERASE RESP(WS-RESP) END-EXEC.

      *----------------------------------------------------------------*
      * ENTER - VALIDATE, READ THE CD AND PRICE THE PENALTY            *
      *----------------------------------------------------------------*
       2000-QUOTE.
           EXEC CICS RECEIVE MAP('CDQUOM') MAPSET('CDQUOS')
                INTO(CDQUOMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           PERFORM 8100-READ-CONTROL
           IF NOT WS-ERROR
               PERFORM 2100-EDIT-INPUT
           END-IF
           IF NOT WS-ERROR
               PERFORM 2200-READ-CD
           END-IF
           IF NOT WS-ERROR
               PERFORM 2300-TERM-DATES
           END-IF
           IF NOT WS-ERROR
               PERFORM 2400-CHECK-OWNERS
               PERFORM 2500-SHOW-TERMS
               PERFORM 2600-PRICE-PENALTY
           END-IF
           PERFORM 8000-SEND-DATAONLY.

       2100-EDIT-INPUT.
           IF ACCTNOL = 0 OR ACCTNOI = SPACES OR LOW-VALUES
               MOVE 'CD ACCOUNT NUMBER IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO ACCTNOL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           MOVE ACCTNOI TO CA-ACCT

           MOVE ZERO TO WS-WD-AMT
           IF WDAMTL > 0 AND WDAMTI NOT = SPACES
               IF FUNCTION TEST-NUMVAL(WDAMTI) NOT = 0
                   MOVE 'WITHDRAWAL AMOUNT MUST BE NUMERIC'
                     TO WS-MESSAGE
                   MOVE -1 TO WDAMTL
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
               COMPUTE WS-WD-AMT = FUNCTION NUMVAL(WDAMTI)
               IF WS-WD-AMT < ZERO
                   MOVE 'WITHDRAWAL AMOUNT CANNOT BE NEGATIVE'
                     TO WS-MESSAGE
                   MOVE -1 TO WDAMTL
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
           END-IF

           MOVE CTL-BUSINESS-DATE TO WS-WD-DATE
           IF WDDATEL > 0 AND WDDATEI NOT = SPACES
               IF WDDATEI IS NOT NUMERIC
                   MOVE 'WITHDRAWAL DATE MUST BE CCYYMMDD' TO WS-MESSAGE
                   MOVE -1 TO WDDATEL
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
               MOVE WDDATEI TO WS-WD-DATE
               INITIALIZE DATE-PARMS
               SET DTU-VALIDATE TO TRUE
               MOVE WS-WD-DATE TO DTU-DATE-1
               CALL 'DATEUTIL' USING DATE-PARMS
               IF NOT DTU-RESULT-YES
                   MOVE 'WITHDRAWAL DATE IS NOT A VALID DATE'
                     TO WS-MESSAGE
                   MOVE -1 TO WDDATEL
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
      *        NO BACK-DATED QUOTES - INC11872
               IF WS-WD-DATE < CTL-BUSINESS-DATE
                   MOVE 'WITHDRAWAL DATE CANNOT BE IN THE PAST'
                     TO WS-MESSAGE
                   MOVE -1 TO WDDATEL
                   MOVE 'Y' TO WS-ERROR-FLAG
               END-IF
           END-IF.

       2200-READ-CD.
           MOVE CA-ACCT TO ACCT-NUMBER
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                RIDFLD(ACCT-NUMBER) RESP(WS-RESP) END-EXEC
           IF WS-RESP = DFHRESP(NOTFND)
               MOVE 'ACCOUNT NOT FOUND' TO WS-MESSAGE
               MOVE -1 TO ACCTNOL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ACCOUNT FILE UNAVAILABLE - TRY AGAIN'
                 TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           MOVE ACCT-PRODUCT-CODE TO PROD-CODE
           EXEC CICS READ FILE('PRODMST') INTO(PRODUCT-RECORD)
                RIDFLD(PROD-CODE) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'PRODUCT NOT ON PRODMST - CALL DEPOSIT OPS'
                 TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           IF NOT PROD-TIME-DEPOSIT
               MOVE 'ACCOUNT IS NOT A CERTIFICATE OF DEPOSIT'
                 TO WS-MESSAGE
               MOVE -1 TO ACCTNOL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN ACCT-CLOSED OR ACCT-ESCHEATED OR ACCT-CHARGED-OFF
                   MOVE 'CD IS CLOSED - NO QUOTE AVAILABLE'
                     TO WS-MESSAGE
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN ACCT-LEDGER-BAL NOT > ZERO
                   MOVE 'CD HAS NO PRINCIPAL' TO WS-MESSAGE
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN WS-WD-AMT > ACCT-LEDGER-BAL
                   MOVE 'AMOUNT EXCEEDS CD PRINCIPAL' TO WS-MESSAGE
                   MOVE -1 TO WDAMTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * START OF THE CURRENT TERM = MATURITY - TERM MONTHS. A CD THAT  *
      * HAS RENEWED (TERM START AFTER OPEN DATE) IS PENALTY FREE FOR   *
      * THE GRACE PERIOD AFTER THE RENEWAL DATE.                       *
      *----------------------------------------------------------------*
       2300-TERM-DATES.
           MOVE ACCT-OPEN-DATE TO WS-TERM-START
           MOVE 'N' TO WS-RENEWED-FLAG
           MOVE ZERO TO WS-GRACE-END
           IF ACCT-MATURITY-DATE > ZERO AND ACCT-TERM-MONTHS > ZERO
               INITIALIZE DATE-PARMS
               SET DTU-ADD-MONTHS TO TRUE
               MOVE ACCT-MATURITY-DATE TO DTU-DATE-1
               COMPUTE DTU-NUMBER = ZERO - ACCT-TERM-MONTHS
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK AND DTU-RESULT-DATE > ACCT-OPEN-DATE
                   MOVE DTU-RESULT-DATE TO WS-TERM-START
                   SET WS-RENEWED TO TRUE
               END-IF
           END-IF
           MOVE WS-TERM-START TO WS-FUND-DATE
           IF WS-RENEWED
               INITIALIZE DATE-PARMS
               SET DTU-ADD-DAYS TO TRUE
               MOVE WS-TERM-START TO DTU-DATE-1
               MOVE WS-GRACE-DAYS TO DTU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO WS-GRACE-END
           END-IF.

      *    DEATH OF ANY OWNER WAIVES THE PENALTY
       2400-CHECK-OWNERS.
           MOVE 'N' TO WS-DECEASED
           MOVE SPACES TO OWNERO
           MOVE ACCT-PRIMARY-CUST-ID TO CUST-ID
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                RIDFLD(CUST-ID) RESP(WS-RESP) END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               STRING FUNCTION TRIM(CUST-FIRST-NAME) ' '
                      FUNCTION TRIM(CUST-LAST-NAME)
                      DELIMITED BY SIZE INTO OWNERO
               IF CUST-DECEASED
                   MOVE 'Y' TO WS-DECEASED
               END-IF
           END-IF
           IF ACCT-JOINT-CUST-ID NOT = SPACES
               MOVE ACCT-JOINT-CUST-ID TO CUST-ID
               EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                    RIDFLD(CUST-ID) RESP(WS-RESP) END-EXEC
               IF WS-RESP = DFHRESP(NORMAL) AND CUST-DECEASED
                   MOVE 'Y' TO WS-DECEASED
               END-IF
           END-IF.

       2500-SHOW-TERMS.
           MOVE ACCT-PRODUCT-CODE TO PRODCDO
           MOVE PROD-NAME         TO PRODNMO
           MOVE ACCT-LEDGER-BAL   TO WS-AMT-ED
           MOVE WS-AMT-ED         TO PRINCO
           MOVE ACCT-INT-RATE     TO WS-RATE-ED
           MOVE WS-RATE-ED        TO RATEO
           MOVE WS-FUND-DATE      TO WS-DATE-ED
           MOVE WS-DATE-ED        TO FUNDDTO
           MOVE ACCT-MATURITY-DATE TO WS-DATE-ED
           MOVE WS-DATE-ED        TO MATDTO
           IF WS-GRACE-END > ZERO
               MOVE WS-GRACE-END  TO WS-DATE-ED
               MOVE WS-DATE-ED    TO GRACDTO
           ELSE
               MOVE 'N/A'         TO GRACDTO
           END-IF
           MOVE ACCT-TERM-MONTHS  TO WS-DAYS-ED
           MOVE WS-DAYS-ED        TO TERMO
           COMPUTE WS-AMT-ED ROUNDED = ACCT-ACCRUED-INT
           MOVE WS-AMT-ED         TO ACCINTO.

      *----------------------------------------------------------------*
      * PRICE THE PENALTY - SAME MODULE AS BRANCH CD CLOSE             *
      *----------------------------------------------------------------*
       2600-PRICE-PENALTY.
           INITIALIZE CDPENLTY-PARMS
           MOVE ACCT-PRODUCT-CODE          TO CDP-PRODUCT-CODE
           MOVE PROD-EARLY-WD-PENALTY-DAYS TO CDP-PENALTY-DAYS
           MOVE ACCT-LEDGER-BAL            TO CDP-PRINCIPAL
           MOVE WS-WD-AMT                  TO CDP-WITHDRAWAL-AMT
           MOVE ACCT-INT-RATE              TO CDP-INT-RATE
           MOVE WS-FUND-DATE               TO CDP-FUNDING-DATE
           MOVE ACCT-MATURITY-DATE         TO CDP-MATURITY-DATE
           MOVE WS-GRACE-END               TO CDP-GRACE-END-DATE
           MOVE WS-WD-DATE                 TO CDP-WITHDRAWAL-DATE
           MOVE ACCT-ACCRUED-INT           TO CDP-ACCRUED-INT
           MOVE WS-DECEASED                TO CDP-OWNER-DECEASED
           EXEC CICS LINK PROGRAM('CDPENLTY')
                COMMAREA(CDPENLTY-PARMS)
                LENGTH(LENGTH OF CDPENLTY-PARMS) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'PENALTY MODULE UNAVAILABLE - TRY AGAIN'
                 TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF

           EVALUATE CDP-RETURN-CODE
               WHEN 00
               WHEN 04
                   PERFORM 2610-SHOW-PENALTY
               WHEN 08
                   MOVE 'AMOUNT EXCEEDS CD PRINCIPAL' TO WS-MESSAGE
                   MOVE -1 TO WDAMTL
               WHEN OTHER
                   MOVE 'CD DATES INVALID FOR QUOTE - CALL DEPOSIT OPS'
                     TO WS-MESSAGE
           END-EVALUATE
           SET CA-QUOTED TO TRUE.

       2610-SHOW-PENALTY.
           MOVE CDP-DAYS-APPLIED      TO WS-DAYS-ED
           MOVE WS-DAYS-ED            TO PENDAYSO
           MOVE CDP-WAIVER-CODE       TO WAIVERO
           MOVE CDP-PENALTY-AMT       TO WS-AMT-ED
           MOVE WS-AMT-ED             TO PENAMTO
           MOVE CDP-PENALTY-FROM-INT  TO WS-AMT-ED
           MOVE WS-AMT-ED             TO PENINTO
           MOVE CDP-PENALTY-FROM-PRIN TO WS-AMT-ED
           MOVE WS-AMT-ED             TO PENPRNO
           MOVE CDP-NET-PROCEEDS      TO WS-AMT-ED
           MOVE WS-AMT-ED             TO NETAMTO
           EVALUATE CDP-WAIVER-CODE
               WHEN 'MT'
                   MOVE 'CD HAS MATURED - NO EARLY WITHDRAWAL PENALTY'
                     TO NOTEO
               WHEN 'GP'
                   MOVE 'WITHIN RENEWAL GRACE PERIOD - NO PENALTY'
                     TO NOTEO
               WHEN 'DC'
                   MOVE 'WAIVED - DEATH OF OWNER (DEATH CERT REQD)'
                     TO NOTEO
               WHEN 'NP'
                   MOVE 'NO-PENALTY CD - FREE WITHDRAWAL AFTER 6 DAYS'
                     TO NOTEO
               WHEN OTHER
                   IF CDP-PENALTY-FROM-PRIN > ZERO
                       MOVE 'PENALTY EXCEEDS INTEREST - PRIN REDUCED'
                         TO NOTEO
                   ELSE
                       MOVE 'PENALTY TAKEN FROM EARNED INTEREST'
                         TO NOTEO
                   END-IF
           END-EVALUATE
           MOVE 'QUOTE ONLY - VALID FOR THE DATE SHOWN'
             TO WS-MESSAGE.

      *----------------------------------------------------------------*
      * COMMON ROUTINES                                                *
      *----------------------------------------------------------------*
       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           MOVE CTL-BUSINESS-DATE TO WS-DATE-ED
           MOVE WS-DATE-ED TO CURDATEO
           EXEC CICS SEND MAP('CDQUOM') MAPSET('CDQUOS')
                FROM(CDQUOMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.

       8100-READ-CONTROL.
           EXEC CICS READ FILE('BANKCTL') INTO(BANK-CONTROL-RECORD)
                RIDFLD(WS-CTL-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'BANK CONTROL RECORD UNAVAILABLE' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.
