      *================================================================*
      * PROGRAM  : RATEINQ                                             *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CB20       MAPSET RATINQS  MAP RATINQM              *
      * PURPOSE  : DEPOSIT RATE SHEET (TRUTH IN SAVINGS DISCLOSURE     *
      *            SUPPORT FOR BRANCH AND CALL CENTRE STAFF).          *
      *            - LISTS EVERY TIER OF THE PRODUCT'S RATE TABLE      *
      *              (RATETBL) WITH ITS RATE AND APY                   *
      *            - WHAT-IF: RATE / APY / TIER FOR A BALANCE, TERM,   *
      *              SEGMENT, EMPLOYEE FLAG AND RELATIONSHIP BALANCE,  *
      *              PRICED BY RATETIER - THE SAME MODULE INTACCR AND  *
      *              CDMATUR USE, SO THE QUOTE MATCHES THE ACCRUAL     *
      *            - BONUS LINE DESCRIBES THE RELATIONSHIP / EMPLOYEE  *
      *              BONUS ELIGIBILITY OF THE PRODUCT                  *
      *            ENTER - INQUIRE       PF7 / PF8 - PREV / NEXT PROD  *
      *            PF3   - MENU          PF12      - CANCEL (CLEAR)    *
      *            PROMOTIONAL RATES ARE ACCOUNT-SPECIFIC AND ARE NOT  *
      *            QUOTED HERE (SEE ACCOUNT INQUIRY CB03).             *
      * FILES    : PRODMST (READ), BANKCTL (READ)                      *
      * LINKS    : RATETIER                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-10-03 RJT  ORIGINAL - TRUTH IN SAVINGS RATE SHEET         *
      * 1998-11-02 MKL  Y2K - AS-OF DATE CCYYMMDD                      *
      * 2008-11-24 DPS  APY PER TIER FROM RATETIER (WAS COMPUTED HERE  *
      *                 WITH 360 DAY YEAR - INC10442)                  *
      * 2016-02-08 DPS  RESP CHECKING - REMOVED HANDLE CONDITION       *
      * 2017-02-08 AKW  WHAT-IF BALANCE; ONLINE SAVINGS / CDNP ADDED   *
      * 2019-08-19 AKW  RELATIONSHIP BALANCE AND SEGMENT - CHG1655     *
      * 2021-03-02 AKW  EMPLOYEE FLAG - CHG1904                        *
      * 2023-02-06 JQP  CMM1 INDEX LINE - CHG2210                      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RATEINQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM                      PIC X(08) VALUE 'RATEINQ'.
       01  WS-TRANID                       PIC X(04) VALUE 'CB20'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-RESP2                        PIC S9(08) COMP.
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.

       01  WS-WORK.
           05  WS-ERROR-FLAG               PIC X(01).
               88  WS-ERROR                          VALUE 'Y'.
           05  WS-MESSAGE                  PIC X(79).
           05  WS-LINE-NO                  PIC 9(02) COMP.
           05  WS-PM-SAVE                  PIC 9(03) COMP.
           05  WS-PTR                      PIC 9(03) COMP.
           05  WS-WI-BALANCE               PIC S9(13)V99 COMP-3.
           05  WS-WI-REL-BAL               PIC S9(13)V99 COMP-3.
           05  WS-WI-TERM                  PIC 9(03).
           05  WS-WI-SEGMENT               PIC X(02).
           05  WS-WI-EMPLOYEE              PIC X(01).
           05  WS-TABLE-ID                 PIC X(04).
           05  WS-TIME-DEP                 PIC X(01).
           05  WS-REL-ELIG                 PIC X(01).
           05  WS-EMP-ELIG                 PIC X(01).
           05  WS-RATE-ED                  PIC ZZ9.999999.
           05  WS-RATE-ED2                 PIC Z9.99.
           05  WS-AMT-ED                   PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  WS-BAL-ED                   PIC ZZZ,ZZZ,ZZ9.
           05  WS-TERM-ED                  PIC ZZ9.
           05  WS-DATE-ED                  PIC 9999/99/99.
           05  WS-TYPE-TEXT                PIC X(05).

       01  WS-TIER-LINE.
           05  TL-TIER                     PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  TL-TYPE                     PIC X(05).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  TL-FROM                     PIC X(26).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  TL-RATE                     PIC ZZ9.999999.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  TL-APY                      PIC ZZ9.999999.
           05  FILLER                      PIC X(08) VALUE SPACES.

       01  WS-SHEET-LINES.
           05  WS-SHEET-LINE               PIC X(70) OCCURS 10.

       COPY RATETBL.
       COPY RTTPARM.
       COPY PRODREC.
       COPY BANKCTL.
       COPY RATINQS.
       COPY DFHAID.
       COPY DFHBMSCA.

       01  WS-COMMAREA.
           05  CA-STATE                    PIC X(01).
               88  CA-FIRST-TIME                     VALUE SPACE.
               88  CA-SHOWN                          VALUE 'S'.
           05  CA-PM-INDEX                 PIC 9(03).
           05  CA-PRODUCT                  PIC X(04).

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(08).

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
                       PERFORM 2000-INQUIRE
                   WHEN DFHPF7
                       PERFORM 3000-PREV-PRODUCT
                   WHEN DFHPF8
                       PERFORM 3100-NEXT-PRODUCT
                   WHEN OTHER
                       MOVE 'INVALID KEY PRESSED' TO WS-MESSAGE
                       PERFORM 8000-SEND-DATAONLY
               END-EVALUATE
           END-IF
           EXEC CICS RETURN TRANSID(WS-TRANID)
                COMMAREA(WS-COMMAREA) LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      * FIRST ENTRY - SHOW THE FIRST INTEREST-BEARING PRODUCT (HYSV)   *
      *----------------------------------------------------------------*
       1000-SEND-INITIAL.
           MOVE LOW-VALUES TO RATINQMO
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           PERFORM 8100-READ-CONTROL
           MOVE 1 TO CA-PM-INDEX
           PERFORM 3200-SKIP-NON-INTEREST-FWD
           PERFORM 2050-DEFAULT-WHAT-IF
           PERFORM 2500-BUILD-SHEET
           IF WS-MESSAGE = SPACES
               MOVE 'ENTER A PRODUCT AND WHAT-IF VALUES, OR PF7/PF8'
                 TO WS-MESSAGE
           END-IF
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('RATINQM') MAPSET('RATINQS')
                FROM(RATINQMO) ERASE RESP(WS-RESP) END-EXEC.

      *----------------------------------------------------------------*
      * ENTER - VALIDATE THE WHAT-IF INPUT AND PRICE IT                *
      *----------------------------------------------------------------*
       2000-INQUIRE.
           EXEC CICS RECEIVE MAP('RATINQM') MAPSET('RATINQS')
                INTO(RATINQMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           PERFORM 8100-READ-CONTROL
           PERFORM 2100-EDIT-SCREEN
           IF NOT WS-ERROR
               PERFORM 2500-BUILD-SHEET
           END-IF
           IF NOT WS-ERROR
               PERFORM 2600-WHAT-IF
           END-IF
           PERFORM 8000-SEND-DATAONLY.

      *    BLANK WHAT-IF FIELDS: $10,000, PRODUCT TERM, RETAIL, NOT
      *    AN EMPLOYEE, RELATIONSHIP = THE WHAT-IF BALANCE
       2050-DEFAULT-WHAT-IF.
           MOVE RT-PMAP-PRODUCT(CA-PM-INDEX) TO CA-PRODUCT
           MOVE 10000.00 TO WS-WI-BALANCE WS-WI-REL-BAL
           MOVE ZERO     TO WS-WI-TERM
           MOVE 'RT'     TO WS-WI-SEGMENT
           MOVE 'N'      TO WS-WI-EMPLOYEE.

       2100-EDIT-SCREEN.
           IF PRODL = 0 OR PRODI = SPACES OR LOW-VALUES
               MOVE CA-PRODUCT TO PRODI
           END-IF
           MOVE ZERO TO CA-PM-INDEX
           PERFORM VARYING RT-PM-IDX FROM 1 BY 1
                   UNTIL RT-PM-IDX > RT-PRODUCT-MAP-COUNT
               IF RT-PMAP-PRODUCT(RT-PM-IDX) = PRODI
                   SET CA-PM-INDEX TO RT-PM-IDX
               END-IF
           END-PERFORM
           IF CA-PM-INDEX = ZERO
               MOVE 'PRODUCT NOT ON THE RATE SHEET' TO WS-MESSAGE
               MOVE -1 TO PRODL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           MOVE PRODI TO CA-PRODUCT
           PERFORM 2050-DEFAULT-WHAT-IF

           IF WIBALL > 0 AND WIBALI NOT = SPACES
               IF FUNCTION TEST-NUMVAL(WIBALI) NOT = 0
                   MOVE 'WHAT-IF BALANCE MUST BE NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO WIBALL
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
               COMPUTE WS-WI-BALANCE = FUNCTION NUMVAL(WIBALI)
               MOVE WS-WI-BALANCE TO WS-WI-REL-BAL
           END-IF
           IF WS-WI-BALANCE < ZERO
               MOVE 'WHAT-IF BALANCE CANNOT BE NEGATIVE' TO WS-MESSAGE
               MOVE -1 TO WIBALL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           IF WIRELL > 0 AND WIRELI NOT = SPACES
               IF FUNCTION TEST-NUMVAL(WIRELI) NOT = 0
                   MOVE 'RELATIONSHIP BALANCE MUST BE NUMERIC'
                     TO WS-MESSAGE
                   MOVE -1 TO WIRELL
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
               COMPUTE WS-WI-REL-BAL = FUNCTION NUMVAL(WIRELI)
           END-IF
      *    THE RELATIONSHIP INCLUDES THIS ACCOUNT
           IF WS-WI-REL-BAL < WS-WI-BALANCE
               MOVE WS-WI-BALANCE TO WS-WI-REL-BAL
           END-IF
           IF WITERML > 0 AND WITERMI NOT = SPACES
               IF FUNCTION TEST-NUMVAL(WITERMI) NOT = 0
                   MOVE 'TERM MUST BE NUMERIC (MONTHS)' TO WS-MESSAGE
                   MOVE -1 TO WITERML
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
               COMPUTE WS-WI-TERM = FUNCTION NUMVAL(WITERMI)
           END-IF
           IF WISEGL > 0 AND WISEGI NOT = SPACES
               MOVE WISEGI TO WS-WI-SEGMENT
           END-IF
           IF WIEMPL > 0 AND WIEMPI NOT = SPACES
               IF WIEMPI NOT = 'Y' AND NOT = 'N'
                   MOVE 'EMPLOYEE FLAG MUST BE Y OR N' TO WS-MESSAGE
                   MOVE -1 TO WIEMPL
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
               MOVE WIEMPI TO WS-WI-EMPLOYEE
           END-IF.

      *----------------------------------------------------------------*
      * RATE SHEET - ONE LINE PER RATETBL TIER OF THE PRODUCT'S TABLE. *
      * EACH TIER IS PRICED THROUGH RATETIER AT ITS LOWER BOUND (OR    *
      * TERM) WITH NO BONUS, SO THE SHEET SHOWS THE BASE RATE / APY.   *
      *----------------------------------------------------------------*
       2500-BUILD-SHEET.
           MOVE RT-PMAP-PRODUCT(CA-PM-INDEX)  TO CA-PRODUCT PRODO
           MOVE RT-PMAP-TABLE-ID(CA-PM-INDEX) TO WS-TABLE-ID
           MOVE RT-PMAP-TIME-DEP(CA-PM-INDEX) TO WS-TIME-DEP
           MOVE RT-PMAP-REL-BONUS(CA-PM-INDEX) TO WS-REL-ELIG
           MOVE RT-PMAP-EMP-BONUS(CA-PM-INDEX) TO WS-EMP-ELIG
           MOVE CA-PRODUCT TO PROD-CODE
           EXEC CICS READ FILE('PRODMST') INTO(PRODUCT-RECORD)
                RIDFLD(PROD-CODE) RESP(WS-RESP) END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE PROD-NAME TO PRDNAMEO
           ELSE
               MOVE '** PRODUCT NOT ON PRODMST **' TO PRDNAMEO
               INITIALIZE PRODUCT-RECORD
           END-IF
           MOVE CTL-BUSINESS-DATE TO WS-DATE-ED
           MOVE WS-DATE-ED TO ASOFO CURDATEO

           MOVE SPACES TO WS-SHEET-LINES
           MOVE ZERO TO WS-LINE-NO
           IF WS-TABLE-ID = 'NONE'
               MOVE 'NOT INTEREST BEARING' TO WS-SHEET-LINE(1)
           ELSE
               PERFORM VARYING RT-TR-IDX FROM 1 BY 1
                       UNTIL RT-TR-IDX > RT-TIER-COUNT
                          OR WS-LINE-NO >= 10
                   IF RT-TIER-TABLE-ID(RT-TR-IDX) = WS-TABLE-ID
                       PERFORM 2510-FORMAT-TIER
                   END-IF
               END-PERFORM
           END-IF
           MOVE WS-SHEET-LINE(01) TO TLINE01O
           MOVE WS-SHEET-LINE(02) TO TLINE02O
           MOVE WS-SHEET-LINE(03) TO TLINE03O
           MOVE WS-SHEET-LINE(04) TO TLINE04O
           MOVE WS-SHEET-LINE(05) TO TLINE05O
           MOVE WS-SHEET-LINE(06) TO TLINE06O
           MOVE WS-SHEET-LINE(07) TO TLINE07O
           MOVE WS-SHEET-LINE(08) TO TLINE08O
           MOVE WS-SHEET-LINE(09) TO TLINE09O
           MOVE WS-SHEET-LINE(10) TO TLINE10O
           PERFORM 2700-BONUS-LINE.

       2510-FORMAT-TIER.
           ADD 1 TO WS-LINE-NO
           MOVE SPACES TO TL-FROM
           INITIALIZE RATETIER-PARMS
           MOVE CA-PRODUCT        TO RTT-PRODUCT-CODE
           MOVE CTL-BUSINESS-DATE TO RTT-AS-OF-DATE
           MOVE 'RT'              TO RTT-CUST-SEGMENT
           MOVE 'N'               TO RTT-EMPLOYEE-FLAG
           MOVE RT-TIER-ID(RT-TR-IDX) TO TL-TIER
           EVALUATE TRUE
               WHEN RT-TIER-BY-BALANCE(RT-TR-IDX)
                   MOVE 'BAL'   TO TL-TYPE
                   MOVE RT-TIER-LOW-BAL(RT-TR-IDX) TO RTT-BALANCE
                   MOVE RT-TIER-LOW-BAL(RT-TR-IDX) TO WS-AMT-ED
                   STRING '$' WS-AMT-ED ' AND OVER'
                          DELIMITED BY SIZE INTO TL-FROM
               WHEN RT-TIER-BY-TERM(RT-TR-IDX)
                   MOVE 'TERM'  TO TL-TYPE
                   MOVE RT-TIER-TERM(RT-TR-IDX) TO RTT-TERM-MONTHS
                   MOVE 1000.00 TO RTT-BALANCE
                   MOVE RT-TIER-TERM(RT-TR-IDX) TO WS-TERM-ED
                   STRING WS-TERM-ED ' MONTHS'
                          DELIMITED BY SIZE INTO TL-FROM
               WHEN RT-TIER-BY-INDEX(RT-TR-IDX)
                   MOVE 'INDEX' TO TL-TYPE
                   MOVE 1000000.00 TO RTT-BALANCE
                   MOVE RT-TIER-RATE(RT-TR-IDX) TO WS-RATE-ED2
                   STRING 'FED FUNDS LESS ' WS-RATE-ED2
                          DELIMITED BY SIZE INTO TL-FROM
           END-EVALUATE
           MOVE RTT-BALANCE TO RTT-REL-BALANCE
           EXEC CICS LINK PROGRAM('RATETIER')
                COMMAREA(RATETIER-PARMS)
                LENGTH(LENGTH OF RATETIER-PARMS) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL) OR RTT-RETURN-CODE > 04
               MOVE RT-TIER-RATE(RT-TR-IDX) TO TL-RATE
               MOVE ZERO TO TL-APY
           ELSE
               MOVE RTT-RESULT-RATE TO TL-RATE
               MOVE RTT-RESULT-APY  TO TL-APY
           END-IF
           MOVE WS-TIER-LINE TO WS-SHEET-LINE(WS-LINE-NO).

      *----------------------------------------------------------------*
      * WHAT-IF - FULL RATETIER PRICING INCLUDING BONUSES AND THE CAP  *
      *----------------------------------------------------------------*
       2600-WHAT-IF.
           INITIALIZE RATETIER-PARMS
           MOVE CA-PRODUCT        TO RTT-PRODUCT-CODE
           MOVE WS-WI-BALANCE     TO RTT-BALANCE
           MOVE WS-WI-REL-BAL     TO RTT-REL-BALANCE
           MOVE WS-WI-SEGMENT     TO RTT-CUST-SEGMENT
           MOVE WS-WI-TERM        TO RTT-TERM-MONTHS
           IF RTT-TERM-MONTHS = ZERO
               MOVE PROD-TERM-MONTHS TO RTT-TERM-MONTHS
           END-IF
           MOVE CTL-BUSINESS-DATE TO RTT-AS-OF-DATE
           MOVE ZERO              TO RTT-PROMO-RATE RTT-PROMO-EXP-DATE
           MOVE WS-WI-EMPLOYEE    TO RTT-EMPLOYEE-FLAG
           EXEC CICS LINK PROGRAM('RATETIER')
                COMMAREA(RATETIER-PARMS)
                LENGTH(LENGTH OF RATETIER-PARMS) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RATE MODULE UNAVAILABLE - TRY AGAIN' TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF

           MOVE WS-WI-BALANCE TO WS-AMT-ED
           MOVE WS-AMT-ED TO WIBALO
           MOVE WS-WI-REL-BAL TO WS-AMT-ED
           MOVE WS-AMT-ED TO WIRELO
           MOVE RTT-TERM-MONTHS TO WS-TERM-ED
           MOVE WS-TERM-ED TO WITERMO
           MOVE WS-WI-SEGMENT TO WISEGO
           MOVE WS-WI-EMPLOYEE TO WIEMPO

           EVALUATE RTT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE RTT-RESULT-RATE TO WS-RATE-ED
                   MOVE WS-RATE-ED TO RRATEO
                   MOVE RTT-RESULT-APY TO WS-RATE-ED
                   MOVE WS-RATE-ED TO RAPYO
                   MOVE RTT-TIER-ID TO RTIERO
                   IF RTT-RETURN-CODE = 04
                       MOVE 'RATE LIMITED BY ALCO CAP / INDEX FLOOR'
                         TO WS-MESSAGE
                   ELSE
                       MOVE 'VARIABLE RATE - MAY CHANGE AT ANY TIME'
                         TO WS-MESSAGE
                   END-IF
               WHEN 08
                   MOVE ZERO TO WS-RATE-ED
                   MOVE WS-RATE-ED TO RRATEO RAPYO
                   MOVE 'NONE' TO RTIERO
                   MOVE 'PRODUCT IS NOT INTEREST BEARING' TO WS-MESSAGE
               WHEN OTHER
                   MOVE SPACES TO RRATEO RAPYO RTIERO
                   MOVE 'NO RATE ON FILE FOR THIS PRODUCT - CALL ALCO'
                     TO WS-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BONUS DISCLOSURE TEXT - VALUES FROM RATETBL PART 3             *
      *----------------------------------------------------------------*
       2700-BONUS-LINE.
           MOVE SPACES TO BONUSO
           MOVE 1 TO WS-PTR
           EVALUATE TRUE
               WHEN WS-TIME-DEP = 'Y'
                   STRING 'FIXED RATE FOR THE TERM - NO RELATIONSHIP '
                          'OR EMPLOYEE BONUS ON CDS'
                          DELIMITED BY SIZE INTO BONUSO
                          WITH POINTER WS-PTR
               WHEN WS-TABLE-ID = 'NONE'
                   CONTINUE
               WHEN WS-REL-ELIG = 'Y'
                   MOVE RT-REL-BONUS-MIN-BAL TO WS-BAL-ED
                   MOVE RT-REL-BONUS-RATE TO WS-RATE-ED2
                   STRING 'BONUS +' WS-RATE-ED2 '% AT $'
                          FUNCTION TRIM(WS-BAL-ED) ' RELATIONSHIP (PW +'
                          DELIMITED BY SIZE INTO BONUSO
                          WITH POINTER WS-PTR
                   MOVE RT-PW-BONUS-RATE TO WS-RATE-ED2
                   STRING WS-RATE-ED2 '%)'
                          DELIMITED BY SIZE INTO BONUSO
                          WITH POINTER WS-PTR
                   IF WS-EMP-ELIG = 'Y'
                       MOVE RT-EMP-BONUS-RATE TO WS-RATE-ED2
                       STRING '; EMPLOYEE +' WS-RATE-ED2 '%'
                              DELIMITED BY SIZE INTO BONUSO
                              WITH POINTER WS-PTR
                   END-IF
               WHEN OTHER
                   STRING 'NO RATE BONUS ON THIS PRODUCT'
                          DELIMITED BY SIZE INTO BONUSO
                          WITH POINTER WS-PTR
           END-EVALUATE
           IF WS-TIME-DEP NOT = 'Y' AND WS-TABLE-ID NOT = 'NONE'
               MOVE RT-MAX-DEPOSIT-RATE TO WS-RATE-ED2
               STRING '  MAX ' WS-RATE-ED2 '%'
                      DELIMITED BY SIZE INTO BONUSO
                      WITH POINTER WS-PTR
           END-IF.

      *----------------------------------------------------------------*
      * PF7 / PF8 - STEP THROUGH THE PRODUCT MAP, SKIPPING PRODUCTS    *
      * WITHOUT A RATE TABLE                                           *
      *----------------------------------------------------------------*
       3000-PREV-PRODUCT.
           MOVE LOW-VALUES TO RATINQMO
           MOVE SPACES TO WS-MESSAGE
           PERFORM 8100-READ-CONTROL
           IF CA-PM-INDEX < 1 OR CA-PM-INDEX > RT-PRODUCT-MAP-COUNT
               MOVE 1 TO CA-PM-INDEX
           END-IF
           MOVE CA-PM-INDEX TO WS-PM-SAVE
           PERFORM WITH TEST AFTER
                   UNTIL RT-PMAP-TABLE-ID(CA-PM-INDEX) NOT = 'NONE'
                      OR CA-PM-INDEX = WS-PM-SAVE
               IF CA-PM-INDEX = 1
                   MOVE RT-PRODUCT-MAP-COUNT TO CA-PM-INDEX
               ELSE
                   SUBTRACT 1 FROM CA-PM-INDEX
               END-IF
           END-PERFORM
           PERFORM 2050-DEFAULT-WHAT-IF
           PERFORM 2500-BUILD-SHEET
           PERFORM 2600-WHAT-IF
           PERFORM 8000-SEND-DATAONLY.

       3100-NEXT-PRODUCT.
           MOVE LOW-VALUES TO RATINQMO
           MOVE SPACES TO WS-MESSAGE
           PERFORM 8100-READ-CONTROL
           IF CA-PM-INDEX < 1 OR CA-PM-INDEX > RT-PRODUCT-MAP-COUNT
               MOVE 1 TO CA-PM-INDEX
           END-IF
           IF CA-PM-INDEX = RT-PRODUCT-MAP-COUNT
               MOVE 1 TO CA-PM-INDEX
           ELSE
               ADD 1 TO CA-PM-INDEX
           END-IF
           PERFORM 3200-SKIP-NON-INTEREST-FWD
           PERFORM 2050-DEFAULT-WHAT-IF
           PERFORM 2500-BUILD-SHEET
           PERFORM 2600-WHAT-IF
           PERFORM 8000-SEND-DATAONLY.

       3200-SKIP-NON-INTEREST-FWD.
           MOVE CA-PM-INDEX TO WS-PM-SAVE
           PERFORM UNTIL RT-PMAP-TABLE-ID(CA-PM-INDEX) NOT = 'NONE'
               IF CA-PM-INDEX = RT-PRODUCT-MAP-COUNT
                   MOVE 1 TO CA-PM-INDEX
               ELSE
                   ADD 1 TO CA-PM-INDEX
               END-IF
               IF CA-PM-INDEX = WS-PM-SAVE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
      * COMMON ROUTINES                                                *
      *----------------------------------------------------------------*
       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           MOVE CTL-BUSINESS-DATE TO WS-DATE-ED
           MOVE WS-DATE-ED TO CURDATEO
           EXEC CICS SEND MAP('RATINQM') MAPSET('RATINQS')
                FROM(RATINQMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.

       8100-READ-CONTROL.
           EXEC CICS READ FILE('BANKCTL') INTO(BANK-CONTROL-RECORD)
                RIDFLD(WS-CTL-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'BANK CONTROL RECORD UNAVAILABLE' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.
