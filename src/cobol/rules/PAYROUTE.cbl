      *================================================================*
      * PROGRAM  : PAYROUTE                                            *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PAYMENT RAIL SELECTION WHEN THE CUSTOMER ASKS FOR   *
      *            THE FASTEST OR THE CHEAPEST WAY TO PAY.             *
      *            FASTEST:                                            *
      *              BENEFICIARY ON OUR BOOKS           -> BOOK        *
      *              BENEFICIARY OUTSIDE THE US         -> INTL WIRE   *
      *              AMOUNT >= 1,000,000                -> WIRE        *
      *              URGENT, BEFORE SDACH CUTOFF        -> SDACH       *
      *              URGENT, AFTER SDACH CUTOFF         -> WIRE        *
      *              OTHERWISE                          -> ACH         *
      *            CHEAPEST:                                           *
      *              BOOK / INTL AS ABOVE, URGENT BEFORE SDACH CUTOFF  *
      *              AND UNDER 1,000,000 -> SDACH, OTHERWISE ACH       *
      *            RETURNS THE CUSTOMER FEE AND ESTIMATED SETTLEMENT   *
      *            DAYS FOR THE CHOSEN RAIL (DISCLOSED ON SCREEN).     *
      * LINKAGE  : PRTPARM                                             *
      * CALLED BY: PAYINIT                                             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2017-03-20 AKW  ORIGINAL - MARCUS PAY ANYONE PROJECT           *
      * 2018-01-15 AKW  WIRE FEE 25.00 -> 30.00 PER PRICING MEMO PM-18 *
      * 2022-03-18 AKW  CHG2207 SDACH $1MM ENTRY LIMIT                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROUTE.
       AUTHOR. A K WINSLOW.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * RAIL TABLE: RAIL(1) NAME(10) FEE 9(5)V99 SETTLE-DAYS(1)        *
      *----------------------------------------------------------------*
       01  WS-RAIL-VALUES.
           05  FILLER PIC X(19) VALUE 'BBOOK      00000000'.
           05  FILLER PIC X(19) VALUE 'AACH       00000001'.
           05  FILLER PIC X(19) VALUE 'SSAME DAY  00005000'.
           05  FILLER PIC X(19) VALUE 'WWIRE      00030000'.
           05  FILLER PIC X(19) VALUE 'IINTL WIRE 00045001'.
       01  WS-RAIL-TABLE REDEFINES WS-RAIL-VALUES.
           05  WS-RAIL-ENTRY OCCURS 5 TIMES INDEXED BY RT-IDX.
               10  WS-RT-RAIL              PIC X(01).
               10  WS-RT-NAME              PIC X(10).
               10  WS-RT-FEE               PIC 9(05)V99.
               10  WS-RT-SETTLE-DAYS       PIC 9(01).

       01  WS-CONSTANTS.
           05  WS-OUR-ABA                  PIC X(09) VALUE '026000013'.
           05  WS-LARGE-VALUE              PIC S9(13)V99 COMP-3
                                           VALUE 1000000.00.
           05  WS-DFLT-SDACH-CUTOFF        PIC 9(04) VALUE 1430.

       01  WS-WORK.
           05  WS-COUNTRY                  PIC X(02).
           05  WS-BEFORE-SDACH             PIC X(01).
               88  WS-IS-BEFORE-SDACH                VALUE 'Y'.

       LINKAGE SECTION.
       COPY PRTPARM.

       PROCEDURE DIVISION USING PAY-ROUTE-PARMS.
       0000-MAIN.
           MOVE ZERO   TO PRT-RETURN-CODE PRT-FEE-AMOUNT
                          PRT-EST-SETTLE-DAYS
           MOVE SPACES TO PRT-RAIL PRT-RAIL-NAME
                          PRT-REASON-CODE PRT-REASON-TEXT
           IF PRT-AMOUNT NOT > ZERO
              OR NOT (PRT-FASTEST OR PRT-CHEAPEST)
               MOVE 12 TO PRT-RETURN-CODE
               MOVE 'ERR ' TO PRT-REASON-CODE
               MOVE 'INVALID AMOUNT OR PREFERENCE' TO PRT-REASON-TEXT
               GOBACK
           END-IF

           MOVE PRT-BENE-COUNTRY TO WS-COUNTRY
           IF WS-COUNTRY = SPACES
               MOVE 'US' TO WS-COUNTRY
           END-IF
           IF PRT-SDACH-CUTOFF = ZERO
               MOVE WS-DFLT-SDACH-CUTOFF TO PRT-SDACH-CUTOFF
           END-IF
           IF PRT-REQUEST-TIME NOT > PRT-SDACH-CUTOFF
               MOVE 'Y' TO WS-BEFORE-SDACH
           ELSE
               MOVE 'N' TO WS-BEFORE-SDACH
           END-IF
           IF PRT-BENE-ROUTING = WS-OUR-ABA
               MOVE 'Y' TO PRT-INTERNAL-FLAG
           END-IF

           IF PRT-FASTEST
               PERFORM 2000-FASTEST
           ELSE
               PERFORM 3000-CHEAPEST
           END-IF
           PERFORM 4000-PRICE-RAIL
           GOBACK.

      *----------------------------------------------------------------*
       2000-FASTEST.
           EVALUATE TRUE
               WHEN PRT-BENE-IS-INTERNAL
                   MOVE 'B'    TO PRT-RAIL
                   MOVE 'INTR' TO PRT-REASON-CODE
                   MOVE 'BENEFICIARY ACCOUNT IS ON US'
                     TO PRT-REASON-TEXT
               WHEN WS-COUNTRY NOT = 'US'
                   MOVE 'I'    TO PRT-RAIL
                   MOVE 'INTL' TO PRT-REASON-CODE
                   MOVE 'BENEFICIARY OUTSIDE UNITED STATES'
                     TO PRT-REASON-TEXT
               WHEN PRT-AMOUNT NOT < WS-LARGE-VALUE
                   MOVE 'W'    TO PRT-RAIL
                   MOVE 'LGAM' TO PRT-REASON-CODE
                   MOVE 'LARGE VALUE - FEDWIRE' TO PRT-REASON-TEXT
               WHEN PRT-IS-URGENT AND WS-IS-BEFORE-SDACH
                   MOVE 'S'    TO PRT-RAIL
                   MOVE 'SDOK' TO PRT-REASON-CODE
                   MOVE 'URGENT - SAME DAY ACH WINDOW OPEN'
                     TO PRT-REASON-TEXT
               WHEN PRT-IS-URGENT
                   MOVE 'W'    TO PRT-RAIL
                   MOVE 'SDLT' TO PRT-REASON-CODE
                   MOVE 'URGENT - AFTER SAME DAY ACH CUTOFF'
                     TO PRT-REASON-TEXT
               WHEN OTHER
                   MOVE 'A'    TO PRT-RAIL
                   MOVE 'STD ' TO PRT-REASON-CODE
                   MOVE 'NOT URGENT - STANDARD ACH' TO PRT-REASON-TEXT
           END-EVALUATE.

      *----------------------------------------------------------------*
       3000-CHEAPEST.
           EVALUATE TRUE
               WHEN PRT-BENE-IS-INTERNAL
                   MOVE 'B'    TO PRT-RAIL
                   MOVE 'INTR' TO PRT-REASON-CODE
                   MOVE 'BENEFICIARY ACCOUNT IS ON US'
                     TO PRT-REASON-TEXT
               WHEN WS-COUNTRY NOT = 'US'
                   MOVE 'I'    TO PRT-RAIL
                   MOVE 'INTL' TO PRT-REASON-CODE
                   MOVE 'BENEFICIARY OUTSIDE UNITED STATES'
                     TO PRT-REASON-TEXT
               WHEN PRT-IS-URGENT AND WS-IS-BEFORE-SDACH
                AND PRT-AMOUNT < WS-LARGE-VALUE
                   MOVE 'S'    TO PRT-RAIL
                   MOVE 'SDOK' TO PRT-REASON-CODE
                   MOVE 'URGENT - LOWEST COST SAME DAY OPTION'
                     TO PRT-REASON-TEXT
               WHEN OTHER
                   MOVE 'A'    TO PRT-RAIL
                   MOVE 'CHEP' TO PRT-REASON-CODE
                   MOVE 'LOWEST COST - STANDARD ACH' TO PRT-REASON-TEXT
           END-EVALUATE.

      *----------------------------------------------------------------*
       4000-PRICE-RAIL.
           SET RT-IDX TO 1
           SEARCH WS-RAIL-ENTRY
               AT END
                   MOVE 12 TO PRT-RETURN-CODE
               WHEN WS-RT-RAIL(RT-IDX) = PRT-RAIL
                   MOVE WS-RT-NAME(RT-IDX)        TO PRT-RAIL-NAME
                   MOVE WS-RT-FEE(RT-IDX)         TO PRT-FEE-AMOUNT
                   MOVE WS-RT-SETTLE-DAYS(RT-IDX) TO PRT-EST-SETTLE-DAYS
           END-SEARCH.
