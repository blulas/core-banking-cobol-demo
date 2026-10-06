      *================================================================*
      * PROGRAM  : ACHVALID                                            *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : NACHA ENTRY DETAIL VALIDATION - SHARED BY INBOUND   *
      *            (ACHIN, RDFI) AND ORIGINATION (PAYPROC, PAYINIT).   *
      *              - IAT NOT SUPPORTED, SEC MUST BE PPD CCD CTX WEB  *
      *              - ABA ROUTING: FED DISTRICT PREFIX AND 3-7-1      *
      *                CHECK DIGIT                                     *
      *              - TRANSACTION CODES 22/27/32/37 (LIVE) AND        *
      *                23/28/33/38 (PRENOTE, MUST BE ZERO DOLLAR)      *
      *              - LIVE ENTRIES MUST BE > 0 AND <= 99,999,999.99   *
      *              - SEC VERSUS ACCOUNT TYPE: PPD/WEB CONSUMER,      *
      *                CCD/CTX CORPORATE; CORPORATE SEC DEBIT TO A     *
      *                CONSUMER ACCOUNT = R05; WEB DEBIT NEEDS AN      *
      *                AUTHORIZATION ON FILE (R10)                     *
      * LINKAGE  : AVLPARM                                             *
      * CALLED BY: ACHIN, PAYPROC (BATCH), PAYINIT (ONLINE)            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-02-12 RJT  ORIGINAL - CHECK DIGIT AND TRAN CODE           *
      * 2002-01-07 BHS  IAT / CROSS BORDER ENTRIES NOT ACCEPTED        *
      * 2009-09-18 DPS  IAT FORMAT MANDATORY - STILL NOT SUPPORTED     *
      * 2013-09-16 DPS  WEB DEBIT AUTHORIZATION, CTX                   *
      * 2015-06-22 DPS  R05 CORPORATE SEC DEBIT TO CONSUMER ACCOUNT    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACHVALID.
       AUTHOR. R J TRAVERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SEC-VALUES.
           05  FILLER PIC X(12) VALUE 'PPDCCDCTXWEB'.
       01  WS-SEC-TABLE REDEFINES WS-SEC-VALUES.
           05  WS-SEC-ALLOWED PIC X(03) OCCURS 4 TIMES
                              INDEXED BY SEC-IDX.

      *    TRAN CODE(2) DR/CR(1) KIND(1) PRENOTE(1)
       01  WS-TC-VALUES.
           05  FILLER PIC X(05) VALUE '22CDN'.
           05  FILLER PIC X(05) VALUE '23CDY'.
           05  FILLER PIC X(05) VALUE '27DDN'.
           05  FILLER PIC X(05) VALUE '28DDY'.
           05  FILLER PIC X(05) VALUE '32CSN'.
           05  FILLER PIC X(05) VALUE '33CSY'.
           05  FILLER PIC X(05) VALUE '37DSN'.
           05  FILLER PIC X(05) VALUE '38DSY'.
       01  WS-TC-TABLE REDEFINES WS-TC-VALUES.
           05  WS-TC-ENTRY OCCURS 8 TIMES INDEXED BY TCV-IDX.
               10  WS-TCV-CODE             PIC X(02).
               10  WS-TCV-DR-CR            PIC X(01).
               10  WS-TCV-KIND             PIC X(01).
               10  WS-TCV-PRENOTE          PIC X(01).

       01  WS-ABA-WEIGHTS-VALUES.
           05  FILLER PIC X(08) VALUE '37137137'.
       01  WS-ABA-WEIGHTS REDEFINES WS-ABA-WEIGHTS-VALUES.
           05  WS-ABA-WEIGHT  PIC 9(01) OCCURS 8 TIMES.

       01  WS-CONSTANTS.
           05  WS-MAX-ENTRY-AMOUNT         PIC S9(13)V99 COMP-3
                                           VALUE 99999999.99.

       01  WS-WORK.
           05  WS-ROUTING-N                PIC 9(09).
           05  WS-ROUTING-X REDEFINES WS-ROUTING-N.
               10  WS-RT-DIGIT             PIC 9(01) OCCURS 9 TIMES.
           05  WS-RT-PREFIX                PIC 9(02).
           05  WS-ABA-SUM                  PIC 9(04).
           05  WS-I                        PIC 9(02).
           05  WS-SEC-OK                   PIC X(01).

       LINKAGE SECTION.
       COPY AVLPARM.

       PROCEDURE DIVISION USING ACH-VALID-PARMS.
       0000-MAIN.
           MOVE 'A'    TO AVL-RESULT
           MOVE 'OKAY' TO AVL-REASON-CODE
           MOVE SPACES TO AVL-RETURN-REASON AVL-DR-CR AVL-ACCT-KIND
           MOVE 'ENTRY ACCEPTED' TO AVL-REASON-TEXT
           MOVE 'N'    TO AVL-PRENOTE-FLAG
           MOVE ZERO   TO AVL-COMPUTED-CHECK-DIGIT AVL-RETURN-CODE

           PERFORM 1000-CHECK-SEC-CODE
           IF NOT AVL-REJECT
               PERFORM 2000-CHECK-ROUTING
           END-IF
           IF NOT AVL-REJECT
               PERFORM 3000-CHECK-TRAN-CODE
           END-IF
           IF NOT AVL-REJECT
               PERFORM 4000-CHECK-AMOUNT
           END-IF
           IF NOT AVL-REJECT
               PERFORM 5000-CHECK-SEC-VS-ACCOUNT
           END-IF

           EVALUATE TRUE
               WHEN AVL-REJECT
                   MOVE 08 TO AVL-RETURN-CODE
               WHEN AVL-WARNING
                   MOVE 04 TO AVL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO AVL-RETURN-CODE
           END-EVALUATE
           GOBACK.

      *----------------------------------------------------------------*
       1000-CHECK-SEC-CODE.
           IF AVL-SEC-CODE = 'IAT'
               MOVE 'IATX' TO AVL-REASON-CODE
               MOVE 'R17'  TO AVL-RETURN-REASON
               MOVE 'IAT ENTRIES NOT SUPPORTED' TO AVL-REASON-TEXT
               SET AVL-REJECT TO TRUE
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-SEC-OK
           SET SEC-IDX TO 1
           SEARCH WS-SEC-ALLOWED
               AT END
                   CONTINUE
               WHEN WS-SEC-ALLOWED(SEC-IDX) = AVL-SEC-CODE
                   MOVE 'Y' TO WS-SEC-OK
           END-SEARCH
           IF WS-SEC-OK = 'N'
               MOVE 'SECU' TO AVL-REASON-CODE
               MOVE 'R17'  TO AVL-RETURN-REASON
               MOVE 'SEC CODE NOT SUPPORTED' TO AVL-REASON-TEXT
               SET AVL-REJECT TO TRUE
           END-IF.

      *----------------------------------------------------------------*
      * ABA ROUTING TRANSIT NUMBER                                     *
      *   PREFIX 00-12 FED DISTRICTS, 21-32 THRIFTS, 61-72 ELECTRONIC, *
      *   80 TRAVELERS CHECKS.  CHECK DIGIT: WEIGHTS 3 7 1 3 7 1 3 7,  *
      *   DIGIT 9 = (10 - (SUM MOD 10)) MOD 10                         *
      *----------------------------------------------------------------*
       2000-CHECK-ROUTING.
           IF AVL-ROUTING IS NOT NUMERIC
               MOVE 'RTNN' TO AVL-REASON-CODE
               MOVE 'R28'  TO AVL-RETURN-REASON
               MOVE 'ROUTING NUMBER NOT NUMERIC' TO AVL-REASON-TEXT
               SET AVL-REJECT TO TRUE
               EXIT PARAGRAPH
           END-IF
           MOVE AVL-ROUTING TO WS-ROUTING-X
           COMPUTE WS-RT-PREFIX = WS-RT-DIGIT(1) * 10 + WS-RT-DIGIT(2)
           IF NOT ((WS-RT-PREFIX >= 00 AND WS-RT-PREFIX <= 12)
                OR (WS-RT-PREFIX >= 21 AND WS-RT-PREFIX <= 32)
                OR (WS-RT-PREFIX >= 61 AND WS-RT-PREFIX <= 72)
                OR  WS-RT-PREFIX = 80)
               MOVE 'RTPX' TO AVL-REASON-CODE
               MOVE 'R28'  TO AVL-RETURN-REASON
               MOVE 'INVALID ROUTING NUMBER PREFIX' TO AVL-REASON-TEXT
               SET AVL-REJECT TO TRUE
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO WS-ABA-SUM
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               COMPUTE WS-ABA-SUM = WS-ABA-SUM
                       + WS-RT-DIGIT(WS-I) * WS-ABA-WEIGHT(WS-I)
           END-PERFORM
           COMPUTE AVL-COMPUTED-CHECK-DIGIT =
               FUNCTION MOD(10 - FUNCTION MOD(WS-ABA-SUM, 10), 10)
           IF AVL-COMPUTED-CHECK-DIGIT NOT = WS-RT-DIGIT(9)
               MOVE 'RTCK' TO AVL-REASON-CODE
               MOVE 'R28'  TO AVL-RETURN-REASON
               MOVE 'ROUTING NUMBER CHECK DIGIT ERROR'
                 TO AVL-REASON-TEXT
               SET AVL-REJECT TO TRUE
           END-IF.

      *----------------------------------------------------------------*
       3000-CHECK-TRAN-CODE.
           SET TCV-IDX TO 1
           SEARCH WS-TC-ENTRY
               AT END
                   MOVE 'TCOD' TO AVL-REASON-CODE
                   MOVE 'R17'  TO AVL-RETURN-REASON
                   MOVE 'TRANSACTION CODE NOT SUPPORTED'
                     TO AVL-REASON-TEXT
                   SET AVL-REJECT TO TRUE
               WHEN WS-TCV-CODE(TCV-IDX) = AVL-TRAN-CODE
                   MOVE WS-TCV-DR-CR(TCV-IDX)   TO AVL-DR-CR
                   MOVE WS-TCV-KIND(TCV-IDX)    TO AVL-ACCT-KIND
                   MOVE WS-TCV-PRENOTE(TCV-IDX) TO AVL-PRENOTE-FLAG
           END-SEARCH.

      *----------------------------------------------------------------*
       4000-CHECK-AMOUNT.
           EVALUATE TRUE
               WHEN AVL-IS-PRENOTE AND AVL-AMOUNT NOT = ZERO
                   MOVE 'PREA' TO AVL-REASON-CODE
                   MOVE 'R17'  TO AVL-RETURN-REASON
                   MOVE 'PRENOTE MUST BE ZERO DOLLAR'
                     TO AVL-REASON-TEXT
                   SET AVL-REJECT TO TRUE
               WHEN AVL-IS-PRENOTE
                   CONTINUE
               WHEN AVL-AMOUNT NOT > ZERO
                   MOVE 'AMT0' TO AVL-REASON-CODE
                   MOVE 'R17'  TO AVL-RETURN-REASON
                   MOVE 'LIVE ENTRY AMOUNT MUST BE > ZERO'
                     TO AVL-REASON-TEXT
                   SET AVL-REJECT TO TRUE
               WHEN AVL-AMOUNT > WS-MAX-ENTRY-AMOUNT
                   MOVE 'AMTX' TO AVL-REASON-CODE
                   MOVE 'R17'  TO AVL-RETURN-REASON
                   MOVE 'AMOUNT EXCEEDS NACHA FIELD SIZE'
                     TO AVL-REASON-TEXT
                   SET AVL-REJECT TO TRUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEC CODE VERSUS RECEIVING ACCOUNT TYPE                         *
      *----------------------------------------------------------------*
       5000-CHECK-SEC-VS-ACCOUNT.
           EVALUATE TRUE
               WHEN (AVL-SEC-CODE = 'CCD' OR 'CTX')
                AND AVL-CONSUMER-ACCT AND AVL-DR-CR = 'D'
                   MOVE 'SECX' TO AVL-REASON-CODE
                   MOVE 'R05'  TO AVL-RETURN-REASON
                   MOVE 'CORPORATE SEC DEBIT TO CONSUMER ACCOUNT'
                     TO AVL-REASON-TEXT
                   SET AVL-REJECT TO TRUE
               WHEN (AVL-SEC-CODE = 'CCD' OR 'CTX')
                AND AVL-CONSUMER-ACCT
                   MOVE 'SECW' TO AVL-REASON-CODE
                   MOVE 'CORPORATE SEC CREDIT TO CONSUMER ACCOUNT'
                     TO AVL-REASON-TEXT
                   SET AVL-WARNING TO TRUE
               WHEN AVL-SEC-CODE = 'WEB' AND AVL-CORPORATE-ACCT
                   MOVE 'WEBC' TO AVL-REASON-CODE
                   MOVE 'R17'  TO AVL-RETURN-REASON
                   MOVE 'WEB ENTRY TO NON-CONSUMER ACCOUNT'
                     TO AVL-REASON-TEXT
                   SET AVL-REJECT TO TRUE
               WHEN AVL-SEC-CODE = 'WEB' AND AVL-DR-CR = 'D'
                AND NOT AVL-AUTH-ON-FILE
                   MOVE 'WEBA' TO AVL-REASON-CODE
                   MOVE 'R10'  TO AVL-RETURN-REASON
                   MOVE 'WEB DEBIT WITHOUT AUTHORIZATION'
                     TO AVL-REASON-TEXT
                   SET AVL-REJECT TO TRUE
               WHEN AVL-SEC-CODE = 'PPD' AND AVL-CORPORATE-ACCT
                   MOVE 'SECW' TO AVL-REASON-CODE
                   MOVE 'CONSUMER SEC ENTRY TO CORPORATE ACCOUNT'
                     TO AVL-REASON-TEXT
                   SET AVL-WARNING TO TRUE
           END-EVALUATE.
