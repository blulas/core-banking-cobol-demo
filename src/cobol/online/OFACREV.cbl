      *================================================================*
      * PROGRAM  : OFACREV                                             *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS    : CB61                                                *
      * MAPSET   : OFACRVS  (MAP OFACRVM)                              *
      * PURPOSE  : OFAC POTENTIAL MATCH REVIEW. SHOWS A CUSTOMER WITH  *
      *            CUST-OFAC-STATUS H (SET BY OFACBAT OR AT ONBOARDING)*
      *            TOGETHER WITH THE SDN ENTRY IT MATCHED (TAKEN FROM  *
      *            THE MOST RECENT OPEN OFAC ALERT IN ALERTKS) AND THE *
      *            CUSTOMER'S ACCOUNTS. THE ANALYST DECIDES:           *
      *              R - TRUE MATCH (CONFIRM):                         *
      *                  CUST-STATUS B (BLOCKED), CUST-OFAC-STATUS R,  *
      *                  EVERY ACCOUNT OWNED (PRIMARY) SET TO FROZEN   *
      *                  WITH RESTRICT REASON 'OF'. BLOCKED PROPERTY   *
      *                  REPORT DUE TO OFAC WITHIN 10 BUSINESS DAYS    *
      *                  (31 CFR 501.603). ALERT DISPOSITION S.        *
      *              C - FALSE POSITIVE (CLEAR):                       *
      *                  CUST-OFAC-STATUS C, SUPPRESSION RECORD WRITTEN*
      *                  TO OFACSUP FOR 365 DAYS SO THE SAME CUSTOMER /*
      *                  SDN ENTRY PAIR IS NOT RE-ALERTED. ALERT       *
      *                  DISPOSITION C.                                *
      *            ONLY COMPLIANCE USERS (USERID CMP*) MAY DECIDE.     *
      *            A REASON OF AT LEAST 10 CHARACTERS IS REQUIRED.     *
      *            A CONFIRM NEEDS PF10 (ENTER ONLY VALIDATES).        *
      * FILES    : CUSTMST (READ/UPDATE), ACCTMST + PATH ACCTCUS       *
      *            (BROWSE/UPDATE), ALERTKS (BROWSE/UPDATE),           *
      *            OFACSUP (WRITE/REWRITE), BANKCTL (READ)             *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2006-08-30 BHS  ORIGINAL                                       *
      * 2009-03-16 BHS  INC18822 FREEZE JOINT ACCOUNTS VIA PRIMARY ONLY*
      * 2014-09-15 DPS  CHG1260 FALSE POSITIVE SUPPRESSION (OFACSUP)   *
      * 2017-06-12 AKW  INC33410 CONFIRMED STATUS R NOT REVERSIBLE HERE*
      * 2019-11-04 AKW  MV-0193 SCORE / RULE DISPLAYED                 *
      * 2021-08-09 AKW  CHG2032 DECISIONS RESTRICTED TO CMP* USERS     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OFACREV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-NAME                 PIC X(08) VALUE 'OFACREV'.
       01  WS-TRANID                       PIC X(04) VALUE 'CB61'.
       01  WS-MAPSET                       PIC X(07) VALUE 'OFACRVS'.
       01  WS-MAPNAME                      PIC X(07) VALUE 'OFACRVM'.

      *----------------------------------------------------------------*
      * BUSINESS RULE CONSTANTS                                        *
      *----------------------------------------------------------------*
       01  WS-RULES.
      *    OFAC POLICY 4.3 - CLEARED FALSE POSITIVES SUPPRESSED 1 YEAR
           05  WS-SUPPRESS-DAYS            PIC S9(05) VALUE 365.
      *    BLOCKED PROPERTY REPORT - 10 BUSINESS DAYS
           05  WS-BLOCK-RPT-BUS-DAYS       PIC S9(05) VALUE 10.
      *    HOW FAR BACK TO LOOK FOR THE ALERT THAT RAISED THE MATCH
           05  WS-ALERT-LOOKBACK-DAYS      PIC S9(05) VALUE -45.
           05  WS-MIN-REASON-LEN           PIC 9(02) VALUE 10.
           05  WS-COMPLIANCE-PREFIX        PIC X(03) VALUE 'CMP'.
           05  WS-FREEZE-REASON            PIC X(02) VALUE 'OF'.

       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-USERID                       PIC X(08).
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.
       01  WS-CUST-KEY                     PIC X(10).
       01  WS-AIX-KEY                      PIC X(10).
       01  WS-ACCT-KEY                     PIC X(12).
       01  WS-ALRT-KEY                     PIC X(14).
       01  WS-ALRT-FOUND-KEY               PIC X(14).
       01  WS-FOUND                        PIC X(01).
           88  WS-WAS-FOUND                          VALUE 'Y'.
       01  WS-END-OF-FILE                  PIC X(01).
       01  WS-ROW                          PIC 9(02) COMP.
       01  WS-FROZEN-COUNT                 PIC 9(03).
       01  WS-REASON-LEN                   PIC 9(02).
       01  WS-MESSAGE                      PIC X(78).
       01  WS-LOOKBACK-DATE                PIC 9(08).
       01  WS-SUPPRESS-UNTIL               PIC 9(08).
       01  WS-BLOCK-RPT-DUE                PIC 9(08).
       01  WS-BAL-ED                       PIC -ZZZ,ZZZ,ZZ9.99.
       01  WS-CNT-ED                       PIC ZZ9.

      *    PARSED FROM THE OFAC ALERT DESCRIPTION WRITTEN BY OFACBAT:
      *    'CUST SDN nnnnnn SCORE nnn RULE MATCHED NAME'
       01  WS-ALERT-PARSE.
           05  WS-AP-SUBJECT               PIC X(04).
           05  WS-AP-SDN-LIT               PIC X(04).
           05  WS-AP-ENT-NUM               PIC X(06).
           05  WS-AP-SCORE-LIT             PIC X(06).
           05  WS-AP-SCORE                 PIC X(03).
           05  WS-AP-RULE                  PIC X(04).
           05  WS-AP-NAME                  PIC X(60).
       01  WS-AP-PTR                       PIC 9(03).

       01  WS-DATE-ED.
           05  WS-DE-CCYY                  PIC 9(04).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  WS-DE-MM                    PIC 9(02).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  WS-DE-DD                    PIC 9(02).
       01  WS-DATE-X                       PIC 9(08).
       01  WS-DATE-R REDEFINES WS-DATE-X.
           05  WS-DR-CCYY                  PIC 9(04).
           05  WS-DR-MM                    PIC 9(02).
           05  WS-DR-DD                    PIC 9(02).

       01  WS-ACCT-LINE.
           05  WS-AL-ACCT                  PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-AL-PROD                  PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-AL-OWN                   PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-AL-STATUS                PIC X(01).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-AL-REASON                PIC X(02).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-AL-BAL                   PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-AL-JOINT                 PIC X(10).
           05  FILLER                      PIC X(16) VALUE SPACES.

       COPY OFACRVS.
      *    ROW VIEW OF THE 5 ACCOUNT LINES (ACCT01-05 ARE CONTIGUOUS)
       01  OFACRVM-ACCT-VIEW REDEFINES OFACRVMO.
           05  FILLER                      PIC X(250).
           05  AV-ROW OCCURS 5 TIMES.
               10  AV-LEN                  COMP PIC S9(4).
               10  AV-ATTR                 PIC X(01).
               10  AV-COLOR                PIC X(01).
               10  AV-HILIGHT              PIC X(01).
               10  AV-LINE                 PIC X(72).

       COPY CUSTREC.
       COPY ACCTREC.
       COPY ALRTWRK.
       COPY OFACSUP.
       COPY BANKCTL.
       COPY DATEPARM.
       COPY DFHAID.
       COPY DFHBMSCA.

       01  WS-COMMAREA.
           05  CA-CUST-ID                  PIC X(10).
           05  CA-ALRT-KEY                 PIC X(14).
           05  CA-SDN-ENT-NUM              PIC 9(06).
           05  CA-SDN-NAME                 PIC X(60).
           05  CA-SDN-SCORE                PIC 9(03).
           05  CA-VALIDATED                PIC X(01).
           05  CA-DECISION                 PIC X(01).
           05  CA-REASON                   PIC X(60).

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(155).

       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS ASSIGN USERID(WS-USERID) END-EXEC
           PERFORM 1000-GET-BUSINESS-DATE
           IF EIBCALEN = 0
               INITIALIZE WS-COMMAREA
               MOVE LOW-VALUES TO OFACRVMO
               MOVE LOW-VALUES TO WS-CUST-KEY
               PERFORM 2000-NEXT-PENDING
               PERFORM 8000-SEND-MAP-ERASE
           ELSE
               MOVE DFHCOMMAREA(1:LENGTH OF WS-COMMAREA)
                 TO WS-COMMAREA
               PERFORM 1100-RECEIVE-MAP
               EVALUATE EIBAID
                   WHEN DFHPF3
                       EXEC CICS XCTL PROGRAM('CBMENU')
                                 RESP(WS-RESP)
                       END-EXEC
                   WHEN DFHPF12
                       MOVE 'N' TO CA-VALIDATED
                       MOVE CA-CUST-ID TO WS-CUST-KEY
                       PERFORM 2100-LOAD-CUSTOMER
                       MOVE 'DECISION CANCELLED' TO WS-MESSAGE
                   WHEN DFHPF8
                       MOVE CA-CUST-ID TO WS-CUST-KEY
                       PERFORM 2000-NEXT-PENDING
                   WHEN DFHENTER
                       PERFORM 3000-PROCESS-ENTER
                   WHEN DFHPF10
                       PERFORM 4000-COMMIT-DECISION
                   WHEN OTHER
                       MOVE 'INVALID KEY PRESSED' TO WS-MESSAGE
               END-EVALUATE
               PERFORM 8100-SEND-MAP-DATAONLY
           END-IF
           EXEC CICS RETURN TRANSID(WS-TRANID)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
       1000-GET-BUSINESS-DATE.
           EXEC CICS READ FILE('BANKCTL')
                     INTO(BANK-CONTROL-RECORD)
                     RIDFLD(WS-CTL-KEY)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'BANK CONTROL RECORD UNAVAILABLE' TO WS-MESSAGE
               PERFORM 9000-ERROR-EXIT
           END-IF.

       1100-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP(WS-MAPNAME) MAPSET(WS-MAPSET)
                     INTO(OFACRVMI)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO OFACRVMI
           END-IF.

      *----------------------------------------------------------------*
      * NEXT CUSTOMER AFTER WS-CUST-KEY WHOSE OFAC STATUS IS H         *
      *----------------------------------------------------------------*
       2000-NEXT-PENDING.
           MOVE 'N' TO WS-FOUND WS-END-OF-FILE
           EXEC CICS STARTBR FILE('CUSTMST')
                     RIDFLD(WS-CUST-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'NO PENDING OFAC MATCHES' TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-WAS-FOUND OR WS-END-OF-FILE = 'Y'
               EXEC CICS READNEXT FILE('CUSTMST')
                         INTO(CUSTOMER-RECORD)
                         RIDFLD(WS-CUST-KEY)
                         RESP(WS-RESP)
               END-EXEC
               EVALUATE TRUE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                       MOVE 'Y' TO WS-END-OF-FILE
                   WHEN CUST-ID = CA-CUST-ID
                       CONTINUE
                   WHEN CUST-OFAC-POTENTIAL-HIT
                       MOVE 'Y' TO WS-FOUND
               END-EVALUATE
           END-PERFORM
           EXEC CICS ENDBR FILE('CUSTMST') RESP(WS-RESP) END-EXEC
           IF WS-WAS-FOUND
               MOVE CUST-ID TO WS-CUST-KEY
               PERFORM 2100-LOAD-CUSTOMER
           ELSE
               INITIALIZE WS-COMMAREA
               MOVE 'NO FURTHER PENDING OFAC MATCHES' TO WS-MESSAGE
           END-IF.

      *----------------------------------------------------------------*
      * DISPLAY ONE CUSTOMER, ITS MATCH AND ITS ACCOUNTS               *
      *----------------------------------------------------------------*
       2100-LOAD-CUSTOMER.
           EXEC CICS READ FILE('CUSTMST')
                     INTO(CUSTOMER-RECORD)
                     RIDFLD(WS-CUST-KEY)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CUSTOMER NOT FOUND' TO WS-MESSAGE
               MOVE -1 TO CUSTIDL
               EXIT PARAGRAPH
           END-IF
           MOVE CUST-ID           TO CA-CUST-ID CUSTIDO
           STRING CUST-LAST-NAME DELIMITED BY '  '
                  ', '           DELIMITED BY SIZE
                  CUST-FIRST-NAME DELIMITED BY '  '
             INTO CNAMEO
           END-STRING
           MOVE CUST-TYPE             TO CTYPEO
           MOVE CUST-COUNTRY          TO CCTRYO
           MOVE CUST-BIRTH-DATE       TO CDOBO
      *    TAX ID MASKED - LAST 4 ONLY (INFOSEC STD 7.2)
           MOVE '*****'               TO CTAXIDO(1:5)
           MOVE CUST-TAX-ID(6:4)      TO CTAXIDO(6:4)
           MOVE CUST-STATUS           TO CSTATO
           MOVE CUST-OFAC-STATUS      TO COFACO
           MOVE CUST-OFAC-LAST-SCREEN TO CLSCRNO
           MOVE 'N'    TO CA-VALIDATED
           MOVE SPACES TO CA-DECISION CA-REASON
           PERFORM 2200-FIND-MATCH-ALERT
           PERFORM 2300-LIST-ACCOUNTS
           EVALUATE TRUE
               WHEN CUST-OFAC-CONFIRMED-HIT
                   MOVE 'CONFIRMED MATCH - CUSTOMER BLOCKED. REFER TO BS
      -                 'A OFFICER TO UNBLOCK'    TO WS-MESSAGE
               WHEN CUST-OFAC-POTENTIAL-HIT
                   MOVE 'POTENTIAL MATCH - ENTER R (CONFIRM) OR C (CLEAR
      -                 ') AND A REASON'          TO WS-MESSAGE
               WHEN OTHER
                   MOVE 'CUSTOMER HAS NO PENDING OFAC MATCH'
                     TO WS-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LATEST OFAC ALERT FOR THIS CUSTOMER IN THE LAST 45 DAYS        *
      *----------------------------------------------------------------*
       2200-FIND-MATCH-ALERT.
           MOVE SPACES TO WS-ALRT-FOUND-KEY CA-SDN-NAME
           MOVE ZERO   TO CA-SDN-ENT-NUM CA-SDN-SCORE
           INITIALIZE DATE-PARMS
           SET DTU-ADD-DAYS TO TRUE
           MOVE CTL-BUSINESS-DATE      TO DTU-DATE-1
           MOVE WS-ALERT-LOOKBACK-DAYS TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-LOOKBACK-DATE
           MOVE WS-LOOKBACK-DATE TO WS-ALRT-KEY(1:8)
           MOVE ZEROES           TO WS-ALRT-KEY(9:6)
           EXEC CICS STARTBR FILE('ALERTKS')
                     RIDFLD(WS-ALRT-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-END-OF-FILE
           PERFORM UNTIL WS-END-OF-FILE = 'Y'
               EXEC CICS READNEXT FILE('ALERTKS')
                         INTO(ALERT-WORK-RECORD)
                         RIDFLD(WS-ALRT-KEY)
                         RESP(WS-RESP)
               END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                   MOVE 'Y' TO WS-END-OF-FILE
               ELSE
                   IF AWQ-ALRT-TYPE = 'OFAC'
                      AND AWQ-ALRT-CUST-ID = CUST-ID
                      AND AWQ-ALRT-DESCRIPTION(1:4) = 'CUST'
                      AND AWQ-DISP-OPEN
                       MOVE AWQ-KEY TO WS-ALRT-FOUND-KEY
                       PERFORM 2210-PARSE-ALERT
                   END-IF
               END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE('ALERTKS') RESP(WS-RESP) END-EXEC
           MOVE WS-ALRT-FOUND-KEY TO CA-ALRT-KEY.

       2210-PARSE-ALERT.
           MOVE SPACES TO WS-ALERT-PARSE
           MOVE 1 TO WS-AP-PTR
           UNSTRING AWQ-ALRT-DESCRIPTION DELIMITED BY ALL SPACE
               INTO WS-AP-SUBJECT WS-AP-SDN-LIT WS-AP-ENT-NUM
                    WS-AP-SCORE-LIT WS-AP-SCORE WS-AP-RULE
               WITH POINTER WS-AP-PTR
           END-UNSTRING
           IF WS-AP-PTR < 80
               MOVE AWQ-ALRT-DESCRIPTION(WS-AP-PTR:) TO WS-AP-NAME
           END-IF
           IF WS-AP-ENT-NUM IS NUMERIC
               MOVE WS-AP-ENT-NUM TO CA-SDN-ENT-NUM
           END-IF
           IF FUNCTION TRIM(WS-AP-SCORE) IS NUMERIC
               COMPUTE CA-SDN-SCORE =
                       FUNCTION NUMVAL(WS-AP-SCORE)
           END-IF
           MOVE WS-AP-NAME     TO CA-SDN-NAME
           MOVE WS-AP-ENT-NUM  TO SDNENTO
           MOVE WS-AP-NAME     TO SDNNAMEO
           MOVE WS-AP-SCORE    TO SDNSCORO
           MOVE WS-AP-RULE     TO SDNRULEO
           MOVE AWQ-ALRT-RULE-ID TO SDNPGMO.

      *----------------------------------------------------------------*
      * UP TO 5 ACCOUNTS WHERE THE CUSTOMER IS PRIMARY OWNER           *
      *----------------------------------------------------------------*
       2300-LIST-ACCOUNTS.
           PERFORM VARYING WS-ROW FROM 1 BY 1 UNTIL WS-ROW > 5
               MOVE SPACES TO AV-LINE(WS-ROW)
           END-PERFORM
           MOVE CUST-ID TO WS-AIX-KEY
           EXEC CICS STARTBR FILE('ACCTCUS') RIDFLD(WS-AIX-KEY)
                     KEYLENGTH(10) EQUAL RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'NO ACCOUNTS' TO AV-LINE(1)
               EXIT PARAGRAPH
           END-IF
           MOVE 0   TO WS-ROW
           MOVE 'N' TO WS-END-OF-FILE
           PERFORM UNTIL WS-ROW >= 5 OR WS-END-OF-FILE = 'Y'
               EXEC CICS READNEXT FILE('ACCTCUS')
                         INTO(ACCOUNT-RECORD)
                         RIDFLD(WS-AIX-KEY)
                         RESP(WS-RESP)
               END-EXEC
               IF (WS-RESP NOT = DFHRESP(NORMAL)
                  AND WS-RESP NOT = DFHRESP(DUPKEY))
                  OR ACCT-PRIMARY-CUST-ID NOT = CUST-ID
                   MOVE 'Y' TO WS-END-OF-FILE
               ELSE
                   ADD 1 TO WS-ROW
                   MOVE ACCT-NUMBER          TO WS-AL-ACCT
                   MOVE ACCT-PRODUCT-CODE    TO WS-AL-PROD
                   MOVE ACCT-OWNERSHIP-TYPE  TO WS-AL-OWN
                   MOVE ACCT-STATUS          TO WS-AL-STATUS
                   MOVE ACCT-RESTRICT-REASON TO WS-AL-REASON
                   MOVE ACCT-LEDGER-BAL      TO WS-AL-BAL
                   MOVE ACCT-JOINT-CUST-ID   TO WS-AL-JOINT
                   MOVE WS-ACCT-LINE         TO AV-LINE(WS-ROW)
               END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE('ACCTCUS') RESP(WS-RESP) END-EXEC.

      *----------------------------------------------------------------*
      * ENTER: LOAD A KEYED CUSTOMER OR VALIDATE THE DECISION          *
      *----------------------------------------------------------------*
       3000-PROCESS-ENTER.
           IF CUSTIDL > 0 AND CUSTIDI NOT = CA-CUST-ID
               IF CUSTIDI = SPACES
                   MOVE CA-CUST-ID TO WS-CUST-KEY
                   PERFORM 2000-NEXT-PENDING
               ELSE
                   MOVE CUSTIDI TO WS-CUST-KEY
                   PERFORM 2100-LOAD-CUSTOMER
               END-IF
               EXIT PARAGRAPH
           END-IF
           IF CA-CUST-ID = SPACES OR LOW-VALUES
               MOVE 'ENTER A CUSTOMER ID OR PRESS PF8' TO WS-MESSAGE
               MOVE -1 TO CUSTIDL
               EXIT PARAGRAPH
           END-IF
           MOVE CA-CUST-ID TO WS-CUST-KEY
           PERFORM 2100-LOAD-CUSTOMER
           PERFORM 3100-VALIDATE-DECISION.

       3100-VALIDATE-DECISION.
           MOVE 'N' TO CA-VALIDATED
           MOVE 0 TO WS-REASON-LEN
           IF REASONL > 0
               MOVE REASONI TO CA-REASON
               COMPUTE WS-REASON-LEN =
                       FUNCTION LENGTH(FUNCTION TRIM(REASONI))
           END-IF
           IF DECISL > 0
               MOVE DECISI TO CA-DECISION
           END-IF
           EVALUATE TRUE
               WHEN WS-USERID(1:3) NOT = WS-COMPLIANCE-PREFIX
                   MOVE 'ONLY COMPLIANCE (CMP) USERS MAY DISPOSITION OFA
      -                 'C MATCHES'               TO WS-MESSAGE
               WHEN NOT CUST-OFAC-POTENTIAL-HIT
                   MOVE 'CUSTOMER IS NOT PENDING OFAC REVIEW'
                     TO WS-MESSAGE
               WHEN CA-DECISION NOT = 'R' AND NOT = 'C'
                   MOVE 'DECISION MUST BE R (CONFIRM) OR C (CLEAR)'
                     TO WS-MESSAGE
                   MOVE -1 TO DECISL
               WHEN WS-REASON-LEN < WS-MIN-REASON-LEN
                   MOVE 'REASON OF AT LEAST 10 CHARACTERS REQUIRED'
                     TO WS-MESSAGE
                   MOVE -1 TO REASONL
               WHEN CA-DECISION = 'C' AND CA-SDN-ENT-NUM = 0
                   MOVE 'NO OPEN OFAC ALERT FOUND - CANNOT SUPPRESS. ESC
      -                 'ALATE VIA CB60'          TO WS-MESSAGE
               WHEN CA-DECISION = 'R'
                   MOVE 'Y' TO CA-VALIDATED
                   MOVE 'CONFIRM WILL BLOCK CUSTOMER AND FREEZE ALL ACCO
      -                 'UNTS - PRESS PF10'       TO WS-MESSAGE
               WHEN OTHER
                   MOVE 'Y' TO CA-VALIDATED
                   MOVE 'CLEAR AS FALSE POSITIVE (SUPPRESS 365 DAYS) - P
      -                 'RESS PF10'               TO WS-MESSAGE
           END-EVALUATE
           MOVE CA-DECISION TO DECISO
           MOVE CA-REASON   TO REASONO.

      *----------------------------------------------------------------*
      * PF10: APPLY A VALIDATED DECISION                               *
      *----------------------------------------------------------------*
       4000-COMMIT-DECISION.
           IF CA-VALIDATED NOT = 'Y'
               MOVE 'VALIDATE THE DECISION WITH ENTER FIRST'
                 TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           MOVE CA-CUST-ID TO WS-CUST-KEY
           EXEC CICS READ FILE('CUSTMST')
                     INTO(CUSTOMER-RECORD)
                     RIDFLD(WS-CUST-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CUSTOMER NOT FOUND' TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           IF NOT CUST-OFAC-POTENTIAL-HIT
               EXEC CICS UNLOCK FILE('CUSTMST') RESP(WS-RESP) END-EXEC
               MOVE 'STATUS CHANGED BY ANOTHER USER - NO UPDATE'
                 TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           IF CA-DECISION = 'R'
               PERFORM 4100-CONFIRM-MATCH
           ELSE
               PERFORM 4200-CLEAR-MATCH
           END-IF
           PERFORM 4300-DISPOSITION-ALERT
           MOVE 'N' TO CA-VALIDATED
           MOVE CA-CUST-ID TO WS-CUST-KEY
           PERFORM 2300-LIST-ACCOUNTS.

      *    TRUE MATCH - BLOCK CUSTOMER, FREEZE EVERY PRIMARY ACCOUNT
       4100-CONFIRM-MATCH.
           SET CUST-BLOCKED            TO TRUE
           SET CUST-OFAC-CONFIRMED-HIT TO TRUE
           MOVE CTL-BUSINESS-DATE TO CUST-LAST-MAINT-DATE
           MOVE WS-USERID         TO CUST-LAST-MAINT-USER
           EXEC CICS REWRITE FILE('CUSTMST')
                     FROM(CUSTOMER-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'UPDATE FAILED - CUSTMST REWRITE' TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           PERFORM 4110-FREEZE-ACCOUNTS
           INITIALIZE DATE-PARMS
           SET DTU-ADD-BUS-DAYS TO TRUE
           MOVE CTL-BUSINESS-DATE     TO DTU-DATE-1
           MOVE WS-BLOCK-RPT-BUS-DAYS TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-BLOCK-RPT-DUE
           MOVE WS-FROZEN-COUNT TO WS-CNT-ED
           STRING 'BLOCKED. ' WS-CNT-ED ' ACCOUNTS FROZEN. BLOCKED '
                  'PROPERTY REPORT DUE ' WS-BLOCK-RPT-DUE
                  DELIMITED BY SIZE INTO WS-MESSAGE
           END-STRING
           MOVE 'B' TO CSTATO
           MOVE 'R' TO COFACO.

      *    JOINT ACCOUNTS ARE FROZEN ONLY WHEN THIS CUSTOMER IS PRIMARY
      *    (INC18822) - JOINT-OWNER-ONLY ACCOUNTS NEED MANUAL ACTION.
       4110-FREEZE-ACCOUNTS.
           MOVE 0 TO WS-FROZEN-COUNT
           MOVE CUST-ID TO WS-AIX-KEY
           EXEC CICS STARTBR FILE('ACCTCUS') RIDFLD(WS-AIX-KEY)
                     KEYLENGTH(10) EQUAL RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-END-OF-FILE
           PERFORM UNTIL WS-END-OF-FILE = 'Y'
               EXEC CICS READNEXT FILE('ACCTCUS')
                         INTO(ACCOUNT-RECORD)
                         RIDFLD(WS-AIX-KEY)
                         RESP(WS-RESP)
               END-EXEC
               IF (WS-RESP NOT = DFHRESP(NORMAL)
                  AND WS-RESP NOT = DFHRESP(DUPKEY))
                  OR ACCT-PRIMARY-CUST-ID NOT = CUST-ID
                   MOVE 'Y' TO WS-END-OF-FILE
               ELSE
                   IF NOT ACCT-CLOSED AND NOT ACCT-ESCHEATED
                      AND NOT ACCT-CHARGED-OFF
                       MOVE ACCT-NUMBER TO WS-ACCT-KEY
                       PERFORM 4120-FREEZE-ONE
                   END-IF
               END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE('ACCTCUS') RESP(WS-RESP) END-EXEC.

       4120-FREEZE-ONE.
           EXEC CICS READ FILE('ACCTMST')
                     INTO(ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               SET ACCT-FROZEN TO TRUE
               MOVE WS-FREEZE-REASON TO ACCT-RESTRICT-REASON
               EXEC CICS REWRITE FILE('ACCTMST')
                         FROM(ACCOUNT-RECORD)
                         RESP(WS-RESP)
               END-EXEC
               IF WS-RESP = DFHRESP(NORMAL)
                   ADD 1 TO WS-FROZEN-COUNT
               END-IF
           END-IF.

      *    FALSE POSITIVE - STATUS C AND A 365-DAY SUPPRESSION RECORD
       4200-CLEAR-MATCH.
           SET CUST-OFAC-CLEAR TO TRUE
           MOVE CTL-BUSINESS-DATE TO CUST-OFAC-LAST-SCREEN
                                     CUST-LAST-MAINT-DATE
           MOVE WS-USERID         TO CUST-LAST-MAINT-USER
           EXEC CICS REWRITE FILE('CUSTMST')
                     FROM(CUSTOMER-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'UPDATE FAILED - CUSTMST REWRITE' TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           INITIALIZE DATE-PARMS
           SET DTU-ADD-DAYS TO TRUE
           MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
           MOVE WS-SUPPRESS-DAYS  TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-SUPPRESS-UNTIL
           INITIALIZE OFAC-SUPPRESS-RECORD
           MOVE CUST-ID           TO OSP-CUST-ID
           MOVE CA-SDN-ENT-NUM    TO OSP-SDN-ENT-NUM
           MOVE CA-SDN-NAME       TO OSP-SDN-NAME
           MOVE CA-SDN-SCORE      TO OSP-MATCH-SCORE
           MOVE CTL-BUSINESS-DATE TO OSP-CLEARED-DATE
           MOVE WS-USERID         TO OSP-CLEARED-USER
           MOVE WS-SUPPRESS-UNTIL TO OSP-SUPPRESS-UNTIL
           MOVE CA-REASON         TO OSP-REASON
           EXEC CICS WRITE FILE('OFACSUP')
                     FROM(OFAC-SUPPRESS-RECORD)
                     RIDFLD(OSP-KEY)
                     RESP(WS-RESP)
           END-EXEC
      *    SAME PAIR CLEARED BEFORE - EXTEND THE EXISTING SUPPRESSION
           IF WS-RESP = DFHRESP(DUPREC)
               EXEC CICS READ FILE('OFACSUP')
                         INTO(OFAC-SUPPRESS-RECORD)
                         RIDFLD(OSP-KEY)
                         UPDATE
                         RESP(WS-RESP)
               END-EXEC
               MOVE CTL-BUSINESS-DATE TO OSP-CLEARED-DATE
               MOVE WS-USERID         TO OSP-CLEARED-USER
               MOVE WS-SUPPRESS-UNTIL TO OSP-SUPPRESS-UNTIL
               MOVE CA-REASON         TO OSP-REASON
               EXEC CICS REWRITE FILE('OFACSUP')
                         FROM(OFAC-SUPPRESS-RECORD)
                         RESP(WS-RESP)
               END-EXEC
           END-IF
           STRING 'CLEARED AS FALSE POSITIVE - SUPPRESSED UNTIL '
                  WS-SUPPRESS-UNTIL
                  DELIMITED BY SIZE INTO WS-MESSAGE
           END-STRING
           MOVE 'C' TO COFACO.

      *    CLOSE THE ALERT THAT RAISED THE MATCH
       4300-DISPOSITION-ALERT.
           IF CA-ALRT-KEY = SPACES
               EXIT PARAGRAPH
           END-IF
           EXEC CICS READ FILE('ALERTKS')
                     INTO(ALERT-WORK-RECORD)
                     RIDFLD(CA-ALRT-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXIT PARAGRAPH
           END-IF
           IF CA-DECISION = 'R'
               SET AWQ-DISP-SAR-FILED TO TRUE
               MOVE 'Y' TO AWQ-SUSPECT-KNOWN
           ELSE
               SET AWQ-DISP-CLOSED TO TRUE
           END-IF
           MOVE WS-USERID         TO AWQ-DISP-USER
           MOVE CTL-BUSINESS-DATE TO AWQ-DISP-DATE
           MOVE CA-REASON         TO AWQ-NOTES
           MOVE 'C'               TO AWQ-ALRT-STATUS
           EXEC CICS REWRITE FILE('ALERTKS')
                     FROM(ALERT-WORK-RECORD)
                     RESP(WS-RESP)
           END-EXEC.

      *----------------------------------------------------------------*
       8000-SEND-MAP-ERASE.
           PERFORM 8200-FILL-HEADER
           EXEC CICS SEND MAP(WS-MAPNAME) MAPSET(WS-MAPSET)
                     FROM(OFACRVMO)
                     ERASE
                     RESP(WS-RESP)
           END-EXEC.

       8100-SEND-MAP-DATAONLY.
           PERFORM 8200-FILL-HEADER
           EXEC CICS SEND MAP(WS-MAPNAME) MAPSET(WS-MAPSET)
                     FROM(OFACRVMO)
                     DATAONLY
                     CURSOR
                     RESP(WS-RESP)
           END-EXEC.

       8200-FILL-HEADER.
           MOVE CTL-BUSINESS-DATE TO WS-DATE-X
           MOVE WS-DR-CCYY TO WS-DE-CCYY
           MOVE WS-DR-MM   TO WS-DE-MM
           MOVE WS-DR-DD   TO WS-DE-DD
           MOVE WS-DATE-ED TO CURDATEO
           MOVE WS-USERID  TO USERIDO
           IF CA-VALIDATED = 'Y'
               MOVE DFHRED TO MSGC
           END-IF
           MOVE WS-MESSAGE TO MSGO.

       9000-ERROR-EXIT.
           EXEC CICS SEND TEXT FROM(WS-MESSAGE)
                     LENGTH(LENGTH OF WS-MESSAGE)
                     ERASE
           END-EXEC
           EXEC CICS RETURN END-EXEC.
