      *================================================================*
      * PROGRAM  : ALRTWORK                                            *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS    : CB60                                                *
      * MAPSET   : ALRTWKS  (MAP ALRTWKM)                              *
      * PURPOSE  : BSA/AML ALERT WORK QUEUE. BROWSES THE ALERTKS KSDS  *
      *            (ONLINE COPY OF ALERTS), LETS AN ANALYST SELECT AN  *
      *            ALERT AND RECORD A DISPOSITION:                     *
      *              C - CLOSED, NO ACTION (NOTES REQUIRED)            *
      *              E - ESCALATED TO THE BSA OFFICER                  *
      *              S - SAR FILED (BSA E-FILING ID REQUIRED)          *
      *            ONLY COMPLIANCE USERS (USERID CMP*) MAY CLOSE AN    *
      *            ALERT OR RECORD A SAR. ANY USER MAY ESCALATE.       *
      *            SAR FILING DEADLINE (31 CFR 1020.320(B)(3)):        *
      *              30 CALENDAR DAYS AFTER INITIAL DETECTION; 60 DAYS *
      *              IF NO SUSPECT HAS BEEN IDENTIFIED.                *
      *            OPEN ALERTS OLDER THAN 30 DAYS ARE HIGHLIGHTED.     *
      * FILES    : ALERTKS (READ/BROWSE/UPDATE), BANKCTL (READ)        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2013-05-13 DPS  ORIGINAL                                       *
      * 2014-01-27 DPS  INC24410 BACKWARD PAGING                       *
      * 2017-02-20 AKW  CHG1544 SAR DUE DATE / SUSPECT-KNOWN FLAG,     *
      *                 AGE HIGHLIGHT                                  *
      * 2021-08-09 AKW  CHG2032 CLOSE / SAR RESTRICTED TO CMP* USERS   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALRTWORK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-NAME                 PIC X(08) VALUE 'ALRTWORK'.
       01  WS-TRANID                       PIC X(04) VALUE 'CB60'.
       01  WS-MAPSET                       PIC X(07) VALUE 'ALRTWKS'.
       01  WS-MAPNAME                      PIC X(07) VALUE 'ALRTWKM'.

      *----------------------------------------------------------------*
      * BUSINESS RULE CONSTANTS                                        *
      *----------------------------------------------------------------*
       01  WS-RULES.
      *    SAR DUE 30 DAYS FROM DETECTION, 60 WHEN NO SUSPECT KNOWN
           05  WS-SAR-DAYS                 PIC S9(05) VALUE 30.
           05  WS-SAR-DAYS-NO-SUSPECT      PIC S9(05) VALUE 60.
      *    OPEN ALERTS OLDER THAN THIS ARE HIGHLIGHTED
           05  WS-AGE-HIGHLIGHT-DAYS       PIC S9(05) VALUE 30.
           05  WS-COMPLIANCE-PREFIX        PIC X(03) VALUE 'CMP'.
           05  WS-ESCALATION-QUEUE         PIC X(08) VALUE 'BSAOFFCR'.

       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-RESP2                        PIC S9(08) COMP.
       01  WS-USERID                       PIC X(08).
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.
       01  WS-BROWSE-KEY                   PIC X(14).
       01  WS-ROW                          PIC 9(02) COMP.
       01  WS-IX                           PIC 9(02) COMP.
       01  WS-AGE                          PIC S9(07).
       01  WS-SEL-COUNT                    PIC 9(02).
       01  WS-SEL-ROW                      PIC 9(02).
       01  WS-END-OF-FILE                  PIC X(01).
       01  WS-MESSAGE                      PIC X(78).
       01  WS-SAR-DUE                      PIC 9(08).
       01  WS-AMT-ED                       PIC ZZZ,ZZZ,ZZ9.99.
       01  WS-AGE-ED                       PIC ZZZ9.
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

       01  WS-ROW-LINE.
           05  WS-RL-DATE                  PIC 9(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RL-TYPE                  PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-RL-SEV                   PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-RL-CUST                  PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RL-ACCT                  PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RL-AMOUNT                PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RL-AGE                   PIC ZZZ9.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RL-SAR-DUE               PIC X(08).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RL-DISP                  PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.

       COPY ALRTWKS.
      *    ROW VIEW OF THE 10 LIST LINES (SELNN / LINNN ARE CONTIGUOUS)
       01  ALRTWKM-ROW-VIEW REDEFINES ALRTWKMI.
           05  FILLER                      PIC X(68).
           05  RV-ROW OCCURS 10 TIMES.
               10  RV-SEL-L                COMP PIC S9(4).
               10  RV-SEL-F                PIC X.
               10  RV-SEL-C                PIC X.
               10  RV-SEL-H                PIC X.
               10  RV-SEL-I                PIC X.
               10  RV-LIN-L                COMP PIC S9(4).
               10  RV-LIN-F                PIC X.
               10  RV-LIN-C                PIC X.
               10  RV-LIN-H                PIC X.
               10  RV-LIN-O                PIC X(75).

       COPY ALRTWRK.
       COPY BANKCTL.
       COPY DATEPARM.
       COPY DFHAID.
       COPY DFHBMSCA.

       01  WS-COMMAREA.
           05  CA-FIRST-KEY                PIC X(14).
           05  CA-LAST-KEY                 PIC X(14).
           05  CA-SEL-KEY                  PIC X(14).
           05  CA-FILTER-TYPE              PIC X(04).
           05  CA-FILTER-OPEN              PIC X(01).
           05  CA-FILTER-DATE              PIC 9(08).
           05  CA-PAGE-NO                  PIC 9(03).
           05  CA-PAGE-START OCCURS 50 TIMES PIC X(14).
           05  CA-ROW-KEY    OCCURS 10 TIMES PIC X(14).

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(1000).

       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS ASSIGN USERID(WS-USERID) END-EXEC
           PERFORM 1000-GET-BUSINESS-DATE
           IF EIBCALEN = 0
               INITIALIZE WS-COMMAREA
               MOVE 'Y'    TO CA-FILTER-OPEN
               MOVE LOW-VALUES TO CA-FIRST-KEY
               MOVE 1      TO CA-PAGE-NO
               MOVE LOW-VALUES TO CA-PAGE-START(1)
               MOVE LOW-VALUES TO ALRTWKMO
               PERFORM 2000-BUILD-PAGE
               MOVE 'ENTER S NEXT TO AN ALERT TO WORK IT' TO WS-MESSAGE
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
                       MOVE SPACES TO CA-SEL-KEY
                       MOVE CA-PAGE-START(CA-PAGE-NO) TO CA-FIRST-KEY
                       PERFORM 2000-BUILD-PAGE
                       MOVE 'SELECTION CANCELLED' TO WS-MESSAGE
                   WHEN DFHPF8
                       IF CA-PAGE-NO < 50
                           ADD 1 TO CA-PAGE-NO
                       END-IF
                       MOVE CA-LAST-KEY TO CA-PAGE-START(CA-PAGE-NO)
                                           CA-FIRST-KEY
                       PERFORM 2000-BUILD-PAGE
                   WHEN DFHPF7
                       IF CA-PAGE-NO > 1
                           SUBTRACT 1 FROM CA-PAGE-NO
                       END-IF
                       MOVE CA-PAGE-START(CA-PAGE-NO) TO CA-FIRST-KEY
                       PERFORM 2000-BUILD-PAGE
                   WHEN DFHENTER
                       PERFORM 3000-PROCESS-ENTER
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
                     INTO(ALRTWKMI)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO ALRTWKMI
           END-IF
           IF FTYPEL > 0
               MOVE FTYPEI TO CA-FILTER-TYPE
           END-IF
           IF FOPENL > 0
               MOVE FOPENI TO CA-FILTER-OPEN
           END-IF
           IF FDATEL > 0 AND FDATEI IS NUMERIC
               MOVE FDATEI TO CA-FILTER-DATE
           END-IF.

      *----------------------------------------------------------------*
      * BUILD ONE PAGE OF UP TO 10 ALERTS STARTING AT CA-FIRST-KEY     *
      *----------------------------------------------------------------*
       2000-BUILD-PAGE.
           PERFORM VARYING WS-ROW FROM 1 BY 1 UNTIL WS-ROW > 10
               MOVE SPACES TO RV-LIN-O(WS-ROW) CA-ROW-KEY(WS-ROW)
               MOVE SPACE  TO RV-SEL-I(WS-ROW)
               MOVE DFHGREEN TO RV-LIN-C(WS-ROW)
               MOVE DFHBMASK TO RV-LIN-F(WS-ROW)
           END-PERFORM
           MOVE CA-FIRST-KEY TO WS-BROWSE-KEY
           IF CA-FILTER-DATE > 0 AND CA-PAGE-NO = 1
               MOVE CA-FILTER-DATE TO WS-BROWSE-KEY(1:8)
               MOVE ZEROES         TO WS-BROWSE-KEY(9:6)
           END-IF
           EXEC CICS STARTBR FILE('ALERTKS')
                     RIDFLD(WS-BROWSE-KEY)
                     GTEQ
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'NO ALERTS FOUND' TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           MOVE 0   TO WS-ROW
           MOVE 'N' TO WS-END-OF-FILE
           PERFORM UNTIL WS-ROW >= 10 OR WS-END-OF-FILE = 'Y'
               EXEC CICS READNEXT FILE('ALERTKS')
                         INTO(ALERT-WORK-RECORD)
                         RIDFLD(WS-BROWSE-KEY)
                         RESP(WS-RESP)
               END-EXEC
               EVALUATE TRUE
                   WHEN WS-RESP = DFHRESP(ENDFILE)
                       MOVE 'Y' TO WS-END-OF-FILE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                       MOVE 'Y' TO WS-END-OF-FILE
                       MOVE 'ERROR BROWSING ALERTKS' TO WS-MESSAGE
                   WHEN OTHER
                       PERFORM 2100-APPLY-FILTER
               END-EVALUATE
           END-PERFORM
           MOVE WS-BROWSE-KEY TO CA-LAST-KEY
           EXEC CICS ENDBR FILE('ALERTKS') RESP(WS-RESP) END-EXEC.

       2100-APPLY-FILTER.
           IF CA-FILTER-TYPE NOT = SPACES
              AND CA-FILTER-TYPE NOT = LOW-VALUES
              AND AWQ-ALRT-TYPE NOT = CA-FILTER-TYPE
               EXIT PARAGRAPH
           END-IF
           IF CA-FILTER-OPEN = 'Y'
              AND (AWQ-DISP-CLOSED OR AWQ-DISP-SAR-FILED)
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-ROW
           MOVE AWQ-KEY TO CA-ROW-KEY(WS-ROW)
           PERFORM 2200-FORMAT-ROW.

      *----------------------------------------------------------------*
      * ROW: AGE IN DAYS SINCE DETECTION, SAR DUE DATE, HIGHLIGHT OPEN *
      * ALERTS OLDER THAN 30 DAYS.                                     *
      *----------------------------------------------------------------*
       2200-FORMAT-ROW.
           PERFORM 7000-COMPUTE-AGE
           PERFORM 7100-COMPUTE-SAR-DUE
           MOVE AWQ-ALRT-DATE        TO WS-RL-DATE
           MOVE AWQ-ALRT-TYPE        TO WS-RL-TYPE
           MOVE AWQ-ALRT-SEVERITY    TO WS-RL-SEV
           MOVE AWQ-ALRT-CUST-ID     TO WS-RL-CUST
           MOVE AWQ-ALRT-ACCT-NUMBER TO WS-RL-ACCT
           MOVE AWQ-ALRT-AMOUNT      TO WS-RL-AMOUNT
           MOVE WS-AGE               TO WS-RL-AGE
           MOVE WS-SAR-DUE           TO WS-RL-SAR-DUE
           IF AWQ-DISP-OPEN
               MOVE '-' TO WS-RL-DISP
           ELSE
               MOVE AWQ-DISP-CODE TO WS-RL-DISP
           END-IF
           MOVE WS-ROW-LINE TO RV-LIN-O(WS-ROW)
           IF WS-AGE > WS-AGE-HIGHLIGHT-DAYS
              AND (AWQ-DISP-OPEN OR AWQ-DISP-ESCALATED)
               MOVE DFHBMASB TO RV-LIN-F(WS-ROW)
               MOVE DFHRED   TO RV-LIN-C(WS-ROW)
               MOVE DFHREVRS TO RV-LIN-H(WS-ROW)
           END-IF.

      *----------------------------------------------------------------*
      * ENTER: EITHER SELECT A ROW OR APPLY A DISPOSITION TO THE       *
      * CURRENTLY SELECTED ALERT.                                      *
      *----------------------------------------------------------------*
       3000-PROCESS-ENTER.
           MOVE 0 TO WS-SEL-COUNT WS-SEL-ROW
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 10
               IF RV-SEL-L(WS-IX) > 0
                  AND (RV-SEL-I(WS-IX) = 'S' OR 's')
                   ADD 1 TO WS-SEL-COUNT
                   MOVE WS-IX TO WS-SEL-ROW
               END-IF
           END-PERFORM
           EVALUATE TRUE
               WHEN WS-SEL-COUNT > 1
                   MOVE 'SELECT ONLY ONE ALERT AT A TIME' TO WS-MESSAGE
               WHEN WS-SEL-COUNT = 1
                   IF CA-ROW-KEY(WS-SEL-ROW) = SPACES
                       MOVE 'NO ALERT ON THAT LINE' TO WS-MESSAGE
                   ELSE
                       MOVE CA-ROW-KEY(WS-SEL-ROW) TO CA-SEL-KEY
                       PERFORM 3100-SHOW-DETAIL
                   END-IF
               WHEN CA-SEL-KEY NOT = SPACES AND DISPL > 0
                   PERFORM 4000-APPLY-DISPOSITION
               WHEN OTHER
                   MOVE CA-PAGE-START(CA-PAGE-NO) TO CA-FIRST-KEY
                   PERFORM 2000-BUILD-PAGE
                   MOVE 'ENTER S NEXT TO AN ALERT TO WORK IT'
                     TO WS-MESSAGE
           END-EVALUATE.

       3100-SHOW-DETAIL.
           EXEC CICS READ FILE('ALERTKS')
                     INTO(ALERT-WORK-RECORD)
                     RIDFLD(CA-SEL-KEY)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ALERT NO LONGER ON FILE' TO WS-MESSAGE
               MOVE SPACES TO CA-SEL-KEY
               EXIT PARAGRAPH
           END-IF
           PERFORM 7000-COMPUTE-AGE
           PERFORM 7100-COMPUTE-SAR-DUE
           MOVE CA-SEL-KEY           TO DALRTIDO
           MOVE AWQ-ALRT-DESCRIPTION TO DDESCO
           MOVE AWQ-ALRT-DATE        TO DDETECTO
           MOVE WS-SAR-DUE           TO DSARDUEO
           MOVE WS-AGE               TO WS-AGE-ED
           MOVE WS-AGE-ED            TO DAGEO
           EVALUATE TRUE
               WHEN AWQ-DISP-OPEN
                   MOVE 'OPEN'                 TO DSTATO
               WHEN AWQ-DISP-CLOSED
                   MOVE 'CLOSED - NO ACTION'   TO DSTATO
               WHEN AWQ-DISP-ESCALATED
                   MOVE 'ESCALATED'            TO DSTATO
               WHEN AWQ-DISP-SAR-FILED
                   MOVE 'SAR FILED'            TO DSTATO
           END-EVALUATE
           MOVE AWQ-DISP-CODE        TO DISPO
           MOVE AWQ-SUSPECT-KNOWN    TO SUSPO
           MOVE AWQ-SAR-BSA-ID       TO SARIDO
           MOVE AWQ-NOTES            TO NOTESO
           IF WS-AGE > WS-AGE-HIGHLIGHT-DAYS AND AWQ-DISP-OPEN
               MOVE DFHRED TO DAGEC DSARDUEC
               MOVE 'ALERT OVER 30 DAYS OLD - SAR DECISION OVERDUE?'
                 TO WS-MESSAGE
           ELSE
               MOVE 'ENTER DISPOSITION AND PRESS ENTER' TO WS-MESSAGE
           END-IF.

      *----------------------------------------------------------------*
      * DISPOSITION RULES                                              *
      *----------------------------------------------------------------*
       4000-APPLY-DISPOSITION.
           EXEC CICS READ FILE('ALERTKS')
                     INTO(ALERT-WORK-RECORD)
                     RIDFLD(CA-SEL-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ALERT NO LONGER ON FILE' TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN AWQ-DISP-CLOSED OR AWQ-DISP-SAR-FILED
                   MOVE 'ALERT ALREADY DISPOSITIONED - NO CHANGE'
                     TO WS-MESSAGE
               WHEN DISPI NOT = 'C' AND NOT = 'E' AND NOT = 'S'
                   MOVE 'DISPOSITION MUST BE C, E OR S' TO WS-MESSAGE
                   MOVE -1 TO DISPL
               WHEN (DISPI = 'C' OR 'S')
                AND WS-USERID(1:3) NOT = WS-COMPLIANCE-PREFIX
                   MOVE 'ONLY COMPLIANCE (CMP) USERS MAY CLOSE OR FILE'
                     TO WS-MESSAGE
               WHEN DISPI = 'C' AND (NOTESL = 0 OR NOTESI = SPACES)
                   MOVE 'NOTES REQUIRED TO CLOSE WITHOUT ACTION'
                     TO WS-MESSAGE
                   MOVE -1 TO NOTESL
               WHEN DISPI = 'S' AND (SARIDL = 0 OR SARIDI = SPACES)
                   MOVE 'BSA E-FILING SAR ID REQUIRED' TO WS-MESSAGE
                   MOVE -1 TO SARIDL
               WHEN SUSPL > 0 AND SUSPI NOT = 'Y' AND NOT = 'N'
                   MOVE 'SUSPECT KNOWN MUST BE Y OR N' TO WS-MESSAGE
                   MOVE -1 TO SUSPL
               WHEN OTHER
                   PERFORM 4100-UPDATE-ALERT
                   EXIT PARAGRAPH
           END-EVALUATE
           EXEC CICS UNLOCK FILE('ALERTKS') RESP(WS-RESP) END-EXEC.

       4100-UPDATE-ALERT.
           MOVE DISPI             TO AWQ-DISP-CODE
           MOVE WS-USERID         TO AWQ-DISP-USER
           MOVE CTL-BUSINESS-DATE TO AWQ-DISP-DATE
           IF SUSPL > 0
               MOVE SUSPI TO AWQ-SUSPECT-KNOWN
           END-IF
           IF NOTESL > 0
               MOVE NOTESI TO AWQ-NOTES
           END-IF
           IF AWQ-DISP-SAR-FILED
               MOVE SARIDI TO AWQ-SAR-BSA-ID
           END-IF
           IF AWQ-DISP-ESCALATED
               MOVE WS-ESCALATION-QUEUE TO AWQ-ESCALATED-TO
           END-IF
           PERFORM 7100-COMPUTE-SAR-DUE
           MOVE WS-SAR-DUE TO AWQ-SAR-DUE-DATE
           MOVE 'C' TO AWQ-ALRT-STATUS
           IF AWQ-DISP-ESCALATED
               MOVE 'O' TO AWQ-ALRT-STATUS
           END-IF
           EXEC CICS REWRITE FILE('ALERTKS')
                     FROM(ALERT-WORK-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               STRING 'ALERT ' CA-SEL-KEY ' UPDATED - DISPOSITION '
                      AWQ-DISP-CODE ' SAR DUE ' WS-SAR-DUE
                      DELIMITED BY SIZE INTO WS-MESSAGE
               END-STRING
               MOVE SPACES TO CA-SEL-KEY
               MOVE CA-PAGE-START(CA-PAGE-NO) TO CA-FIRST-KEY
               PERFORM 2000-BUILD-PAGE
           ELSE
               MOVE 'UPDATE FAILED - ALERTKS REWRITE' TO WS-MESSAGE
           END-IF.

      *----------------------------------------------------------------*
       7000-COMPUTE-AGE.
           INITIALIZE DATE-PARMS
           SET DTU-DAYS-BETWEEN TO TRUE
           MOVE AWQ-ALRT-DATE     TO DTU-DATE-1
           MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-NUMBER TO WS-AGE.

      *    30 CALENDAR DAYS FROM DETECTION; 60 IF NO SUSPECT IDENTIFIED
       7100-COMPUTE-SAR-DUE.
           INITIALIZE DATE-PARMS
           SET DTU-ADD-DAYS TO TRUE
           MOVE AWQ-ALRT-DATE TO DTU-DATE-1
           IF AWQ-NO-SUSPECT
               MOVE WS-SAR-DAYS-NO-SUSPECT TO DTU-NUMBER
           ELSE
               MOVE WS-SAR-DAYS TO DTU-NUMBER
           END-IF
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-SAR-DUE.

      *----------------------------------------------------------------*
       8000-SEND-MAP-ERASE.
           PERFORM 8200-FILL-HEADER
           EXEC CICS SEND MAP(WS-MAPNAME) MAPSET(WS-MAPSET)
                     FROM(ALRTWKMO)
                     ERASE
                     RESP(WS-RESP)
           END-EXEC.

       8100-SEND-MAP-DATAONLY.
           PERFORM 8200-FILL-HEADER
           EXEC CICS SEND MAP(WS-MAPNAME) MAPSET(WS-MAPSET)
                     FROM(ALRTWKMO)
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
           MOVE CA-FILTER-TYPE TO FTYPEO
           MOVE CA-FILTER-OPEN TO FOPENO
           MOVE WS-MESSAGE TO MSGO.

       9000-ERROR-EXIT.
           EXEC CICS SEND TEXT FROM(WS-MESSAGE)
                     LENGTH(LENGTH OF WS-MESSAGE)
                     ERASE
           END-EXEC
           EXEC CICS RETURN END-EXEC.
