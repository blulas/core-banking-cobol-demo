      *================================================================*
      * PROGRAM  : CUSTINQ                                             *
      * TYPE     : CICS ONLINE - PSEUDO-CONVERSATIONAL                 *
      * TRANS    : CB01                                                *
      * MAPSET   : CUINQS  MAP CUINQM                                  *
      * PURPOSE  : CUSTOMER INQUIRY. SHOWS PROFILE, KYC, RISK, OFAC    *
      *            AND RELATIONSHIP BALANCE, AND BROWSES THE           *
      *            CUSTOMER'S ACCOUNTS THROUGH THE ACCTMST AIX PATH    *
      *            'ACCTCUS' (KEY = ACCT-PRIMARY-CUST-ID), 7 PER PAGE. *
      *            WARNS WHEN AN ACCOUNT IS ABOUT TO GO DORMANT SO THE *
      *            BANKER CAN PROMPT CUSTOMER ACTIVITY.                *
      * FILES    : CUSTMST, ACCTCUS (AIX PATH OVER ACCTMST), BANKCTL   *
      * PF KEYS  : ENTER INQUIRE, PF7 PREV PAGE, PF8 NEXT PAGE,        *
      *            PF3 MENU, PF12 CANCEL                               *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-03-20 RJT  ORIGINAL                                       *
      * 2002-04-01 BHS  KYC STATUS / RISK RATING DISPLAY               *
      * 2006-08-30 BHS  PEP AND OFAC FIELDS                            *
      * 2009-05-11 DPS  DORMANT-SOON WARNING ON ACCOUNT LIST (CHG0244) *
      * 2015-08-03 DPS  TAX ID MASKED TO LAST 4 (PRIVACY AUDIT)        *
      * 2018-05-11 AKW  KYC TIER / REVIEW DATE, GRACE DAYS REMAINING   *
      * 2020-11-16 JLM  RELATIONSHIP BALANCE (RELBAL) DISPLAY          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTINQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANSID                      PIC X(04) VALUE 'CB01'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-MESSAGE                      PIC X(79).
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.
       01  WS-AIX-KEY                      PIC X(10).
       01  WS-PAGE-SIZE                    PIC 9(02) VALUE 07.
       01  WS-SKIP                         PIC 9(05).
       01  WS-READ-CNT                     PIC 9(05).
       01  WS-LINE-CNT                     PIC 9(02).
       01  WS-BROWSE-FLAG                  PIC X(01).
           88  WS-BROWSE-DONE                        VALUE 'Y'.
       01  WS-ABSTIME                      PIC S9(15) COMP-3.
       01  WS-DATE-OUT                     PIC X(10).

      *--- DORMANCY WARNING ------------------------------------------*
      *    MONTHS OF INACTIVITY BEFORE AN ACCOUNT IS CLASSIFIED        *
      *    DORMANT, BY CATEGORY. WARN 30 DAYS AHEAD.                   *
       01  WS-DORM-VALUES.
           05  FILLER PIC X(05) VALUE 'DD012'.
           05  FILLER PIC X(05) VALUE 'SV024'.
           05  FILLER PIC X(05) VALUE 'MM024'.
       01  WS-DORM-TABLE REDEFINES WS-DORM-VALUES.
           05  WS-DORM-ENTRY OCCURS 3 TIMES INDEXED BY WS-DRM-IDX.
               10  WS-DORM-CAT             PIC X(02).
               10  WS-DORM-MONTHS          PIC 9(03).
       01  WS-DORM-WARN-DAYS               PIC S9(05) VALUE +30.
       01  WS-DORMANT-ON                   PIC 9(08).
       01  WS-WARN-FROM                    PIC 9(08).
       01  WS-NOTE                         PIC X(18).
       01  WS-KYC-GRACE-DAYS               PIC S9(05) VALUE +90.
       01  WS-DAYS                         PIC S9(07).

       01  WS-EDIT-BAL                     PIC Z,ZZZ,ZZZ,ZZ9.99-.
       01  WS-EDIT-RELBAL                  PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-EDIT-DATE.
           05  WS-ED-CCYY                  PIC 9(04).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  WS-ED-MM                    PIC 9(02).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  WS-ED-DD                    PIC 9(02).
       01  WS-DATE-IN                      PIC 9(08).
       01  WS-DATE-IN-R REDEFINES WS-DATE-IN.
           05  WS-DI-CCYY                  PIC 9(04).
           05  WS-DI-MM                    PIC 9(02).
           05  WS-DI-DD                    PIC 9(02).
       01  WS-ACCT-LINE.
           05  WS-AL-ACCT                  PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-AL-PROD                  PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-AL-STATUS                PIC X(02).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-AL-OWN                   PIC X(04).
           05  WS-AL-BAL                   PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-AL-LASTACT               PIC X(10).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  WS-AL-NOTE                  PIC X(18).

       01  WS-COMMAREA.
           05  CA-CUST-ID                  PIC X(10).
           05  CA-PAGE                     PIC 9(03).
           05  CA-ACCT-TOTAL               PIC 9(05).

       COPY CUSTREC.
       COPY ACCTREC.
       COPY BANKCTL.
       COPY DATEPARM.
       COPY CUINQS.
       COPY DFHAID.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(18).

       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS READ FILE('BANKCTL') INTO(BANK-CONTROL-RECORD)
                     RIDFLD(WS-CTL-KEY) RESP(WS-RESP)
           END-EXEC
           IF EIBCALEN = 0
               INITIALIZE WS-COMMAREA
               MOVE LOW-VALUES TO CUINQMO
               MOVE 'ENTER CUSTOMER ID' TO WS-MESSAGE
               PERFORM 8000-SEND-MAP-ERASE
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               EVALUATE EIBAID
                   WHEN DFHPF3
                       EXEC CICS XCTL PROGRAM('CBMENU') END-EXEC
                   WHEN DFHPF12
                   WHEN DFHCLEAR
                       INITIALIZE WS-COMMAREA
                       MOVE LOW-VALUES TO CUINQMO
                       MOVE 'ENTER CUSTOMER ID' TO WS-MESSAGE
                       PERFORM 8000-SEND-MAP-ERASE
                   WHEN DFHENTER
                       PERFORM 1000-RECEIVE-INPUT
                       MOVE 1 TO CA-PAGE
                       PERFORM 2000-INQUIRE
                   WHEN DFHPF8
                       ADD 1 TO CA-PAGE
                       PERFORM 2000-INQUIRE
                   WHEN DFHPF7
                       IF CA-PAGE > 1
                           SUBTRACT 1 FROM CA-PAGE
                       END-IF
                       PERFORM 2000-INQUIRE
                   WHEN OTHER
                       MOVE 'INVALID KEY' TO WS-MESSAGE
                       PERFORM 8000-SEND-MAP-ERASE
               END-EVALUATE
           END-IF
           EXEC CICS RETURN TRANSID(WS-TRANSID)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
       1000-RECEIVE-INPUT.
           EXEC CICS RECEIVE MAP('CUINQM') MAPSET('CUINQS')
                     INTO(CUINQMI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL) AND CUSTIDL > 0
               MOVE CUSTIDI TO CA-CUST-ID
           END-IF.

      *----------------------------------------------------------------*
       2000-INQUIRE.
           MOVE LOW-VALUES TO CUINQMO
           IF CA-CUST-ID = SPACES OR LOW-VALUES
               MOVE 'CUSTOMER ID IS REQUIRED' TO WS-MESSAGE
               PERFORM 8000-SEND-MAP-ERASE
               EXIT PARAGRAPH
           END-IF
           MOVE CA-CUST-ID TO CUST-ID
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                     RIDFLD(CUST-ID) RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM 2100-FORMAT-CUSTOMER
                   PERFORM 2200-BROWSE-ACCOUNTS
                   PERFORM 8000-SEND-MAP-ERASE
               WHEN DFHRESP(NOTFND)
                   MOVE 'CUSTOMER NOT FOUND' TO WS-MESSAGE
                   PERFORM 8000-SEND-MAP-ERASE
               WHEN OTHER
                   MOVE 'CUSTMST READ ERROR - CONTACT HELP DESK'
                     TO WS-MESSAGE
                   PERFORM 8000-SEND-MAP-ERASE
           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-FORMAT-CUSTOMER.
           MOVE CUST-ID TO CUSTIDO
           IF CUST-FIRST-NAME = SPACES
               MOVE CUST-LAST-NAME TO CNAMEO
           ELSE
               STRING FUNCTION TRIM(CUST-FIRST-NAME) ' '
                      CUST-MIDDLE-INIT ' '
                      FUNCTION TRIM(CUST-LAST-NAME)
                      DELIMITED BY SIZE INTO CNAMEO
           END-IF
           EVALUATE TRUE
               WHEN CUST-INDIVIDUAL MOVE 'INDIVIDUAL' TO CTYPEO
               WHEN CUST-CORPORATE  MOVE 'CORPORATE'  TO CTYPEO
               WHEN CUST-TRUST      MOVE 'TRUST'      TO CTYPEO
               WHEN OTHER           MOVE CUST-TYPE    TO CTYPEO
           END-EVALUATE
           EVALUATE TRUE
               WHEN CUST-ACTIVE   MOVE 'ACTIVE'   TO CSTATO
               WHEN CUST-INACTIVE MOVE 'INACTIVE' TO CSTATO
               WHEN CUST-DECEASED MOVE 'DECEASED' TO CSTATO
               WHEN CUST-BLOCKED  MOVE 'BLOCKED'  TO CSTATO
               WHEN CUST-PROSPECT MOVE 'PROSPECT' TO CSTATO
               WHEN OTHER
                   STRING 'CODE ' CUST-STATUS DELIMITED BY SIZE
                          INTO CSTATO
           END-EVALUATE
           EVALUATE TRUE
               WHEN CUST-SEG-RETAIL         MOVE 'RETAIL'    TO CSEGO
               WHEN CUST-SEG-MASS-AFFLUENT  MOVE 'MASS AFFL' TO CSEGO
               WHEN CUST-SEG-PRIVATE-WEALTH MOVE 'PRIV WLTH' TO CSEGO
               WHEN CUST-SEG-CORPORATE      MOVE 'CORP BANK' TO CSEGO
               WHEN CUST-SEG-INSTITUTIONAL  MOVE 'INSTITUTN' TO CSEGO
           END-EVALUATE
           MOVE CUST-SINCE-DATE TO WS-DATE-IN
           PERFORM 8100-EDIT-DATE
           MOVE WS-EDIT-DATE TO CSINCEO
           STRING FUNCTION TRIM(CUST-ADDR-LINE1) ', '
                  FUNCTION TRIM(CUST-CITY) ' ' CUST-STATE ' '
                  FUNCTION TRIM(CUST-ZIP) ' ' CUST-COUNTRY
                  DELIMITED BY SIZE INTO CADDRO
           MOVE CUST-PHONE TO CPHONEO
           MOVE CUST-EMAIL TO CEMAILO
           MOVE CUST-BIRTH-DATE TO WS-DATE-IN
           PERFORM 8100-EDIT-DATE
           MOVE WS-EDIT-DATE TO CDOBO
      *    2015-08 DPS PRIVACY AUDIT - ONLY LAST 4 OF TAX ID DISPLAYED
           STRING '***-**-' CUST-TAX-ID(6:4) DELIMITED BY SIZE
                  INTO CTAXIDO
           MOVE CUST-RELATIONSHIP-ID TO CRELIDO
      *--- KYC ---------------------------------------------------------
           EVALUATE TRUE
               WHEN CUST-KYC-VERIFIED
                   MOVE 'VERIFIED'          TO CKYCO
               WHEN CUST-KYC-PENDING
                   MOVE 'PENDING (CIP)'     TO CKYCO
               WHEN CUST-KYC-FAILED
                   MOVE 'FAILED'            TO CKYCO
               WHEN CUST-KYC-EDD-REQUIRED
                   MOVE 'EDD REQUIRED'      TO CKYCO
               WHEN CUST-KYC-EXPIRED
                   PERFORM 2110-KYC-GRACE
           END-EVALUATE
           EVALUATE TRUE
               WHEN CUST-KYC-TIER-SIMPLIFIED
                   MOVE '1 SIMPLIFIED' TO CTIERO
               WHEN CUST-KYC-TIER-STANDARD
                   MOVE '2 STANDARD'   TO CTIERO
               WHEN CUST-KYC-TIER-ENHANCED
                   MOVE '3 ENHANCED'   TO CTIERO
           END-EVALUATE
           MOVE CUST-KYC-REVIEW-DATE TO WS-DATE-IN
           PERFORM 8100-EDIT-DATE
           MOVE WS-EDIT-DATE TO CKYCRVO
      *--- RISK / SCREENING --------------------------------------------
           EVALUATE TRUE
               WHEN CUST-RISK-LOW        MOVE 'LOW'        TO CRISKO
               WHEN CUST-RISK-MEDIUM     MOVE 'MEDIUM'     TO CRISKO
               WHEN CUST-RISK-HIGH       MOVE 'HIGH'       TO CRISKO
               WHEN CUST-RISK-PROHIBITED MOVE 'PROHIBITED' TO CRISKO
           END-EVALUATE
           MOVE CUST-RISK-SCORE    TO CSCOREO
           MOVE CUST-PEP-FLAG      TO CPEPO
           MOVE CUST-MSB-FLAG      TO CMSBO
           MOVE CUST-EMPLOYEE-FLAG TO CEMPLO
           EVALUATE TRUE
               WHEN CUST-OFAC-CLEAR
                   MOVE 'CLEAR'         TO COFACO
               WHEN CUST-OFAC-POTENTIAL-HIT
                   MOVE 'POTENTIAL HIT' TO COFACO
               WHEN CUST-OFAC-CONFIRMED-HIT
                   MOVE 'CONFIRMED HIT' TO COFACO
           END-EVALUATE
           IF CUST-IS-PEP OR NOT CUST-OFAC-CLEAR
               MOVE DFHRED TO CPEPC COFACC
           END-IF
           MOVE CUST-OFAC-LAST-SCREEN TO WS-DATE-IN
           PERFORM 8100-EDIT-DATE
           MOVE WS-EDIT-DATE TO COFDTO
           MOVE CUST-TOTAL-REL-BALANCE TO WS-EDIT-RELBAL
           MOVE WS-EDIT-RELBAL TO CRELBALO
           STRING CUST-LAST-MAINT-DATE ' ' CUST-LAST-MAINT-USER
                  DELIMITED BY SIZE INTO CMAINTO.

      *----------------------------------------------------------------*
      * EXPIRED: SHOW DAYS REMAINING IN THE 90-DAY GRACE BEFORE THE    *
      * MONTH-END KYCREVW RUN RESTRICTS THE ACCOUNTS                   *
      *----------------------------------------------------------------*
       2110-KYC-GRACE.
           INITIALIZE DATE-PARMS
           SET DTU-DAYS-BETWEEN TO TRUE
           MOVE CUST-KYC-REVIEW-DATE TO DTU-DATE-1
           MOVE CTL-BUSINESS-DATE    TO DTU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           COMPUTE WS-DAYS = WS-KYC-GRACE-DAYS - DTU-RESULT-NUMBER
           IF WS-DAYS > 0
               STRING 'EXPIRED-GRACE ' WS-DAYS(5:3)
                      DELIMITED BY SIZE INTO CKYCO
           ELSE
               MOVE 'EXPIRED-RESTRICT' TO CKYCO
           END-IF
           MOVE DFHRED TO CKYCC.

      *----------------------------------------------------------------*
      * BROWSE ACCOUNTS VIA AIX PATH ACCTCUS. PAGE N SKIPS (N-1) * 7.  *
      *----------------------------------------------------------------*
       2200-BROWSE-ACCOUNTS.
           MOVE CUST-ID TO WS-AIX-KEY
           MOVE ZERO TO WS-READ-CNT WS-LINE-CNT
           COMPUTE WS-SKIP = (CA-PAGE - 1) * WS-PAGE-SIZE
           MOVE 'N' TO WS-BROWSE-FLAG
           EXEC CICS STARTBR FILE('ACCTCUS') RIDFLD(WS-AIX-KEY)
                     KEYLENGTH(10) EQUAL RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'NO ACCOUNTS ON FILE FOR THIS CUSTOMER'
                 TO WS-MESSAGE
               MOVE 0 TO CA-ACCT-TOTAL
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-BROWSE-DONE
               EXEC CICS READNEXT FILE('ACCTCUS')
                         INTO(ACCOUNT-RECORD) RIDFLD(WS-AIX-KEY)
                         KEYLENGTH(10) RESP(WS-RESP)
               END-EXEC
               IF (WS-RESP NOT = DFHRESP(NORMAL)
                   AND WS-RESP NOT = DFHRESP(DUPKEY))
                  OR ACCT-PRIMARY-CUST-ID NOT = CUST-ID
                   SET WS-BROWSE-DONE TO TRUE
               ELSE
                   ADD 1 TO WS-READ-CNT
                   IF WS-READ-CNT > WS-SKIP
                      AND WS-LINE-CNT < WS-PAGE-SIZE
                       ADD 1 TO WS-LINE-CNT
                       PERFORM 2210-FORMAT-ACCOUNT-LINE
                   END-IF
               END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE('ACCTCUS') END-EXEC
           MOVE WS-READ-CNT TO CA-ACCT-TOTAL ACCTCNTO
           MOVE CA-PAGE     TO PAGENOO
           IF WS-LINE-CNT = 0 AND CA-PAGE > 1
               MOVE 'NO MORE ACCOUNTS' TO WS-MESSAGE
               SUBTRACT 1 FROM CA-PAGE
           ELSE
               MOVE 'INQUIRY COMPLETE' TO WS-MESSAGE
           END-IF.

       2210-FORMAT-ACCOUNT-LINE.
           MOVE ACCT-NUMBER       TO WS-AL-ACCT
           MOVE ACCT-PRODUCT-CODE TO WS-AL-PROD
           MOVE ACCT-STATUS       TO WS-AL-STATUS
           EVALUATE TRUE
               WHEN ACCT-OWN-JOINT     MOVE 'JNT ' TO WS-AL-OWN
               WHEN ACCT-OWN-TRUST     MOVE 'TRST' TO WS-AL-OWN
               WHEN ACCT-OWN-CORPORATE MOVE 'CORP' TO WS-AL-OWN
               WHEN OTHER              MOVE 'SGL ' TO WS-AL-OWN
           END-EVALUATE
           MOVE ACCT-LEDGER-BAL TO WS-AL-BAL
           MOVE ACCT-LAST-ACTIVITY-DATE TO WS-DATE-IN
           PERFORM 8100-EDIT-DATE
           MOVE WS-EDIT-DATE TO WS-AL-LASTACT
           PERFORM 2220-DORMANCY-NOTE
           MOVE WS-NOTE TO WS-AL-NOTE
           EVALUATE WS-LINE-CNT
               WHEN 1 MOVE WS-ACCT-LINE TO ALN01O
               WHEN 2 MOVE WS-ACCT-LINE TO ALN02O
               WHEN 3 MOVE WS-ACCT-LINE TO ALN03O
               WHEN 4 MOVE WS-ACCT-LINE TO ALN04O
               WHEN 5 MOVE WS-ACCT-LINE TO ALN05O
               WHEN 6 MOVE WS-ACCT-LINE TO ALN06O
               WHEN 7 MOVE WS-ACCT-LINE TO ALN07O
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTE COLUMN: STATUS TEXT, OR 'DORMANT SOON' WHEN AN ACTIVE     *
      * ACCOUNT IS WITHIN 30 DAYS OF ITS DORMANCY DATE                 *
      *----------------------------------------------------------------*
       2220-DORMANCY-NOTE.
           MOVE SPACES TO WS-NOTE
           EVALUATE TRUE
               WHEN ACCT-DORMANT    MOVE 'DORMANT'        TO WS-NOTE
               WHEN ACCT-ESCHEATED  MOVE 'ESCHEATED'      TO WS-NOTE
               WHEN ACCT-FROZEN     MOVE 'FROZEN'         TO WS-NOTE
               WHEN ACCT-CLOSED     MOVE 'CLOSED'         TO WS-NOTE
               WHEN ACCT-RESTRICTED
                   STRING 'RESTRICTED ' ACCT-RESTRICT-REASON
                          DELIMITED BY SIZE INTO WS-NOTE
               WHEN ACCT-ACTIVE
                   SET WS-DRM-IDX TO 1
                   SEARCH WS-DORM-ENTRY
                       AT END CONTINUE
                       WHEN WS-DORM-CAT(WS-DRM-IDX) = ACCT-CATEGORY
                           PERFORM 2230-DORMANT-SOON-TEST
                   END-SEARCH
           END-EVALUATE.

       2230-DORMANT-SOON-TEST.
           INITIALIZE DATE-PARMS
           SET DTU-ADD-MONTHS TO TRUE
           MOVE ACCT-LAST-ACTIVITY-DATE   TO DTU-DATE-1
           MOVE WS-DORM-MONTHS(WS-DRM-IDX) TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-DORMANT-ON
           INITIALIZE DATE-PARMS
           SET DTU-ADD-DAYS TO TRUE
           MOVE WS-DORMANT-ON TO DTU-DATE-1
           COMPUTE DTU-NUMBER = 0 - WS-DORM-WARN-DAYS
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-WARN-FROM
           IF CTL-BUSINESS-DATE >= WS-WARN-FROM
               MOVE 'DORMANT SOON' TO WS-NOTE
           END-IF.

      *----------------------------------------------------------------*
       8000-SEND-MAP-ERASE.
           MOVE WS-MESSAGE TO MSGO
           MOVE 'ENTER=INQUIRE PF7=PREV PF8=NEXT PF3=MENU PF12=CANCEL'
             TO PFKEYSO
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     MMDDYYYY(WS-DATE-OUT) DATESEP('/')
           END-EXEC
           MOVE WS-DATE-OUT TO CURDATEO
           MOVE -1 TO CUSTIDL
           EXEC CICS SEND MAP('CUINQM') MAPSET('CUINQS')
                     FROM(CUINQMO) ERASE CURSOR
           END-EXEC.

       8100-EDIT-DATE.
           IF WS-DATE-IN = ZERO
               MOVE SPACES TO WS-EDIT-DATE
           ELSE
               MOVE WS-DI-CCYY TO WS-ED-CCYY
               MOVE WS-DI-MM   TO WS-ED-MM
               MOVE WS-DI-DD   TO WS-ED-DD
           END-IF.
