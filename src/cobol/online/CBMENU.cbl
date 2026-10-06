      *================================================================*
      * PROGRAM  : CBMENU                                              *
      * TYPE     : CICS ONLINE - PSEUDO-CONVERSATIONAL                 *
      * TRANS    : CB00                                                *
      * MAPSET   : CBMNUS  MAP CBMNUM                                  *
      * PURPOSE  : CORE BANKING SYSTEM MAIN MENU. LISTS EVERY FUNCTION *
      *            THE SIGNED-ON USER'S ROLE IS AUTHORISED FOR AND     *
      *            TRANSFERS CONTROL (XCTL) TO THE SELECTED PROGRAM.   *
      *            ROLE IS DERIVED FROM THE RACF USER ID PREFIX:       *
      *              TLR* TELLER        OPS* OPERATIONS                *
      *              CMP* COMPLIANCE    MGR* BRANCH / OPS MANAGER      *
      *              TRS* TREASURY      LND* LENDING                   *
      *              OTHER - GENERAL (INQUIRY ONLY)                    *
      *            THE SAME MATRIX IS ENFORCED HERE ONLY - TARGET      *
      *            PROGRAMS RELY ON RACF TRANSACTION SECURITY.         *
      * PF KEYS  : ENTER SELECT, PF3 SIGN OFF, PF12 CANCEL (REDISPLAY) *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-03-20 RJT  ORIGINAL                                       *
      * 2004-06-14 BHS  PAYMENTS FUNCTIONS CB30-CB32                   *
      * 2009-03-02 DPS  LENDING FUNCTIONS CB40-CB42                    *
      * 2011-04-12 DPS  TREASURY FUNCTIONS CB50-CB52                   *
      * 2013-03-04 DPS  ALERT WORK QUEUE / OFAC REVIEW CB60-CB61       *
      * 2019-02-11 AKW  ROLE-BASED MENU FROM USER ID PREFIX (CHG1777)  *
      * 2022-07-25 JLM  CB12 HOLD MAINT OPENED TO COMPLIANCE (INC17112)*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBMENU.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANSID                      PIC X(04) VALUE 'CB00'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-RESP2                        PIC S9(08) COMP.
       01  WS-USERID                       PIC X(08).
       01  WS-ABSTIME                      PIC S9(15) COMP-3.
       01  WS-DATE-OUT                     PIC X(10).
       01  WS-MESSAGE                      PIC X(79).

      *--- ROLE ------------------------------------------------------*
       01  WS-ROLE                         PIC X(01).
           88  WS-ROLE-TELLER                        VALUE 'T'.
           88  WS-ROLE-OPERATIONS                    VALUE 'O'.
           88  WS-ROLE-COMPLIANCE                    VALUE 'C'.
           88  WS-ROLE-MANAGER                       VALUE 'M'.
           88  WS-ROLE-TREASURY                      VALUE 'R'.
           88  WS-ROLE-LENDING                       VALUE 'L'.
           88  WS-ROLE-GENERAL                       VALUE 'G'.
       01  WS-ROLE-SUB                     PIC 9(01).
       01  WS-ROLE-DESC                    PIC X(20).

      *--- FUNCTION / AUTHORISATION MATRIX ----------------------------*
      *    TRAN PROGRAM  DESCRIPTION (30)               T O C M R L G
       01  WS-FUNCTION-VALUES.
           05  FILLER PIC X(49) VALUE
               'CB01CUSTINQ CUSTOMER INQUIRY              YYYYYYY'.
           05  FILLER PIC X(49) VALUE
               'CB02CUSTMNT CUSTOMER MAINTENANCE          YYNYNNN'.
           05  FILLER PIC X(49) VALUE
               'CB03ACCTOPENOPEN ACCOUNT                  YYNYNNN'.
           05  FILLER PIC X(49) VALUE
               'CB04ACCTINQ ACCOUNT INQUIRY               YYYYYYY'.
           05  FILLER PIC X(49) VALUE
               'CB05ACCTCLS CLOSE ACCOUNT                 NYNYNNN'.
           05  FILLER PIC X(49) VALUE
               'CB10XFERFUNDFUNDS TRANSFER                YYNYYNN'.
           05  FILLER PIC X(49) VALUE
               'CB11TELLTRN TELLER CASH                   YNNYNNN'.
           05  FILLER PIC X(49) VALUE
               'CB12HOLDMNT HOLD MAINTENANCE              NYYYNNN'.
           05  FILLER PIC X(49) VALUE
               'CB20RATEINQ RATE INQUIRY                  YYYYYYY'.
           05  FILLER PIC X(49) VALUE
               'CB21CDQUOTE CD EARLY WITHDRAWAL QUOTE     YYNYNNN'.
           05  FILLER PIC X(49) VALUE
               'CB22FEEREV  FEE REVERSAL                  NYNYNNN'.
           05  FILLER PIC X(49) VALUE
               'CB30PAYINIT PAYMENT INITIATION            YYNYYNN'.
           05  FILLER PIC X(49) VALUE
               'CB31WIREAPPRWIRE APPROVAL                 NNNYYNN'.
           05  FILLER PIC X(49) VALUE
               'CB32PAYINQ  PAYMENT INQUIRY               YYYYYYY'.
           05  FILLER PIC X(49) VALUE
               'CB40LNINQ   LOAN INQUIRY                  YYYYNYY'.
           05  FILLER PIC X(49) VALUE
               'CB41LNPAYOF LOAN PAYOFF QUOTE             YYNYNYN'.
           05  FILLER PIC X(49) VALUE
               'CB42LNORIG  LOAN ORIGINATION              NNNYNYN'.
           05  FILLER PIC X(49) VALUE
               'CB50CMPOSINQCASH POSITION                 NNNYYNN'.
           05  FILLER PIC X(49) VALUE
               'CB51SWEEPMNTSWEEP MAINTENANCE             NYNYYNN'.
           05  FILLER PIC X(49) VALUE
               'CB52GLINQ   GL INQUIRY                    NYNYYNN'.
           05  FILLER PIC X(49) VALUE
               'CB60ALRTWORKALERT WORK QUEUE              NYYYNNN'.
           05  FILLER PIC X(49) VALUE
               'CB61OFACREV OFAC REVIEW                   NNYYNNN'.
       01  WS-FUNCTION-TABLE REDEFINES WS-FUNCTION-VALUES.
           05  WS-FN-ENTRY OCCURS 22 TIMES INDEXED BY WS-FN-IDX.
               10  WS-FN-TRANID            PIC X(04).
               10  WS-FN-PROGRAM           PIC X(08).
               10  WS-FN-DESC              PIC X(30).
               10  WS-FN-AUTH              PIC X(01) OCCURS 7 TIMES.
       01  WS-FN-COUNT                     PIC 9(02) VALUE 22.
       01  WS-SUB                          PIC 9(02).
       01  WS-SEL-NUM                      PIC 9(02).
       01  WS-SEL-FOUND                    PIC 9(02).
       01  WS-OPT-LINE                     PIC X(36).

       01  WS-COMMAREA.
           05  CA-ROLE                     PIC X(01).
           05  CA-USERID                   PIC X(08).

       COPY CBMNUS.
       COPY DFHAID.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(09).

       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS ASSIGN USERID(WS-USERID) END-EXEC
           PERFORM 1000-DETERMINE-ROLE
           IF EIBCALEN = 0
               MOVE LOW-VALUES TO CBMNUMO
               PERFORM 3000-SEND-MENU-ERASE
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               EVALUATE EIBAID
                   WHEN DFHPF3
                       PERFORM 9000-SIGN-OFF
                   WHEN DFHPF12
                   WHEN DFHCLEAR
                       MOVE LOW-VALUES TO CBMNUMO
                       PERFORM 3000-SEND-MENU-ERASE
                   WHEN DFHENTER
                       PERFORM 2000-PROCESS-SELECTION
                   WHEN OTHER
                       MOVE 'INVALID KEY - ENTER, PF3 OR PF12'
                         TO WS-MESSAGE
                       PERFORM 3100-SEND-MENU-DATAONLY
               END-EVALUATE
           END-IF
           MOVE WS-ROLE   TO CA-ROLE
           MOVE WS-USERID TO CA-USERID
           EXEC CICS RETURN TRANSID(WS-TRANSID)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      * ROLE FROM USER ID PREFIX                                       *
      *----------------------------------------------------------------*
       1000-DETERMINE-ROLE.
           EVALUATE WS-USERID(1:3)
               WHEN 'TLR'
                   SET WS-ROLE-TELLER TO TRUE
                   MOVE 1 TO WS-ROLE-SUB
                   MOVE 'TELLER'            TO WS-ROLE-DESC
               WHEN 'OPS'
                   SET WS-ROLE-OPERATIONS TO TRUE
                   MOVE 2 TO WS-ROLE-SUB
                   MOVE 'OPERATIONS'        TO WS-ROLE-DESC
               WHEN 'CMP'
                   SET WS-ROLE-COMPLIANCE TO TRUE
                   MOVE 3 TO WS-ROLE-SUB
                   MOVE 'COMPLIANCE / BSA'  TO WS-ROLE-DESC
               WHEN 'MGR'
                   SET WS-ROLE-MANAGER TO TRUE
                   MOVE 4 TO WS-ROLE-SUB
                   MOVE 'MANAGER'           TO WS-ROLE-DESC
               WHEN 'TRS'
                   SET WS-ROLE-TREASURY TO TRUE
                   MOVE 5 TO WS-ROLE-SUB
                   MOVE 'TREASURY SERVICES' TO WS-ROLE-DESC
               WHEN 'LND'
                   SET WS-ROLE-LENDING TO TRUE
                   MOVE 6 TO WS-ROLE-SUB
                   MOVE 'LENDING'           TO WS-ROLE-DESC
               WHEN OTHER
                   SET WS-ROLE-GENERAL TO TRUE
                   MOVE 7 TO WS-ROLE-SUB
                   MOVE 'GENERAL (INQUIRY)' TO WS-ROLE-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECTION BY TRANSACTION ID (CB04) OR LINE NUMBER (01-22)      *
      *----------------------------------------------------------------*
       2000-PROCESS-SELECTION.
           EXEC CICS RECEIVE MAP('CBMNUM') MAPSET('CBMNUS')
                     INTO(CBMNUMI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(MAPFAIL) OR CHOICEL = 0
               MOVE 'ENTER A TRANSACTION ID OR OPTION NUMBER'
                 TO WS-MESSAGE
               PERFORM 3100-SEND-MENU-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO WS-SEL-FOUND
           IF CHOICEI(1:2) IS NUMERIC AND CHOICEI(3:2) = SPACES
               MOVE CHOICEI(1:2) TO WS-SEL-NUM
               IF WS-SEL-NUM >= 1 AND WS-SEL-NUM <= WS-FN-COUNT
                   MOVE WS-SEL-NUM TO WS-SEL-FOUND
               END-IF
           ELSE
               PERFORM VARYING WS-SUB FROM 1 BY 1
                       UNTIL WS-SUB > WS-FN-COUNT
                   IF WS-FN-TRANID(WS-SUB) = CHOICEI
                       MOVE WS-SUB TO WS-SEL-FOUND
                   END-IF
               END-PERFORM
           END-IF
           EVALUATE TRUE
               WHEN WS-SEL-FOUND = 0
                   MOVE 'INVALID SELECTION' TO WS-MESSAGE
                   PERFORM 3100-SEND-MENU-DATAONLY
               WHEN WS-FN-AUTH(WS-SEL-FOUND, WS-ROLE-SUB) NOT = 'Y'
                   STRING 'FUNCTION ' WS-FN-TRANID(WS-SEL-FOUND)
                          ' NOT AUTHORISED FOR ROLE '
                          WS-ROLE-DESC
                          DELIMITED BY SIZE INTO WS-MESSAGE
                   PERFORM 3100-SEND-MENU-DATAONLY
               WHEN OTHER
                   EXEC CICS XCTL
                             PROGRAM(WS-FN-PROGRAM(WS-SEL-FOUND))
                             RESP(WS-RESP)
                   END-EXEC
      *            XCTL ONLY RETURNS ON FAILURE (PGMIDERR)
                   STRING 'PROGRAM ' WS-FN-PROGRAM(WS-SEL-FOUND)
                          ' NOT AVAILABLE - CONTACT HELP DESK'
                          DELIMITED BY SIZE INTO WS-MESSAGE
                   PERFORM 3100-SEND-MENU-DATAONLY
           END-EVALUATE.

      *----------------------------------------------------------------*
       2500-BUILD-MENU-LINES.
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > WS-FN-COUNT
               MOVE SPACES TO WS-OPT-LINE
               IF WS-FN-AUTH(WS-SUB, WS-ROLE-SUB) = 'Y'
                   STRING WS-SUB '  ' WS-FN-TRANID(WS-SUB) '  '
                          WS-FN-DESC(WS-SUB)
                          DELIMITED BY SIZE INTO WS-OPT-LINE
               END-IF
               EVALUATE WS-SUB
                   WHEN 01 MOVE WS-OPT-LINE TO OPT01O
                   WHEN 02 MOVE WS-OPT-LINE TO OPT02O
                   WHEN 03 MOVE WS-OPT-LINE TO OPT03O
                   WHEN 04 MOVE WS-OPT-LINE TO OPT04O
                   WHEN 05 MOVE WS-OPT-LINE TO OPT05O
                   WHEN 06 MOVE WS-OPT-LINE TO OPT06O
                   WHEN 07 MOVE WS-OPT-LINE TO OPT07O
                   WHEN 08 MOVE WS-OPT-LINE TO OPT08O
                   WHEN 09 MOVE WS-OPT-LINE TO OPT09O
                   WHEN 10 MOVE WS-OPT-LINE TO OPT10O
                   WHEN 11 MOVE WS-OPT-LINE TO OPT11O
                   WHEN 12 MOVE WS-OPT-LINE TO OPT12O
                   WHEN 13 MOVE WS-OPT-LINE TO OPT13O
                   WHEN 14 MOVE WS-OPT-LINE TO OPT14O
                   WHEN 15 MOVE WS-OPT-LINE TO OPT15O
                   WHEN 16 MOVE WS-OPT-LINE TO OPT16O
                   WHEN 17 MOVE WS-OPT-LINE TO OPT17O
                   WHEN 18 MOVE WS-OPT-LINE TO OPT18O
                   WHEN 19 MOVE WS-OPT-LINE TO OPT19O
                   WHEN 20 MOVE WS-OPT-LINE TO OPT20O
                   WHEN 21 MOVE WS-OPT-LINE TO OPT21O
                   WHEN 22 MOVE WS-OPT-LINE TO OPT22O
               END-EVALUATE
           END-PERFORM
           MOVE WS-USERID   TO USERIDO
           MOVE WS-ROLE-DESC TO ROLEDSCO
           MOVE 'ENTER=SELECT  PF3=SIGN OFF  PF12=CANCEL' TO PFKEYSO
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     MMDDYYYY(WS-DATE-OUT) DATESEP('/')
           END-EXEC
           MOVE WS-DATE-OUT TO CURDATEO BUSDTO.

       3000-SEND-MENU-ERASE.
           PERFORM 2500-BUILD-MENU-LINES
           MOVE 'SELECT A FUNCTION' TO MSGO
           EXEC CICS SEND MAP('CBMNUM') MAPSET('CBMNUS')
                     FROM(CBMNUMO) ERASE CURSOR
           END-EXEC.

       3100-SEND-MENU-DATAONLY.
           PERFORM 2500-BUILD-MENU-LINES
           MOVE WS-MESSAGE TO MSGO
           MOVE -1 TO CHOICEL
           EXEC CICS SEND MAP('CBMNUM') MAPSET('CBMNUS')
                     FROM(CBMNUMO) DATAONLY CURSOR
           END-EXEC.

       9000-SIGN-OFF.
           EXEC CICS SEND TEXT FROM('CBS SESSION ENDED')
                     LENGTH(17) ERASE FREEKB
           END-EXEC
           EXEC CICS RETURN END-EXEC.
