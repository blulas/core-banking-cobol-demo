      *================================================================*
      * PROGRAM  : CUSTMNT                                             *
      * TYPE     : CICS ONLINE - PSEUDO-CONVERSATIONAL                 *
      * TRANS    : CB02                                                *
      * MAPSET   : CUMNTS  MAP CUMNTM                                  *
      * PURPOSE  : CUSTOMER MAINTENANCE.                               *
      *            - ADDRESS / PHONE / EMAIL APPLIED IMMEDIATELY       *
      *            - LEGAL NAME AND TAX ID ARE DUAL-CONTROL FIELDS:    *
      *              THE CHANGE IS WRITTEN TO CUSTPND AND APPLIED ONLY *
      *              WHEN A DIFFERENT USER APPROVES IT (PF10)          *
      *            - COUNTRY CHANGE RE-TIERS THE CUSTOMER VIA KYCTIER; *
      *              A MOVE TO A HIGH-RISK JURISDICTION FORCES EDD     *
      *            - RISK RATING OVERRIDE (MGR/CMP ONLY, REASON        *
      *              MANDATORY) RESETS THE KYC REVIEW DATE             *
      * FILES    : CUSTMST (UPDATE), CUSTPND, BANKCTL                  *
      * CALLS    : KYCTIER, DATEUTIL                                   *
      * PF KEYS  : ENTER DISPLAY, PF5 UPDATE, PF10 APPROVE PENDING,    *
      *            PF11 REJECT PENDING, PF3 MENU, PF12 CANCEL          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-03-20 RJT  ORIGINAL                                       *
      * 2002-04-01 BHS  RISK RATING MAINTENANCE                        *
      * 2007-01-15 BHS  DUAL CONTROL FOR NAME / TAX ID (SOX, CHG0102)  *
      * 2007-06-04 BHS  NAME / TAX ID CHANGE FORCES OFAC RE-SCREEN     *
      * 2011-02-01 DPS  HIGH-RISK COUNTRY MOVE -> EDD (CHG0388)        *
      * 2013-08-19 DPS  PENDING CHANGES EXPIRE AFTER 5 BUSINESS DAYS   *
      * 2018-05-11 AKW  COUNTRY CHANGE CALLS KYCTIER                   *
      * 2021-06-07 JLM  RISK OVERRIDE LIMITED TO MGR / CMP (INC16620)  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANSID                      PIC X(04) VALUE 'CB02'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-MESSAGE                      PIC X(79).
       01  WS-RESULT                       PIC X(69).
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.
       01  WS-USERID                       PIC X(08).
       01  WS-ABSTIME                      PIC S9(15) COMP-3.
       01  WS-DATE-OUT                     PIC X(10).
       01  WS-TIME-OUT                     PIC 9(06).

       01  WS-FLAGS.
           05  WS-ERROR-FLAG               PIC X(01).
               88  WS-INPUT-ERROR                    VALUE 'Y'.
           05  WS-APPLIED-FLAG             PIC X(01).
               88  WS-CHANGES-APPLIED                VALUE 'Y'.
           05  WS-PENDING-FLAG             PIC X(01).
               88  WS-PENDING-WRITTEN                VALUE 'Y'.
           05  WS-COUNTRY-CHG-FLAG         PIC X(01).
               88  WS-COUNTRY-CHANGED                VALUE 'Y'.
           05  WS-HRC-FLAG                 PIC X(01).
               88  WS-NEW-COUNTRY-HIGH-RISK          VALUE 'Y'.
           05  WS-BROWSE-FLAG              PIC X(01).
               88  WS-BROWSE-DONE                    VALUE 'Y'.

      *--- REVIEW CYCLE APPLIED WHEN RISK RATING IS OVERRIDDEN -------*
      *    (SET BY RISK RATING, NOT TIER - PRE-DATES KYCTIER)          *
       01  WS-OVR-REVIEW-VALUES.
           05  FILLER PIC X(04) VALUE 'P012'.
           05  FILLER PIC X(04) VALUE 'H012'.
           05  FILLER PIC X(04) VALUE 'M036'.
           05  FILLER PIC X(04) VALUE 'L036'.
       01  WS-OVR-REVIEW-TABLE REDEFINES WS-OVR-REVIEW-VALUES.
           05  WS-OVR-ENTRY OCCURS 4 TIMES INDEXED BY WS-OVR-IDX.
               10  WS-OVR-RATING           PIC X(01).
               10  WS-OVR-MONTHS           PIC 9(03).
       01  WS-PENDING-EXPIRY-BUS-DAYS      PIC S9(05) VALUE +5.

       01  WS-NEW-VALUES.
           05  WS-NEW-LNAME                PIC X(30).
           05  WS-NEW-FNAME                PIC X(20).
           05  WS-NEW-MINIT                PIC X(01).
           05  WS-NEW-TAXID                PIC X(09).
           05  WS-NEW-TAXTYP               PIC X(01).
           05  WS-NEW-COUNTRY              PIC X(02).
       01  WS-PND-SEQ                      PIC 9(04).
       01  WS-PND-COUNT                    PIC 9(03).
       01  WS-PND-KEY-WORK.
           05  WS-PK-CUST                  PIC X(10).
           05  WS-PK-SEQ                   PIC 9(04).
       01  WS-AT-POS                       PIC 9(03).
       01  WS-OLD-RISK                     PIC X(01).

       01  WS-COMMAREA.
           05  CA-CUST-ID                  PIC X(10).
           05  CA-LAST-MAINT-DATE          PIC 9(08).
           05  CA-LAST-MAINT-USER          PIC X(08).

       COPY CUSTREC.
       COPY CUSTPND.
       COPY BANKCTL.
       COPY DATEPARM.
       COPY KYCTPARM.
       COPY CIFRSKTB.
       COPY CUMNTS.
       COPY DFHAID.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(26).

       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS ASSIGN USERID(WS-USERID) END-EXEC
           EXEC CICS READ FILE('BANKCTL') INTO(BANK-CONTROL-RECORD)
                     RIDFLD(WS-CTL-KEY) RESP(WS-RESP)
           END-EXEC
           IF EIBCALEN = 0
               INITIALIZE WS-COMMAREA
               MOVE LOW-VALUES TO CUMNTMO
               MOVE 'ENTER CUSTOMER ID' TO WS-MESSAGE
               PERFORM 8000-SEND-MAP
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               EVALUATE EIBAID
                   WHEN DFHPF3
                       EXEC CICS XCTL PROGRAM('CBMENU') END-EXEC
                   WHEN DFHPF12
                   WHEN DFHCLEAR
                       INITIALIZE WS-COMMAREA
                       MOVE LOW-VALUES TO CUMNTMO
                       MOVE 'CANCELLED - ENTER CUSTOMER ID'
                         TO WS-MESSAGE
                       PERFORM 8000-SEND-MAP
                   WHEN DFHENTER
                       PERFORM 1000-RECEIVE-MAP
                       PERFORM 2000-DISPLAY-CUSTOMER
                   WHEN DFHPF5
                       PERFORM 1000-RECEIVE-MAP
                       PERFORM 3000-UPDATE-CUSTOMER
                   WHEN DFHPF10
                       PERFORM 1000-RECEIVE-MAP
                       PERFORM 5000-APPROVE-PENDING
                   WHEN DFHPF11
                       PERFORM 1000-RECEIVE-MAP
                       PERFORM 5500-REJECT-PENDING
                   WHEN OTHER
                       MOVE 'INVALID KEY' TO WS-MESSAGE
                       PERFORM 8000-SEND-MAP
               END-EVALUATE
           END-IF
           EXEC CICS RETURN TRANSID(WS-TRANSID)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
       1000-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP('CUMNTM') MAPSET('CUMNTS')
                     INTO(CUMNTMI) RESP(WS-RESP)
           END-EXEC
           IF CUSTIDL > 0 AND CUSTIDI NOT = CA-CUST-ID
               MOVE CUSTIDI TO CA-CUST-ID
               MOVE ZERO    TO CA-LAST-MAINT-DATE
           END-IF.

      *----------------------------------------------------------------*
       2000-DISPLAY-CUSTOMER.
           MOVE CA-CUST-ID TO CUST-ID
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                     RIDFLD(CUST-ID) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE LOW-VALUES TO CUMNTMO
               MOVE 'CUSTOMER NOT FOUND' TO WS-MESSAGE
               PERFORM 8000-SEND-MAP
               EXIT PARAGRAPH
           END-IF
           MOVE CUST-LAST-MAINT-DATE TO CA-LAST-MAINT-DATE
           MOVE CUST-LAST-MAINT-USER TO CA-LAST-MAINT-USER
           PERFORM 2100-FORMAT-SCREEN
           MOVE 'MAKE CHANGES AND PRESS PF5' TO WS-MESSAGE
           PERFORM 8000-SEND-MAP.

       2100-FORMAT-SCREEN.
           MOVE LOW-VALUES TO CUMNTMO
           MOVE CUST-ID          TO CUSTIDO
           MOVE CUST-STATUS      TO CSTATO
           STRING CUST-LAST-MAINT-DATE ' ' CUST-LAST-MAINT-USER(1:6)
                  DELIMITED BY SIZE INTO CMAINTO
           MOVE CUST-LAST-NAME   TO LNAMEO
           MOVE CUST-FIRST-NAME  TO FNAMEO
           MOVE CUST-MIDDLE-INIT TO MINITO
           MOVE CUST-TAX-ID      TO TAXIDO
           MOVE CUST-TAX-ID-TYPE TO TAXTYPO
           MOVE CUST-ADDR-LINE1  TO ADDR1O
           MOVE CUST-ADDR-LINE2  TO ADDR2O
           MOVE CUST-CITY        TO CITYO
           MOVE CUST-STATE       TO STATEO
           MOVE CUST-ZIP         TO ZIPO
           MOVE CUST-COUNTRY     TO CTRYO
           MOVE CUST-PHONE       TO PHONEO
           MOVE CUST-EMAIL       TO EMAILO
           MOVE CUST-RISK-RATING TO CRISKO
           MOVE CUST-KYC-TIER    TO CTIERO
           MOVE CUST-KYC-STATUS  TO CKYCO
           MOVE CUST-KYC-REVIEW-DATE TO CKYCRVO
           PERFORM 2200-COUNT-PENDING
           IF WS-PND-COUNT > 0
               STRING WS-PND-COUNT ' CHANGE(S) AWAITING CHECKER'
                      DELIMITED BY SIZE INTO PENDDSCO
           END-IF.

      *----------------------------------------------------------------*
       2200-COUNT-PENDING.
           MOVE ZERO TO WS-PND-COUNT WS-PND-SEQ
           MOVE CUST-ID TO WS-PK-CUST
           MOVE ZERO    TO WS-PK-SEQ
           MOVE 'N' TO WS-BROWSE-FLAG
           EXEC CICS STARTBR FILE('CUSTPND') RIDFLD(WS-PND-KEY-WORK)
                     GTEQ RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-BROWSE-DONE
               EXEC CICS READNEXT FILE('CUSTPND')
                         INTO(CUST-PENDING-RECORD)
                         RIDFLD(WS-PND-KEY-WORK) RESP(WS-RESP)
               END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                  OR PND-CUST-ID NOT = CUST-ID
                   SET WS-BROWSE-DONE TO TRUE
               ELSE
                   MOVE PND-SEQ TO WS-PND-SEQ
                   IF PND-AWAITING-CHECKER
                      AND PND-EXPIRY-DATE >= CTL-BUSINESS-DATE
                       ADD 1 TO WS-PND-COUNT
                   END-IF
               END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE('CUSTPND') END-EXEC.

      *----------------------------------------------------------------*
      * PF5 - VALIDATE, APPLY STANDARD FIELDS, QUEUE CONTROLLED FIELDS *
      *----------------------------------------------------------------*
       3000-UPDATE-CUSTOMER.
           MOVE 'N' TO WS-ERROR-FLAG WS-APPLIED-FLAG WS-PENDING-FLAG
                       WS-COUNTRY-CHG-FLAG WS-HRC-FLAG
           MOVE SPACES TO WS-RESULT
           MOVE CA-CUST-ID TO CUST-ID
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                     RIDFLD(CUST-ID) UPDATE RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CUSTOMER NOT FOUND - PRESS ENTER FIRST'
                 TO WS-MESSAGE
               PERFORM 8000-SEND-MAP
               EXIT PARAGRAPH
           END-IF
      *    OPTIMISTIC LOCK - SOMEONE ELSE CHANGED THE RECORD
           IF CUST-LAST-MAINT-DATE NOT = CA-LAST-MAINT-DATE
              OR CUST-LAST-MAINT-USER NOT = CA-LAST-MAINT-USER
               EXEC CICS UNLOCK FILE('CUSTMST') END-EXEC
               PERFORM 2100-FORMAT-SCREEN
               MOVE 'RECORD CHANGED BY ANOTHER USER - REVIEW AND RETRY'
                 TO WS-MESSAGE
               MOVE CUST-LAST-MAINT-DATE TO CA-LAST-MAINT-DATE
               MOVE CUST-LAST-MAINT-USER TO CA-LAST-MAINT-USER
               PERFORM 8000-SEND-MAP
               EXIT PARAGRAPH
           END-IF
           PERFORM 3100-VALIDATE-INPUT
           IF WS-INPUT-ERROR
               EXEC CICS UNLOCK FILE('CUSTMST') END-EXEC
               PERFORM 8000-SEND-MAP
               EXIT PARAGRAPH
           END-IF
           PERFORM 3200-STANDARD-FIELDS
           PERFORM 3300-CONTROLLED-FIELDS
           IF RISKOVRL > 0 AND RISKOVRI NOT = SPACE
              AND RISKOVRI NOT = CUST-RISK-RATING
               PERFORM 3400-RISK-OVERRIDE
           END-IF
           IF WS-COUNTRY-CHANGED
               PERFORM 3500-COUNTRY-RETIER
           END-IF
           IF WS-INPUT-ERROR
               EXEC CICS UNLOCK FILE('CUSTMST') END-EXEC
               PERFORM 8000-SEND-MAP
               EXIT PARAGRAPH
           END-IF
           IF WS-CHANGES-APPLIED
               MOVE CTL-BUSINESS-DATE TO CUST-LAST-MAINT-DATE
               MOVE WS-USERID         TO CUST-LAST-MAINT-USER
               EXEC CICS REWRITE FILE('CUSTMST')
                         FROM(CUSTOMER-RECORD) RESP(WS-RESP)
               END-EXEC
               MOVE CUST-LAST-MAINT-DATE TO CA-LAST-MAINT-DATE
               MOVE CUST-LAST-MAINT-USER TO CA-LAST-MAINT-USER
           ELSE
               EXEC CICS UNLOCK FILE('CUSTMST') END-EXEC
           END-IF
           PERFORM 2100-FORMAT-SCREEN
           EVALUATE TRUE
               WHEN WS-CHANGES-APPLIED AND WS-PENDING-WRITTEN
                   MOVE 'UPDATED - NAME/TAX ID CHANGE SENT TO CHECKER'
                     TO WS-MESSAGE
               WHEN WS-CHANGES-APPLIED
                   MOVE 'CUSTOMER UPDATED' TO WS-MESSAGE
               WHEN WS-PENDING-WRITTEN
                   MOVE 'NAME/TAX ID CHANGE PENDING CHECKER APPROVAL'
                     TO WS-MESSAGE
               WHEN OTHER
                   MOVE 'NO CHANGES DETECTED' TO WS-MESSAGE
           END-EVALUATE
           MOVE WS-RESULT TO RESULTO
           PERFORM 8000-SEND-MAP.

      *----------------------------------------------------------------*
       3100-VALIDATE-INPUT.
           MOVE CUST-COUNTRY TO WS-NEW-COUNTRY
           IF CTRYL > 0
               MOVE CTRYI TO WS-NEW-COUNTRY
           END-IF
           IF WS-NEW-COUNTRY = 'US'
               IF STATEL > 0 AND STATEI = SPACES
                   MOVE 'STATE REQUIRED FOR US ADDRESS' TO WS-MESSAGE
                   SET WS-INPUT-ERROR TO TRUE
                   MOVE -1 TO STATEL
               END-IF
               IF ZIPL > 0
                  AND NOT (ZIPI(1:5) IS NUMERIC
                      AND (ZIPI(6:5) = SPACES
                       OR (ZIPI(6:1) = '-' AND ZIPI(7:4) IS NUMERIC)))
                   MOVE 'US ZIP MUST BE 99999 OR 99999-9999'
                     TO WS-MESSAGE
                   SET WS-INPUT-ERROR TO TRUE
                   MOVE -1 TO ZIPL
               END-IF
           END-IF
           IF EMAILL > 0 AND EMAILI NOT = SPACES
               MOVE ZERO TO WS-AT-POS
               INSPECT EMAILI TALLYING WS-AT-POS FOR ALL '@'
               IF WS-AT-POS NOT = 1
                   MOVE 'EMAIL ADDRESS IS INVALID' TO WS-MESSAGE
                   SET WS-INPUT-ERROR TO TRUE
                   MOVE -1 TO EMAILL
               END-IF
           END-IF
           IF TAXIDL > 0 AND TAXIDI IS NOT NUMERIC
               MOVE 'TAX ID MUST BE 9 DIGITS' TO WS-MESSAGE
               SET WS-INPUT-ERROR TO TRUE
               MOVE -1 TO TAXIDL
           END-IF
           IF RISKOVRL > 0 AND RISKOVRI NOT = SPACE
               IF RISKOVRI NOT = 'L' AND NOT = 'M' AND NOT = 'H'
                  AND NOT = 'P'
                   MOVE 'RISK OVERRIDE MUST BE L, M, H OR P'
                     TO WS-MESSAGE
                   SET WS-INPUT-ERROR TO TRUE
               END-IF
               IF RSKRSNL = 0 OR RSKRSNI = SPACES
                   MOVE 'REASON IS MANDATORY FOR A RISK OVERRIDE'
                     TO WS-MESSAGE
                   SET WS-INPUT-ERROR TO TRUE
                   MOVE -1 TO RSKRSNL
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * STANDARD FIELDS - APPLIED IMMEDIATELY                          *
      *----------------------------------------------------------------*
       3200-STANDARD-FIELDS.
           IF ADDR1L > 0 AND ADDR1I NOT = CUST-ADDR-LINE1
               MOVE ADDR1I TO CUST-ADDR-LINE1
               SET WS-CHANGES-APPLIED TO TRUE
           END-IF
           IF ADDR2L > 0 AND ADDR2I NOT = CUST-ADDR-LINE2
               MOVE ADDR2I TO CUST-ADDR-LINE2
               SET WS-CHANGES-APPLIED TO TRUE
           END-IF
           IF CITYL > 0 AND CITYI NOT = CUST-CITY
               MOVE CITYI TO CUST-CITY
               SET WS-CHANGES-APPLIED TO TRUE
           END-IF
           IF STATEL > 0 AND STATEI NOT = CUST-STATE
               MOVE STATEI TO CUST-STATE
               SET WS-CHANGES-APPLIED TO TRUE
           END-IF
           IF ZIPL > 0 AND ZIPI NOT = CUST-ZIP
               MOVE ZIPI TO CUST-ZIP
               SET WS-CHANGES-APPLIED TO TRUE
           END-IF
           IF WS-NEW-COUNTRY NOT = CUST-COUNTRY
               MOVE WS-NEW-COUNTRY TO CUST-COUNTRY
               SET WS-CHANGES-APPLIED TO TRUE
               SET WS-COUNTRY-CHANGED TO TRUE
           END-IF
           IF PHONEL > 0 AND PHONEI NOT = CUST-PHONE
               MOVE PHONEI TO CUST-PHONE
               SET WS-CHANGES-APPLIED TO TRUE
           END-IF
           IF EMAILL > 0 AND EMAILI NOT = CUST-EMAIL
               MOVE EMAILI TO CUST-EMAIL
               SET WS-CHANGES-APPLIED TO TRUE
           END-IF.

      *----------------------------------------------------------------*
      * CONTROLLED FIELDS - NEVER APPLIED BY THE MAKER. EACH CHANGED   *
      * FIELD IS QUEUED ON CUSTPND FOR A SECOND USER.                  *
      *----------------------------------------------------------------*
       3300-CONTROLLED-FIELDS.
           PERFORM 2200-COUNT-PENDING
           IF LNAMEL > 0 AND LNAMEI NOT = CUST-LAST-NAME
               MOVE 'LNAM' TO PND-FIELD-ID
               MOVE CUST-LAST-NAME TO PND-OLD-VALUE
               MOVE LNAMEI         TO PND-NEW-VALUE
               PERFORM 3310-WRITE-PENDING
           END-IF
           IF FNAMEL > 0 AND FNAMEI NOT = CUST-FIRST-NAME
               MOVE 'FNAM' TO PND-FIELD-ID
               MOVE CUST-FIRST-NAME TO PND-OLD-VALUE
               MOVE FNAMEI          TO PND-NEW-VALUE
               PERFORM 3310-WRITE-PENDING
           END-IF
           IF MINITL > 0 AND MINITI NOT = CUST-MIDDLE-INIT
               MOVE 'MINI' TO PND-FIELD-ID
               MOVE CUST-MIDDLE-INIT TO PND-OLD-VALUE
               MOVE MINITI           TO PND-NEW-VALUE
               PERFORM 3310-WRITE-PENDING
           END-IF
           IF TAXIDL > 0 AND TAXIDI NOT = CUST-TAX-ID
               MOVE 'TXID' TO PND-FIELD-ID
               MOVE CUST-TAX-ID TO PND-OLD-VALUE
               MOVE TAXIDI      TO PND-NEW-VALUE
               PERFORM 3310-WRITE-PENDING
           END-IF
           IF TAXTYPL > 0 AND TAXTYPI NOT = CUST-TAX-ID-TYPE
               MOVE 'TXTP' TO PND-FIELD-ID
               MOVE CUST-TAX-ID-TYPE TO PND-OLD-VALUE
               MOVE TAXTYPI          TO PND-NEW-VALUE
               PERFORM 3310-WRITE-PENDING
           END-IF.

       3310-WRITE-PENDING.
           ADD 1 TO WS-PND-SEQ
           MOVE CUST-ID    TO PND-CUST-ID
           MOVE WS-PND-SEQ TO PND-SEQ
           SET PND-AWAITING-CHECKER TO TRUE
           MOVE WS-USERID  TO PND-MAKER-USER
           MOVE CTL-BUSINESS-DATE TO PND-MAKER-DATE
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     TIME(WS-TIME-OUT)
           END-EXEC
           MOVE WS-TIME-OUT TO PND-MAKER-TIME
           MOVE SPACES TO PND-CHECKER-USER PND-REJECT-REASON
           MOVE ZERO   TO PND-CHECKER-DATE
           INITIALIZE DATE-PARMS
           SET DTU-ADD-BUS-DAYS TO TRUE
           MOVE CTL-BUSINESS-DATE          TO DTU-DATE-1
           MOVE WS-PENDING-EXPIRY-BUS-DAYS TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO PND-EXPIRY-DATE
           EXEC CICS WRITE FILE('CUSTPND') FROM(CUST-PENDING-RECORD)
                     RIDFLD(PND-KEY) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               SET WS-PENDING-WRITTEN TO TRUE
           ELSE
               MOVE 'UNABLE TO QUEUE CONTROLLED CHANGE' TO WS-MESSAGE
               SET WS-INPUT-ERROR TO TRUE
           END-IF.

      *----------------------------------------------------------------*
      * RISK OVERRIDE - MANAGERS AND COMPLIANCE ONLY. A DOWNGRADE      *
      * FROM H OR P IS COMPLIANCE ONLY. REVIEW DATE IS RESET FROM THE  *
      * OVERRIDE RATING.                                               *
      *----------------------------------------------------------------*
       3400-RISK-OVERRIDE.
           IF WS-USERID(1:3) NOT = 'MGR' AND NOT = 'CMP'
               MOVE 'RISK OVERRIDE REQUIRES MANAGER OR COMPLIANCE'
                 TO WS-MESSAGE
               SET WS-INPUT-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           IF (CUST-RISK-HIGH OR CUST-RISK-PROHIBITED)
              AND (RISKOVRI = 'L' OR RISKOVRI = 'M')
              AND WS-USERID(1:3) NOT = 'CMP'
               MOVE 'DOWNGRADE FROM H/P REQUIRES COMPLIANCE'
                 TO WS-MESSAGE
               SET WS-INPUT-ERROR TO TRUE
               EXIT PARAGRAPH
           END-IF
           MOVE CUST-RISK-RATING TO WS-OLD-RISK
           MOVE RISKOVRI TO CUST-RISK-RATING
           SET WS-OVR-IDX TO 1
           SEARCH WS-OVR-ENTRY
               AT END CONTINUE
               WHEN WS-OVR-RATING(WS-OVR-IDX) = CUST-RISK-RATING
                   INITIALIZE DATE-PARMS
                   SET DTU-ADD-MONTHS TO TRUE
                   MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
                   MOVE WS-OVR-MONTHS(WS-OVR-IDX) TO DTU-NUMBER
                   CALL 'DATEUTIL' USING DATE-PARMS
                   MOVE DTU-RESULT-DATE TO CUST-KYC-REVIEW-DATE
           END-SEARCH
           IF CUST-RISK-HIGH OR CUST-RISK-PROHIBITED
               IF CUST-KYC-VERIFIED
                   SET CUST-KYC-EDD-REQUIRED TO TRUE
               END-IF
               MOVE '3' TO CUST-KYC-TIER
           END-IF
           SET WS-CHANGES-APPLIED TO TRUE
           STRING 'RISK ' WS-OLD-RISK '->' CUST-RISK-RATING
                  ' (' FUNCTION TRIM(RSKRSNI) ') REVIEW '
                  CUST-KYC-REVIEW-DATE
                  DELIMITED BY SIZE INTO WS-RESULT.

      *----------------------------------------------------------------*
      * COUNTRY CHANGE - RE-TIER. A MOVE TO A HIGH-RISK JURISDICTION   *
      * ESCALATES TO TIER 3 / EDD WITH AN IMMEDIATE REVIEW DATE.       *
      *----------------------------------------------------------------*
       3500-COUNTRY-RETIER.
           SET CIF-HRC-IDX TO 1
           SEARCH CIF-HR-CTRY
               AT END CONTINUE
               WHEN CIF-HR-CTRY(CIF-HRC-IDX) = CUST-COUNTRY
                   SET WS-NEW-COUNTRY-HIGH-RISK TO TRUE
           END-SEARCH
           INITIALIZE KYCTIER-PARMS
           MOVE CUST-TYPE          TO KYT-CUST-TYPE
           MOVE CUST-PEP-FLAG      TO KYT-PEP-FLAG
           MOVE CUST-MSB-FLAG      TO KYT-MSB-FLAG
           MOVE CUST-RISK-RATING   TO KYT-RISK-RATING
           MOVE CUST-COUNTRY       TO KYT-COUNTRY
           MOVE CUST-SEGMENT       TO KYT-SEGMENT
           MOVE CUST-ANNUAL-INCOME TO KYT-ANNUAL-INCOME
           MOVE CUST-INDUSTRY-CODE TO KYT-INDUSTRY-CODE
           MOVE CUST-OFAC-STATUS   TO KYT-OFAC-STATUS
           MOVE CTL-BUSINESS-DATE  TO KYT-AS-OF-DATE
           CALL 'KYCTIER' USING KYCTIER-PARMS
           IF KYT-RETURN-CODE NOT = 00 AND NOT = 08
               MOVE 'KYC TIER COULD NOT BE RECOMPUTED' TO WS-RESULT
               EXIT PARAGRAPH
           END-IF
           IF KYT-RESULT-TIER > CUST-KYC-TIER
               MOVE KYT-RESULT-TIER TO CUST-KYC-TIER
           END-IF
           IF KYT-EDD-IS-REQUIRED AND CUST-KYC-VERIFIED
               SET CUST-KYC-EDD-REQUIRED TO TRUE
           END-IF
           IF WS-NEW-COUNTRY-HIGH-RISK
      *        EDD MUST BE COMPLETED BEFORE THE NEXT TRANSACTION CYCLE
               MOVE CTL-BUSINESS-DATE TO CUST-KYC-REVIEW-DATE
               IF CUST-RISK-LOW OR CUST-RISK-MEDIUM
                   SET CUST-RISK-HIGH TO TRUE
               END-IF
               STRING 'HIGH-RISK COUNTRY ' CUST-COUNTRY
                      ' - TIER 3 / EDD REQUIRED, REASON '
                      KYT-REASON-CODE
                      DELIMITED BY SIZE INTO WS-RESULT
           ELSE
               STRING 'COUNTRY CHANGED - KYC TIER ' CUST-KYC-TIER
                      ' REASON ' KYT-REASON-CODE
                      DELIMITED BY SIZE INTO WS-RESULT
           END-IF.

      *----------------------------------------------------------------*
      * PF10 - CHECKER APPROVES ALL PENDING CHANGES FOR THE CUSTOMER.  *
      * THE CHECKER MAY NOT BE THE MAKER. NAME / TAX ID CHANGES RESET  *
      * THE OFAC LAST-SCREEN DATE SO OFACBAT RE-SCREENS TONIGHT.       *
      *----------------------------------------------------------------*
       5000-APPROVE-PENDING.
           MOVE CA-CUST-ID TO CUST-ID
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                     RIDFLD(CUST-ID) UPDATE RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CUSTOMER NOT FOUND' TO WS-MESSAGE
               PERFORM 8000-SEND-MAP
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO WS-PND-COUNT
           MOVE 'N'  TO WS-ERROR-FLAG
           MOVE CUST-ID TO WS-PK-CUST
           MOVE ZERO    TO WS-PK-SEQ
           MOVE 'N' TO WS-BROWSE-FLAG
           PERFORM UNTIL WS-BROWSE-DONE
               EXEC CICS READ FILE('CUSTPND')
                         INTO(CUST-PENDING-RECORD)
                         RIDFLD(WS-PND-KEY-WORK) GTEQ UPDATE
                         RESP(WS-RESP)
               END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                  OR PND-CUST-ID NOT = CUST-ID
                   SET WS-BROWSE-DONE TO TRUE
               ELSE
                   MOVE PND-KEY TO WS-PND-KEY-WORK
                   ADD 1 TO WS-PK-SEQ
                   EVALUATE TRUE
                       WHEN NOT PND-AWAITING-CHECKER
                           EXEC CICS UNLOCK FILE('CUSTPND') END-EXEC
                       WHEN PND-EXPIRY-DATE < CTL-BUSINESS-DATE
                           SET PND-EXPIRED TO TRUE
                           PERFORM 5100-REWRITE-PENDING
                       WHEN PND-MAKER-USER = WS-USERID
                           EXEC CICS UNLOCK FILE('CUSTPND') END-EXEC
                           SET WS-INPUT-ERROR TO TRUE
                       WHEN OTHER
                           PERFORM 5200-APPLY-PENDING-FIELD
                           SET PND-APPROVED TO TRUE
                           MOVE WS-USERID TO PND-CHECKER-USER
                           MOVE CTL-BUSINESS-DATE TO PND-CHECKER-DATE
                           PERFORM 5100-REWRITE-PENDING
                           ADD 1 TO WS-PND-COUNT
                   END-EVALUATE
               END-IF
           END-PERFORM
           IF WS-PND-COUNT > 0
      *        FORCE OFAC RE-SCREEN OF THE NEW NAME / TAX ID
               MOVE ZERO TO CUST-OFAC-LAST-SCREEN
               MOVE CTL-BUSINESS-DATE TO CUST-LAST-MAINT-DATE
               MOVE WS-USERID         TO CUST-LAST-MAINT-USER
               EXEC CICS REWRITE FILE('CUSTMST')
                         FROM(CUSTOMER-RECORD) RESP(WS-RESP)
               END-EXEC
               MOVE CUST-LAST-MAINT-DATE TO CA-LAST-MAINT-DATE
               MOVE CUST-LAST-MAINT-USER TO CA-LAST-MAINT-USER
               STRING WS-PND-COUNT ' CHANGE(S) APPROVED - OFAC '
                      'RE-SCREEN SCHEDULED'
                      DELIMITED BY SIZE INTO WS-MESSAGE
           ELSE
               EXEC CICS UNLOCK FILE('CUSTMST') END-EXEC
               IF WS-INPUT-ERROR
                   MOVE 'CHECKER CANNOT BE THE MAKER OF THE CHANGE'
                     TO WS-MESSAGE
               ELSE
                   MOVE 'NO PENDING CHANGES TO APPROVE' TO WS-MESSAGE
               END-IF
           END-IF
           PERFORM 2100-FORMAT-SCREEN
           PERFORM 8000-SEND-MAP.

       5100-REWRITE-PENDING.
           EXEC CICS REWRITE FILE('CUSTPND')
                     FROM(CUST-PENDING-RECORD) RESP(WS-RESP)
           END-EXEC.

       5200-APPLY-PENDING-FIELD.
           EVALUATE TRUE
               WHEN PND-FLD-LAST-NAME
                   MOVE PND-NEW-VALUE      TO CUST-LAST-NAME
               WHEN PND-FLD-FIRST-NAME
                   MOVE PND-NEW-VALUE(1:20) TO CUST-FIRST-NAME
               WHEN PND-FLD-MIDDLE-INIT
                   MOVE PND-NEW-VALUE(1:1) TO CUST-MIDDLE-INIT
               WHEN PND-FLD-TAX-ID
                   MOVE PND-NEW-VALUE(1:9) TO CUST-TAX-ID
               WHEN PND-FLD-TAX-ID-TYPE
                   MOVE PND-NEW-VALUE(1:1) TO CUST-TAX-ID-TYPE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PF11 - CHECKER REJECTS ALL PENDING CHANGES                     *
      *----------------------------------------------------------------*
       5500-REJECT-PENDING.
           MOVE ZERO TO WS-PND-COUNT
           MOVE CA-CUST-ID TO WS-PK-CUST CUST-ID
           MOVE ZERO       TO WS-PK-SEQ
           MOVE 'N' TO WS-BROWSE-FLAG
           PERFORM UNTIL WS-BROWSE-DONE
               EXEC CICS READ FILE('CUSTPND')
                         INTO(CUST-PENDING-RECORD)
                         RIDFLD(WS-PND-KEY-WORK) GTEQ UPDATE
                         RESP(WS-RESP)
               END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                  OR PND-CUST-ID NOT = CA-CUST-ID
                   SET WS-BROWSE-DONE TO TRUE
               ELSE
                   MOVE PND-KEY TO WS-PND-KEY-WORK
                   ADD 1 TO WS-PK-SEQ
                   IF PND-AWAITING-CHECKER
                      AND PND-MAKER-USER NOT = WS-USERID
                       SET PND-REJECTED TO TRUE
                       MOVE WS-USERID TO PND-CHECKER-USER
                       MOVE CTL-BUSINESS-DATE TO PND-CHECKER-DATE
                       MOVE 'REJECTED BY CHECKER' TO PND-REJECT-REASON
                       PERFORM 5100-REWRITE-PENDING
                       ADD 1 TO WS-PND-COUNT
                   ELSE
                       EXEC CICS UNLOCK FILE('CUSTPND') END-EXEC
                   END-IF
               END-IF
           END-PERFORM
           STRING WS-PND-COUNT ' PENDING CHANGE(S) REJECTED'
                  DELIMITED BY SIZE INTO WS-MESSAGE
           PERFORM 2000-DISPLAY-CUSTOMER.

      *----------------------------------------------------------------*
       8000-SEND-MAP.
           MOVE WS-MESSAGE TO MSGO
           MOVE 'ENTER=DISPLAY PF5=UPDATE PF10=APPROVE PF11=REJECT'
             TO PFKEYSO
           MOVE ' PF3=MENU' TO PFKEYSO(50:9)
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     MMDDYYYY(WS-DATE-OUT) DATESEP('/')
           END-EXEC
           MOVE WS-DATE-OUT TO CURDATEO
           EXEC CICS SEND MAP('CUMNTM') MAPSET('CUMNTS')
                     FROM(CUMNTMO) ERASE CURSOR
           END-EXEC.
