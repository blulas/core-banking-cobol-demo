      *================================================================*
      * PROGRAM  : GLINQ                                               *
      * TYPE     : CICS ONLINE - TRANSACTION CB52                      *
      * PURPOSE  : GENERAL LEDGER BALANCE INQUIRY BY GL ACCOUNT AND    *
      *            COST CENTER (GLBAL KSDS). SHOWS OPENING, PERIOD     *
      *            DEBITS/CREDITS, CLOSING AND TODAY'S MOVEMENT, PLUS  *
      *            THE CLOSING BALANCE OF THE GL ACCOUNT ACROSS ALL    *
      *            COST CENTERS. PF7/PF8 STEP THROUGH COST CENTERS OF  *
      *            THE SAME GL ACCOUNT. GL ACCOUNT MUST BE ON THE      *
      *            CHART OF ACCOUNTS (GLCHART).                        *
      *            BALANCES ARE SHOWN IN THE ACCOUNT'S NORMAL          *
      *            DIRECTION (SEE GLBALREC).                           *
      * MAPSET   : GLINQAS / GLINQAM                                   *
      * FILES    : GLBAL (READ / BROWSE)                               *
      * COMMAREA : GL ACCOUNT, COST CENTER                             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2006-02-14 BHS  ORIGINAL - ORACLE GL COST CENTERS              *
      * 2009-08-30 RJT  INC12207 BLANK COST CENTER = FIRST ON FILE     *
      * 2015-10-02 DPS  TODAY DEBIT/CREDIT FIELDS                      *
      * 2018-05-14 AKW  ALL COST CENTER TOTAL AND COUNT                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GLINQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANSID                      PIC X(04) VALUE 'CB52'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-ABSTIME                      PIC S9(15) COMP-3.
       01  WS-WORK.
           05  WS-BROWSE-KEY.
               10  WS-BK-GL-ACCOUNT        PIC X(10).
               10  WS-BK-COST-CENTER       PIC X(05).
           05  WS-DIRECTION                PIC X(01).
               88  WS-DIR-FIRST                      VALUE 'F'.
               88  WS-DIR-NEXT                       VALUE 'N'.
               88  WS-DIR-PREV                       VALUE 'P'.
           05  WS-FOUND-FLAG               PIC X(01).
               88  WS-FOUND                          VALUE 'Y'.
           05  WS-END-BROWSE               PIC X(01).
               88  WS-BROWSE-DONE                    VALUE 'Y'.
           05  WS-ALL-CC-TOTAL             PIC S9(15)V99 COMP-3.
           05  WS-CC-COUNT                 PIC 9(03).
           05  WS-CHART-FLAG               PIC X(01).
               88  WS-ON-CHART                       VALUE 'Y'.
       01  WS-AMT-ED                       PIC -(15)9.99.
       01  WS-CNT-ED                       PIC ZZ9.
       01  WS-DATE-DISP.
           05  WS-DD-CCYY                  PIC 9(04).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  WS-DD-MM                    PIC 9(02).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  WS-DD-DD                    PIC 9(02).
       01  WS-DATE-9                       PIC 9(08).
       01  WS-DATE-X REDEFINES WS-DATE-9.
           05  WS-DX-CCYY                  PIC 9(04).
           05  WS-DX-MM                    PIC 9(02).
           05  WS-DX-DD                    PIC 9(02).
       01  WS-MESSAGE                      PIC X(79).

       01  WS-COMMAREA.
           05  CA-GL-ACCOUNT               PIC X(10).
           05  CA-COST-CENTER              PIC X(05).

       COPY GLINQAS.
       COPY GLBALREC.
       COPY GLCHART.
       COPY DFHAID.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(15).

       PROCEDURE DIVISION.
       0000-MAIN.
           IF EIBCALEN = 0
               MOVE SPACES     TO WS-COMMAREA
               MOVE LOW-VALUES TO GLINQAMO
               MOVE 'ENTER GL ACCOUNT (COST CENTER OPTIONAL)'
                 TO WS-MESSAGE
               PERFORM 8000-SEND-MAP-ERASE
               PERFORM 9000-RETURN
           END-IF
           MOVE DFHCOMMAREA TO WS-COMMAREA

           EVALUATE EIBAID
               WHEN DFHPF3
                   EXEC CICS XCTL PROGRAM('CBMENU')
                             RESP(WS-RESP)
                   END-EXEC
               WHEN DFHPF12
               WHEN DFHCLEAR
                   MOVE SPACES     TO WS-COMMAREA
                   MOVE LOW-VALUES TO GLINQAMO
                   MOVE 'CANCELLED' TO WS-MESSAGE
                   PERFORM 8000-SEND-MAP-ERASE
               WHEN DFHENTER
                   PERFORM 1000-RECEIVE-MAP
                   SET WS-DIR-FIRST TO TRUE
                   PERFORM 2000-INQUIRE
               WHEN DFHPF8
                   SET WS-DIR-NEXT TO TRUE
                   PERFORM 2000-INQUIRE
               WHEN DFHPF7
                   SET WS-DIR-PREV TO TRUE
                   PERFORM 2000-INQUIRE
               WHEN OTHER
                   MOVE 'INVALID KEY PRESSED' TO WS-MESSAGE
                   PERFORM 8100-SEND-DATAONLY
           END-EVALUATE
           PERFORM 9000-RETURN.

       1000-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP('GLINQAM') MAPSET('GLINQAS')
                     INTO(GLINQAMI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO GLINQAMI
           END-IF
           INSPECT GLINQAMI REPLACING ALL LOW-VALUES BY SPACES
           MOVE GLACCTI TO CA-GL-ACCOUNT
           MOVE GLCCI   TO CA-COST-CENTER.

      *----------------------------------------------------------------*
      * LOCATE THE GLBAL RECORD                                        *
      *----------------------------------------------------------------*
       2000-INQUIRE.
           MOVE LOW-VALUES TO GLINQAMO
           MOVE SPACES     TO WS-MESSAGE
           IF CA-GL-ACCOUNT = SPACES
               MOVE 'GL ACCOUNT IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO GLACCTL
               PERFORM 8000-SEND-MAP-ERASE
               PERFORM 9000-RETURN
           END-IF
           PERFORM 2100-CHECK-CHART
           IF NOT WS-ON-CHART
               MOVE 'GL ACCOUNT NOT ON CHART OF ACCOUNTS' TO WS-MESSAGE
               MOVE CA-GL-ACCOUNT TO GLACCTO
               MOVE -1 TO GLACCTL
               PERFORM 8000-SEND-MAP-ERASE
               PERFORM 9000-RETURN
           END-IF

           MOVE 'N' TO WS-FOUND-FLAG
           EVALUATE TRUE
      *        EXACT KEY, OR INC12207 FIRST COST CENTER ON FILE
               WHEN WS-DIR-FIRST AND CA-COST-CENTER NOT = SPACES
                   MOVE CA-GL-ACCOUNT  TO GLB-GL-ACCOUNT
                   MOVE CA-COST-CENTER TO GLB-COST-CENTER
                   EXEC CICS READ FILE('GLBAL')
                             INTO(GL-BALANCE-RECORD)
                             RIDFLD(GLB-KEY) RESP(WS-RESP)
                   END-EXEC
                   IF WS-RESP = DFHRESP(NORMAL)
                       SET WS-FOUND TO TRUE
                   END-IF
               WHEN WS-DIR-FIRST
                   MOVE CA-GL-ACCOUNT TO WS-BK-GL-ACCOUNT
                   MOVE LOW-VALUES    TO WS-BK-COST-CENTER
                   PERFORM 2200-BROWSE-NEXT
               WHEN WS-DIR-NEXT
                   MOVE CA-GL-ACCOUNT  TO WS-BK-GL-ACCOUNT
                   MOVE CA-COST-CENTER TO WS-BK-COST-CENTER
                   PERFORM 2200-BROWSE-NEXT
               WHEN WS-DIR-PREV
                   PERFORM 2300-BROWSE-PREV
           END-EVALUATE

           IF NOT WS-FOUND
               EVALUATE TRUE
                   WHEN WS-DIR-NEXT
                       MOVE 'NO MORE COST CENTERS FOR THIS GL ACCOUNT'
                         TO WS-MESSAGE
                   WHEN WS-DIR-PREV
                       MOVE 'FIRST COST CENTER FOR THIS GL ACCOUNT'
                         TO WS-MESSAGE
                   WHEN OTHER
                       MOVE 'NO BALANCE ON FILE FOR GL / COST CENTER'
                         TO WS-MESSAGE
               END-EVALUATE
               IF WS-DIR-FIRST
                   MOVE CA-GL-ACCOUNT  TO GLACCTO
                   MOVE CA-COST-CENTER TO GLCCO
                   PERFORM 8000-SEND-MAP-ERASE
                   PERFORM 9000-RETURN
               END-IF
      *        STAY ON THE CURRENT COST CENTER
               MOVE CA-GL-ACCOUNT  TO GLB-GL-ACCOUNT
               MOVE CA-COST-CENTER TO GLB-COST-CENTER
               EXEC CICS READ FILE('GLBAL') INTO(GL-BALANCE-RECORD)
                         RIDFLD(GLB-KEY) RESP(WS-RESP)
               END-EXEC
           END-IF
           MOVE GLB-COST-CENTER TO CA-COST-CENTER
           PERFORM 3000-ALL-COST-CENTERS
           PERFORM 4000-FORMAT-SCREEN
           PERFORM 8000-SEND-MAP-ERASE.

       2100-CHECK-CHART.
           MOVE 'N' TO WS-CHART-FLAG
           SET GLC-IDX TO 1
           SEARCH GLC-ENTRY
               AT END
                   CONTINUE
               WHEN GLC-GL-ACCOUNT(GLC-IDX) = CA-GL-ACCOUNT
                   MOVE 'Y' TO WS-CHART-FLAG
           END-SEARCH.

      *    NEXT KEY AFTER WS-BROWSE-KEY FOR THE SAME GL ACCOUNT
       2200-BROWSE-NEXT.
           EXEC CICS STARTBR FILE('GLBAL') RIDFLD(WS-BROWSE-KEY)
                     GTEQ RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-END-BROWSE
           PERFORM UNTIL WS-BROWSE-DONE OR WS-FOUND
               EXEC CICS READNEXT FILE('GLBAL')
                         INTO(GL-BALANCE-RECORD)
                         RIDFLD(WS-BROWSE-KEY) RESP(WS-RESP)
               END-EXEC
               EVALUATE TRUE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                       SET WS-BROWSE-DONE TO TRUE
                   WHEN GLB-GL-ACCOUNT NOT = CA-GL-ACCOUNT
                       SET WS-BROWSE-DONE TO TRUE
                   WHEN WS-DIR-NEXT
                    AND GLB-COST-CENTER NOT > CA-COST-CENTER
                       CONTINUE
                   WHEN OTHER
                       SET WS-FOUND TO TRUE
               END-EVALUATE
           END-PERFORM
           EXEC CICS ENDBR FILE('GLBAL') RESP(WS-RESP) END-EXEC.

       2300-BROWSE-PREV.
           MOVE CA-GL-ACCOUNT  TO WS-BK-GL-ACCOUNT
           MOVE CA-COST-CENTER TO WS-BK-COST-CENTER
           EXEC CICS STARTBR FILE('GLBAL') RIDFLD(WS-BROWSE-KEY)
                     GTEQ RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-END-BROWSE
           PERFORM UNTIL WS-BROWSE-DONE OR WS-FOUND
               EXEC CICS READPREV FILE('GLBAL')
                         INTO(GL-BALANCE-RECORD)
                         RIDFLD(WS-BROWSE-KEY) RESP(WS-RESP)
               END-EXEC
               EVALUATE TRUE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                       SET WS-BROWSE-DONE TO TRUE
                   WHEN GLB-GL-ACCOUNT NOT = CA-GL-ACCOUNT
                       SET WS-BROWSE-DONE TO TRUE
                   WHEN GLB-COST-CENTER NOT < CA-COST-CENTER
                       CONTINUE
                   WHEN OTHER
                       SET WS-FOUND TO TRUE
               END-EVALUATE
           END-PERFORM
           EXEC CICS ENDBR FILE('GLBAL') RESP(WS-RESP) END-EXEC.

      *----------------------------------------------------------------*
      * TOTAL CLOSING BALANCE ACROSS ALL COST CENTERS OF THE GL ACCOUNT*
      *----------------------------------------------------------------*
       3000-ALL-COST-CENTERS.
           MOVE ZERO TO WS-ALL-CC-TOTAL WS-CC-COUNT
           MOVE CA-GL-ACCOUNT TO WS-BK-GL-ACCOUNT
           MOVE LOW-VALUES    TO WS-BK-COST-CENTER
           EXEC CICS STARTBR FILE('GLBAL') RIDFLD(WS-BROWSE-KEY)
                     GTEQ RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-END-BROWSE
           PERFORM UNTIL WS-BROWSE-DONE
               EXEC CICS READNEXT FILE('GLBAL')
                         INTO(GL-BALANCE-RECORD)
                         RIDFLD(WS-BROWSE-KEY) RESP(WS-RESP)
               END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                  OR GLB-GL-ACCOUNT NOT = CA-GL-ACCOUNT
                   SET WS-BROWSE-DONE TO TRUE
               ELSE
                   ADD GLB-CLOSING-BAL TO WS-ALL-CC-TOTAL
                   ADD 1               TO WS-CC-COUNT
               END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE('GLBAL') RESP(WS-RESP) END-EXEC
      *    RE-READ THE DISPLAYED RECORD (BROWSE OVERLAID IT)
           MOVE CA-GL-ACCOUNT  TO GLB-GL-ACCOUNT
           MOVE CA-COST-CENTER TO GLB-COST-CENTER
           EXEC CICS READ FILE('GLBAL') INTO(GL-BALANCE-RECORD)
                     RIDFLD(GLB-KEY) RESP(WS-RESP)
           END-EXEC.

       4000-FORMAT-SCREEN.
           MOVE GLB-GL-ACCOUNT     TO GLACCTO
           MOVE GLB-COST-CENTER    TO GLCCO
           MOVE GLB-DESCRIPTION    TO GLDESCO
           IF GLB-DESCRIPTION = SPACES
               MOVE GLC-DESCRIPTION(GLC-IDX) TO GLDESCO
           END-IF
           MOVE GLB-ACCT-TYPE      TO GLTYPEO
           MOVE GLB-NORMAL-BAL     TO GLNORMO
           MOVE GLB-PERIOD-START   TO WS-DATE-9
           PERFORM 8300-FORMAT-DATE
           MOVE WS-DATE-DISP       TO PERSTO
           MOVE GLB-OPENING-BAL    TO WS-AMT-ED
           MOVE WS-AMT-ED          TO OPENBLO
           MOVE GLB-PERIOD-DEBITS  TO WS-AMT-ED
           MOVE WS-AMT-ED          TO PERDRO
           MOVE GLB-PERIOD-CREDITS TO WS-AMT-ED
           MOVE WS-AMT-ED          TO PERCRO
           MOVE GLB-CLOSING-BAL    TO WS-AMT-ED
           MOVE WS-AMT-ED          TO CLOSBLO
           MOVE GLB-TODAY-DEBITS   TO WS-AMT-ED
           MOVE WS-AMT-ED          TO TDYDRO
           MOVE GLB-TODAY-CREDITS  TO WS-AMT-ED
           MOVE WS-AMT-ED          TO TDYCRO
           IF GLB-LAST-POST-DATE > 0
               MOVE GLB-LAST-POST-DATE TO WS-DATE-9
               PERFORM 8300-FORMAT-DATE
               MOVE WS-DATE-DISP       TO LSTPSTO
           END-IF
           MOVE WS-ALL-CC-TOTAL    TO WS-AMT-ED
           MOVE WS-AMT-ED          TO ALLCCO
           MOVE WS-CC-COUNT        TO WS-CNT-ED
           MOVE WS-CNT-ED          TO CCCNTO
      *    SUSPENSE SHOULD BE CLEARED EVERY DAY BY GL OPERATIONS
           IF GLB-GL-ACCOUNT = '2999' AND GLB-CLOSING-BAL NOT = 0
               MOVE 'SUSPENSE NOT CLEAR - REFER TO GLRECON REPORT'
                 TO WS-MESSAGE
           END-IF
           IF WS-MESSAGE = SPACES
               IF WS-CC-COUNT > 1
                   MOVE 'PF7/PF8 FOR OTHER COST CENTERS' TO WS-MESSAGE
               ELSE
                   MOVE 'INQUIRY COMPLETE' TO WS-MESSAGE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       8000-SEND-MAP-ERASE.
           PERFORM 8200-COMMON-FIELDS
           EXEC CICS SEND MAP('GLINQAM') MAPSET('GLINQAS')
                     FROM(GLINQAMO) ERASE CURSOR RESP(WS-RESP)
           END-EXEC.

       8100-SEND-DATAONLY.
           PERFORM 8200-COMMON-FIELDS
           EXEC CICS SEND MAP('GLINQAM') MAPSET('GLINQAS')
                     FROM(GLINQAMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.

       8200-COMMON-FIELDS.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-9) RESP(WS-RESP)
           END-EXEC
           PERFORM 8300-FORMAT-DATE
           MOVE WS-DATE-DISP TO CURDATEO
           MOVE WS-MESSAGE   TO MSGO.

       8300-FORMAT-DATE.
           MOVE WS-DX-CCYY TO WS-DD-CCYY
           MOVE WS-DX-MM   TO WS-DD-MM
           MOVE WS-DX-DD   TO WS-DD-DD.

       9000-RETURN.
           EXEC CICS RETURN TRANSID(WS-TRANSID)
                     COMMAREA(WS-COMMAREA) LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.
