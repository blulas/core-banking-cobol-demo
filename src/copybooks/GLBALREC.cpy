      *================================================================*
      * COPYBOOK : GLBALREC                                            *
      * PURPOSE  : GENERAL LEDGER BALANCE BY ACCOUNT AND COST CENTER   *
      * FILE     : GLBAL (KSDS, KEY = GLB-KEY)                         *
      *   BALANCES ARE HELD IN THE ACCOUNT'S NORMAL DIRECTION:         *
      *   NORMAL D : CLOSING = OPENING + DEBITS - CREDITS              *
      *   NORMAL C : CLOSING = OPENING + CREDITS - DEBITS              *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 2006-02-14 BHS  GL ACCOUNT EXPANDED TO X(10)                   *
      * 2015-10-02 DPS  TODAY DEBIT/CREDIT BUCKETS FOR TRIAL BALANCE   *
      *================================================================*
       01  GL-BALANCE-RECORD.
           05  GLB-KEY.
               10  GLB-GL-ACCOUNT          PIC X(10).
               10  GLB-COST-CENTER         PIC X(05).
           05  GLB-DESCRIPTION             PIC X(30).
           05  GLB-ACCT-TYPE               PIC X(01).
               88  GLB-ASSET                         VALUE 'A'.
               88  GLB-LIABILITY                     VALUE 'L'.
               88  GLB-EQUITY                        VALUE 'Q'.
               88  GLB-INCOME                        VALUE 'I'.
               88  GLB-EXPENSE                       VALUE 'E'.
           05  GLB-NORMAL-BAL              PIC X(01).
               88  GLB-DEBIT-NORMAL                  VALUE 'D'.
               88  GLB-CREDIT-NORMAL                 VALUE 'C'.
           05  GLB-PERIOD-START            PIC 9(08).
           05  GLB-OPENING-BAL             PIC S9(15)V99 COMP-3.
           05  GLB-PERIOD-DEBITS           PIC S9(15)V99 COMP-3.
           05  GLB-PERIOD-CREDITS          PIC S9(15)V99 COMP-3.
           05  GLB-CLOSING-BAL             PIC S9(15)V99 COMP-3.
           05  GLB-TODAY-DEBITS            PIC S9(15)V99 COMP-3.
           05  GLB-TODAY-CREDITS           PIC S9(15)V99 COMP-3.
           05  GLB-LAST-POST-DATE          PIC 9(08).
           05  FILLER                      PIC X(30).
