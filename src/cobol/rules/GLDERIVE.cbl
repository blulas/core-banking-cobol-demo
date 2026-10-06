      *================================================================*
      * PROGRAM  : GLDERIVE                                            *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DERIVES THE GENERAL LEDGER ACCOUNTS FOR A POSTED    *
      *            TRANSACTION.                                        *
      *            CUSTOMER SIDE - THE DEPOSIT LIABILITY GL OF THE     *
      *              ACCOUNT (FROM CATEGORY + OWNERSHIP)               *
      *            OFFSET SIDE   - FROM THE TRANSACTION CODE (CASH,    *
      *              SETTLEMENT, CLEARING, FEE INCOME, INTEREST        *
      *              EXPENSE, PAYABLES) OR THE POSTING PROGRAM'S       *
      *              OVERRIDE GL (TRAN-GL-ACCOUNT)                     *
      *            A CUSTOMER DEBIT  = DR CUSTOMER GL / CR OFFSET GL   *
      *            A CUSTOMER CREDIT = DR OFFSET GL   / CR CUSTOMER GL *
      *            ANYTHING THAT CANNOT BE MAPPED GOES TO SUSPENSE     *
      *            2999 WITH THE SUSPENSE FLAG SET (RC 04).            *
      * CALLED BY: GLEXTR (FUNCTION T), GLRECON (FUNCTION C)           *
      * LINKAGE  : GLDPARM                                             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1997-03-04 RJT  CHG0952 BOOK TRANSFERS (DXFR/WXFR) VIA 1190    *
      * 2001-10-01 BHS  ACH 1150 / FEDWIRE 1160 SETTLEMENT SPLIT       *
      * 2004-06-14 BHS  CARD NETWORK SETTLEMENT 1170 (WPOS)            *
      * 2010-07-01 DPS  REG E - FEE INCOME BY FEE TYPE 4110-4190       *
      * 2011-04-12 DPS  CHG1544 DDA SPLIT 2010 CONSUMER/2020 CORPORATE *
      *                 SWEEP CODES DSWP/WSWP VIA 1190                 *
      * 2013-06-11 DPS  OVERRIDE GL FROM POSTING PROGRAM (POOLCALC)    *
      * 2014-01-06 DPS  ESCHEAT 2310, BACKUP WITHHOLDING 2320          *
      * 2015-10-02 DPS  SUSPENSE FLAG / REASON CODE                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GLDERIVE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CONSTANTS.
           05  WS-SUSPENSE-GL              PIC X(10) VALUE '2999'.
      *    CENTRAL TREASURY / FINANCE COST CENTER FOR BALANCE SHEET
      *    OFFSETS (SETTLEMENT, CLEARING, PAYABLES)
           05  WS-CENTRAL-CC               PIC X(05) VALUE '90000'.
       01  WS-WORK.
           05  WS-OFFSET-FOUND             PIC X(01).
               88  WS-OFFSET-MAPPED                  VALUE 'Y'.
           05  WS-GL-FIRST-DIGIT           PIC X(01).
       COPY GLCHART.

       LINKAGE SECTION.
       COPY GLDPARM.

       PROCEDURE DIVISION USING GL-DERIVE-PARMS.
       0000-MAIN.
           MOVE SPACES TO GLD-CUST-GL GLD-OFFSET-GL GLD-OFFSET-CC
                          GLD-DEBIT-GL GLD-DEBIT-CC
                          GLD-CREDIT-GL GLD-CREDIT-CC GLD-REASON-CODE
           MOVE 'N'    TO GLD-SUSPENSE-FLAG
           MOVE ZERO   TO GLD-RETURN-CODE

           EVALUATE TRUE
               WHEN GLD-FN-CATEGORY
                   PERFORM 2150-CATEGORY-TO-GL
               WHEN GLD-FN-TRANSACTION
                   IF GLD-DR-CR NOT = 'D' AND GLD-DR-CR NOT = 'C'
                       MOVE 12     TO GLD-RETURN-CODE
                       MOVE 'EDRC' TO GLD-REASON-CODE
                   ELSE
                       PERFORM 2100-DERIVE-CUSTOMER-GL
                       PERFORM 3000-DERIVE-OFFSET-GL
                       PERFORM 4000-BUILD-ENTRY
                   END-IF
               WHEN OTHER
                   MOVE 12     TO GLD-RETURN-CODE
                   MOVE 'EFUN' TO GLD-REASON-CODE
           END-EVALUATE
           GOBACK.

      *----------------------------------------------------------------*
      * CUSTOMER SIDE                                                  *
      *----------------------------------------------------------------*
       2100-DERIVE-CUSTOMER-GL.
      *    CHG0952 - BOOK TRANSFER CREDITS TO CHECKING POST TO THE
      *    DDA LIABILITY CONTROL ACCOUNT
           IF GLD-TRAN-CODE = 'DXFR' AND GLD-ACCT-CATEGORY = 'DD'
               MOVE '2010' TO GLD-CUST-GL
           ELSE
               PERFORM 2150-CATEGORY-TO-GL
           END-IF.

      *    DEPOSIT LIABILITY GL BY ACCOUNT CATEGORY (SUB-LEDGER MAP)
       2150-CATEGORY-TO-GL.
           EVALUATE GLD-ACCT-CATEGORY
               WHEN 'DD'
      *            CHG1544 - CORPORATE DDA SPLIT OUT OF 2010
                   IF GLD-OWNERSHIP-TYPE = 'C'
                       MOVE '2020' TO GLD-CUST-GL
                   ELSE
                       MOVE '2010' TO GLD-CUST-GL
                   END-IF
               WHEN 'SV'
                   MOVE '2110' TO GLD-CUST-GL
               WHEN 'MM'
                   MOVE '2120' TO GLD-CUST-GL
               WHEN 'TD'
                   MOVE '2130' TO GLD-CUST-GL
               WHEN OTHER
                   MOVE WS-SUSPENSE-GL TO GLD-CUST-GL
                   MOVE 'Y'    TO GLD-SUSPENSE-FLAG
                   MOVE 04     TO GLD-RETURN-CODE
                   MOVE 'SCAT' TO GLD-REASON-CODE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OFFSET SIDE - BY TRANSACTION CODE                              *
      *----------------------------------------------------------------*
       3000-DERIVE-OFFSET-GL.
           MOVE 'Y' TO WS-OFFSET-FOUND
           EVALUATE GLD-TRAN-CODE
      *        --- CASH -------------------------------------------
               WHEN 'DCSH'
               WHEN 'WCSH'
               WHEN 'WATM'
                   MOVE '1010' TO GLD-OFFSET-GL
      *        --- CHECKS -----------------------------------------
               WHEN 'DCHK'
               WHEN 'WCHK'
                   MOVE '1050' TO GLD-OFFSET-GL
      *        --- ACH / FEDWIRE / CARD SETTLEMENT ----------------
               WHEN 'DACH'
               WHEN 'WACH'
                   MOVE '1150' TO GLD-OFFSET-GL
               WHEN 'DWIR'
               WHEN 'WWIR'
                   MOVE '1160' TO GLD-OFFSET-GL
               WHEN 'WPOS'
                   MOVE '1170' TO GLD-OFFSET-GL
      *        --- INTERNAL MOVEMENTS (BOTH LEGS POST, NET ZERO) --
               WHEN 'DXFR'
               WHEN 'WXFR'
               WHEN 'DSWP'
               WHEN 'WSWP'
               WHEN 'DMAT'
               WHEN 'WMAT'
                   MOVE '1190' TO GLD-OFFSET-GL
      *        --- LOANS ------------------------------------------
               WHEN 'DLON'
               WHEN 'WLNP'
                   MOVE '1400' TO GLD-OFFSET-GL
      *        --- INTEREST EXPENSE BY PRODUCT --------------------
               WHEN 'DINT'
                   PERFORM 3100-INTEREST-EXPENSE-GL
      *        --- FEE INCOME (REG E 2010 - ONE GL PER FEE TYPE) --
               WHEN 'WFOD'
                   MOVE '4110' TO GLD-OFFSET-GL
               WHEN 'WFNS'
                   MOVE '4120' TO GLD-OFFSET-GL
               WHEN 'WFMN'
                   MOVE '4130' TO GLD-OFFSET-GL
               WHEN 'WFWR'
                   MOVE '4140' TO GLD-OFFSET-GL
               WHEN 'WFAN'
                   MOVE '4150' TO GLD-OFFSET-GL
               WHEN 'WFRD'
                   MOVE '4160' TO GLD-OFFSET-GL
               WHEN 'WFLT'
                   MOVE '4170' TO GLD-OFFSET-GL
               WHEN 'WPEN'
                   MOVE '4175' TO GLD-OFFSET-GL
               WHEN 'WFOT'
                   MOVE '4180' TO GLD-OFFSET-GL
               WHEN 'DRFD'
                   MOVE '4190' TO GLD-OFFSET-GL
      *        --- LOSSES / PAYABLES ------------------------------
               WHEN 'DCOF'
                   MOVE '5510' TO GLD-OFFSET-GL
               WHEN 'WESC'
                   MOVE '2310' TO GLD-OFFSET-GL
               WHEN 'WBWH'
                   MOVE '2320' TO GLD-OFFSET-GL
      *        --- ADJUSTMENTS / REVERSALS NEED AN OVERRIDE GL ----
               WHEN 'ADJC'
               WHEN 'ADJD'
               WHEN 'DREV'
                   MOVE 'N' TO WS-OFFSET-FOUND
               WHEN OTHER
                   MOVE 'N' TO WS-OFFSET-FOUND
           END-EVALUATE

      *    2013-06 OVERRIDE FROM THE POSTING PROGRAM WINS, PROVIDED IT
      *    IS ON THE CHART OF ACCOUNTS
           IF GLD-OVERRIDE-GL NOT = SPACES
               SET GLC-IDX TO 1
               SEARCH GLC-ENTRY
                   AT END
                       MOVE 'SOVR' TO GLD-REASON-CODE
                   WHEN GLC-GL-ACCOUNT(GLC-IDX) = GLD-OVERRIDE-GL
                       MOVE GLD-OVERRIDE-GL TO GLD-OFFSET-GL
                       MOVE 'Y' TO WS-OFFSET-FOUND
               END-SEARCH
           END-IF

           IF NOT WS-OFFSET-MAPPED
               MOVE WS-SUSPENSE-GL TO GLD-OFFSET-GL
               MOVE 'Y'  TO GLD-SUSPENSE-FLAG
               MOVE 04   TO GLD-RETURN-CODE
               IF GLD-REASON-CODE = SPACES
                   MOVE 'STRC' TO GLD-REASON-CODE
               END-IF
           END-IF

      *    INCOME AND EXPENSE STAY WITH THE ACCOUNT'S BUSINESS UNIT,
      *    BALANCE SHEET OFFSETS GO TO CENTRAL TREASURY
           MOVE GLD-OFFSET-GL(1:1) TO WS-GL-FIRST-DIGIT
           IF WS-GL-FIRST-DIGIT = '4' OR WS-GL-FIRST-DIGIT = '5'
               MOVE GLD-COST-CENTER TO GLD-OFFSET-CC
           ELSE
               MOVE WS-CENTRAL-CC   TO GLD-OFFSET-CC
           END-IF.

      *    DEPOSIT INTEREST EXPENSE - COMMERCIAL ACCOUNTS (INCLUDING
      *    NOTIONAL POOL CREDIT INTEREST) TO 5050, OTHERWISE BY PRODUCT
       3100-INTEREST-EXPENSE-GL.
           IF GLD-OWNERSHIP-TYPE = 'C'
               MOVE '5050' TO GLD-OFFSET-GL
           ELSE
               EVALUATE GLD-ACCT-CATEGORY
                   WHEN 'DD'
                       MOVE '5010' TO GLD-OFFSET-GL
                   WHEN 'SV'
                       MOVE '5020' TO GLD-OFFSET-GL
                   WHEN 'MM'
                       MOVE '5030' TO GLD-OFFSET-GL
                   WHEN 'TD'
                       MOVE '5040' TO GLD-OFFSET-GL
                   WHEN OTHER
                       MOVE 'N' TO WS-OFFSET-FOUND
               END-EVALUATE
           END-IF.

      *----------------------------------------------------------------*
      * BALANCED ENTRY                                                 *
      *----------------------------------------------------------------*
       4000-BUILD-ENTRY.
           IF GLD-DR-CR = 'D'
               MOVE GLD-CUST-GL     TO GLD-DEBIT-GL
               MOVE GLD-COST-CENTER TO GLD-DEBIT-CC
               MOVE GLD-OFFSET-GL   TO GLD-CREDIT-GL
               MOVE GLD-OFFSET-CC   TO GLD-CREDIT-CC
           ELSE
               MOVE GLD-OFFSET-GL   TO GLD-DEBIT-GL
               MOVE GLD-OFFSET-CC   TO GLD-DEBIT-CC
               MOVE GLD-CUST-GL     TO GLD-CREDIT-GL
               MOVE GLD-COST-CENTER TO GLD-CREDIT-CC
           END-IF.
