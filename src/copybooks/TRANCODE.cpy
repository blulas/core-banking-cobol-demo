      *================================================================*
      * COPYBOOK : TRANCODE                                            *
      * PURPOSE  : TRANSACTION CODE TABLE. ATTRIBUTES PER CODE:        *
      *   CODE(4) DESC(20) DRCR(1) CASH(1) REGD(1) CUSTINIT(1) PRIO(1) *
      *   CASH     - Y = PHYSICAL CURRENCY, COUNTS TOWARD CTR          *
      *   REGD     - Y = COUNTS AS A CONVENIENT WITHDRAWAL (REG D)     *
      *   CUSTINIT - Y = CUSTOMER-INITIATED, RESETS DORMANCY CLOCK     *
      *   PRIO     - POSTING PRIORITY, 1 POSTS FIRST                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 2001-09-10 BHS  CASH INDICATOR                                 *
      * 2010-07-01 DPS  REG E - FEE CODES SPLIT BY FEE TYPE            *
      * 2011-04-12 DPS  SWEEP CODES                                    *
      *================================================================*
       01  TRAN-CODE-TABLE-VALUES.
      *--- CREDITS ---------------------------------------------------*
           05  FILLER PIC X(29) VALUE 'DCSHCASH DEPOSIT        CYNY1'.
           05  FILLER PIC X(29) VALUE 'DCHKCHECK DEPOSIT       CNNY1'.
           05  FILLER PIC X(29) VALUE 'DACHACH CREDIT          CNNY1'.
           05  FILLER PIC X(29) VALUE 'DWIRINCOMING WIRE       CNNY1'.
           05  FILLER PIC X(29) VALUE 'DXFRTRANSFER IN         CNNY1'.
           05  FILLER PIC X(29) VALUE 'DINTINTEREST CREDIT     CNNN2'.
           05  FILLER PIC X(29) VALUE 'DREVREVERSAL CREDIT     CNNN1'.
           05  FILLER PIC X(29) VALUE 'DLONLOAN DISBURSEMENT   CNNN1'.
           05  FILLER PIC X(29) VALUE 'DSWPSWEEP IN            CNNN2'.
           05  FILLER PIC X(29) VALUE 'DRFDFEE REFUND          CNNN2'.
           05  FILLER PIC X(29) VALUE 'DCOFCHARGE-OFF CREDIT   CNNN2'.
           05  FILLER PIC X(29) VALUE 'ADJCADJUSTMENT CREDIT   CNNN2'.
           05  FILLER PIC X(29) VALUE 'DMATCD MATURITY CREDIT  CNNN2'.
      *--- DEBITS ----------------------------------------------------*
           05  FILLER PIC X(29) VALUE 'WCSHCASH WITHDRAWAL     DYNY4'.
           05  FILLER PIC X(29) VALUE 'WATMATM WITHDRAWAL      DYNY4'.
           05  FILLER PIC X(29) VALUE 'WCHKCHECK PAID          DNYY6'.
           05  FILLER PIC X(29) VALUE 'WACHACH DEBIT           DNYY5'.
           05  FILLER PIC X(29) VALUE 'WWIROUTGOING WIRE       DNYY3'.
           05  FILLER PIC X(29) VALUE 'WXFRTRANSFER OUT        DNYY5'.
           05  FILLER PIC X(29) VALUE 'WPOSDEBIT CARD PURCHASE DNYY4'.
           05  FILLER PIC X(29) VALUE 'WLNPLOAN PAYMENT        DNNY5'.
           05  FILLER PIC X(29) VALUE 'WSWPSWEEP OUT           DNNN8'.
           05  FILLER PIC X(29) VALUE 'WPENCD EARLY WD PENALTY DNNN8'.
           05  FILLER PIC X(29) VALUE 'WBWHBACKUP WITHHOLDING  DNNN8'.
           05  FILLER PIC X(29) VALUE 'WMATCD MATURITY PAYOUT  DNNN8'.
           05  FILLER PIC X(29) VALUE 'WESCESCHEAT TO STATE    DNNN9'.
           05  FILLER PIC X(29) VALUE 'ADJDADJUSTMENT DEBIT    DNNN7'.
      *--- FEES (REG E 2010 - ONE CODE PER FEE TYPE) ------------------*
           05  FILLER PIC X(29) VALUE 'WFODOVERDRAFT FEE       DNNN9'.
           05  FILLER PIC X(29) VALUE 'WFNSNSF RETURN FEE      DNNN9'.
           05  FILLER PIC X(29) VALUE 'WFMNMONTHLY MAINT FEE   DNNN9'.
           05  FILLER PIC X(29) VALUE 'WFWRWIRE TRANSFER FEE   DNNN9'.
           05  FILLER PIC X(29) VALUE 'WFANACCOUNT ANALYSIS FEEDNNN9'.
           05  FILLER PIC X(29) VALUE 'WFRDREG D EXCESS WD FEE DNNN9'.
           05  FILLER PIC X(29) VALUE 'WFLTLATE PAYMENT FEE    DNNN9'.
           05  FILLER PIC X(29) VALUE 'WFOTOTHER SERVICE FEE   DNNN9'.
       01  TRAN-CODE-TABLE REDEFINES TRAN-CODE-TABLE-VALUES.
           05  TC-ENTRY OCCURS 35 TIMES INDEXED BY TC-IDX.
               10  TC-CODE                 PIC X(04).
               10  TC-DESC                 PIC X(20).
               10  TC-DR-CR                PIC X(01).
               10  TC-CASH-FLAG            PIC X(01).
               10  TC-REGD-FLAG            PIC X(01).
               10  TC-CUST-INIT-FLAG       PIC X(01).
               10  TC-POST-PRIORITY        PIC 9(01).
       01  TRAN-CODE-TABLE-COUNT           PIC 9(03) VALUE 35.
