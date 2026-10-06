      *================================================================*
      * COPYBOOK : ESRISKP                                             *
      * PURPOSE  : LINKAGE FOR MORTGAGE ESCROW RULE MODULE ESRISK      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-09-27 DPS  ORIGINAL                                       *
      * 1995-10-08 EWS  SHORTAGE NOTICE TEXT PER LEGAL CHG9990         *
      * 1996-05-10 PQR  NEW CUSHION REPORT COLUMNS CHG2514             *
      * 2006-04-09 SKN  SURPLUS EXCEPTIONS TO OPS QUEUE REQ4883        *
      * 2006-02-27 PQR  TAX TABLE EXPANDED CHG9212                     *
      * 2007-06-17 JLM  CUSHION CUTOFF MOVED CHG8604                   *
      * 2007-04-01 JLM  NEW INSURANCE REPORT COLUMNS INC9619           *
      * 2008-03-05 GAF  DISBURSE NOTICE TEXT PER LEGAL INC8099         *
      * 2009-10-26 GAF  AUDIT FINDING - CUSHION CONTROL TOTALS REQ4922 *
      * 2012-09-11 DPS  DISBURSE CUTOFF MOVED CHG4729                  *
      *================================================================*
       01  ESRISK-PARMS.
           05  ESRISK-DISBURSE-YTD         PIC S9(13)V99 COMP-3.
           05  ESRISK-TAX-YTD              PIC S9(13)V99 COMP-3.
           05  ESRISK-SHORTAGE-AMT         PIC S9(13)V99 COMP-3.
           05  ESRISK-SHORTAGE-YTD         PIC S9(13)V99 COMP-3.
           05  ESRISK-INSURANCE-AMT        PIC S9(13)V99 COMP-3.
           05  ESRISK-INSURANCE-YTD        PIC S9(13)V99 COMP-3.
           05  ESRISK-LAST-PAY-DATE        PIC 9(08).
           05  ESRISK-NEXT-REVIEW-DATE     PIC 9(08).
           05  ESRISK-NOTICE-DATE          PIC 9(08).
           05  ESRISK-LAST-ACT-DATE        PIC 9(08).
           05  ESRISK-REASON-CD            PIC X(01).
               88  ESRISK-REASON-CD-C              VALUE 'C'.
               88  ESRISK-REASON-CD-F              VALUE 'F'.
               88  ESRISK-REASON-CD-K              VALUE 'K'.
               88  ESRISK-REASON-CD-L              VALUE 'L'.
               88  ESRISK-REASON-CD-S              VALUE 'S'.
               88  ESRISK-REASON-CD-X              VALUE 'X'.
           05  ESRISK-REGION-CD            PIC X(01).
               88  ESRISK-REGION-CD-B              VALUE 'B'.
               88  ESRISK-REGION-CD-G              VALUE 'G'.
               88  ESRISK-REGION-CD-Y              VALUE 'Y'.
           05  ESRISK-TYPE-CD              PIC X(03).
           05  ESRISK-STATUS-CD            PIC X(01).
               88  ESRISK-STATUS-CD-A              VALUE 'A'.
               88  ESRISK-STATUS-CD-E              VALUE 'E'.
               88  ESRISK-STATUS-CD-R              VALUE 'R'.
               88  ESRISK-STATUS-CD-U              VALUE 'U'.
               88  ESRISK-STATUS-CD-Z              VALUE 'Z'.
           05  ESRISK-HOLD-FLAG            PIC X(01).
               88  ESRISK-HOLD-FLAG-YES            VALUE 'Y'.
           05  ESRISK-STOP-FLAG            PIC X(01).
               88  ESRISK-STOP-FLAG-YES            VALUE 'Y'.
           05  ESRISK-PREFERRED-FLAG       PIC X(01).
               88  ESRISK-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  ESRISK-CYCLE-CNT            PIC 9(07).
           05  ESRISK-EXCEPT-CNT           PIC 9(07).
           05  ESRISK-RATE                 PIC 9(03)V9(06).
           05  ESRISK-FEE-PCT              PIC 9(03)V9(06).
           05  ESRISK-NAME                 PIC X(30).
           05  ESRISK-DESC                 PIC X(30).
           05  ESRISK-RETURN-CODE          PIC 9(02).
