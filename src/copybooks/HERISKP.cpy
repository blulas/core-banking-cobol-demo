      *================================================================*
      * COPYBOOK : HERISKP                                             *
      * PURPOSE  : LINKAGE FOR HOME EQUITY LINES RULE MODULE HERISK    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-12-26 TWH  ORIGINAL                                       *
      * 1992-07-10 JLM  APPRAISE STATUS CODES ADDED INC5733            *
      * 1993-05-11 EWS  MIN-PAY NOTICE TEXT PER LEGAL REQ3714          *
      * 1995-09-22 BHS  INTEREST RERUN PROTECTION REQ6529              *
      * 1995-11-24 GAF  INTEREST ROUNDING - HALF UP INC6066            *
      * 1998-03-15 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-12-17 GAF  DRAW LIMIT REVISED INC1853                     *
      * 2001-05-24 JLM  PERFORMANCE - BALANCE READ AHEAD REQ4787       *
      * 2004-11-19 RJT  MIN-PAY RERUN PROTECTION INC1719               *
      * 2006-06-05 DPS  BALANCE NOTICE TEXT PER LEGAL INC1185          *
      *================================================================*
       01  HERISK-PARMS.
           05  HERISK-INTEREST-AMT         PIC S9(13)V99 COMP-3.
           05  HERISK-MIN-PAY-AMT          PIC S9(13)V99 COMP-3.
           05  HERISK-BALANCE-YTD          PIC S9(13)V99 COMP-3.
           05  HERISK-ANNUAL-FEE-AMT       PIC S9(13)V99 COMP-3.
           05  HERISK-MIN-PAY-YTD          PIC S9(13)V99 COMP-3.
           05  HERISK-APPRAISE-AMT         PIC S9(13)V99 COMP-3.
           05  HERISK-EFFECTIVE-DATE       PIC 9(08).
           05  HERISK-LAST-MAINT-DATE      PIC 9(08).
           05  HERISK-OPEN-DATE            PIC 9(08).
           05  HERISK-MATURITY-DATE        PIC 9(08).
           05  HERISK-REASON-CD            PIC X(03).
               88  HERISK-REASON-CD-KST            VALUE 'KST'.
               88  HERISK-REASON-CD-NWS            VALUE 'NWS'.
               88  HERISK-REASON-CD-ZJM            VALUE 'ZJM'.
           05  HERISK-TYPE-CD              PIC X(02).
               88  HERISK-TYPE-CD-CD               VALUE 'CD'.
               88  HERISK-TYPE-CD-JW               VALUE 'JW'.
               88  HERISK-TYPE-CD-KV               VALUE 'KV'.
               88  HERISK-TYPE-CD-WS               VALUE 'WS'.
               88  HERISK-TYPE-CD-XL               VALUE 'XL'.
               88  HERISK-TYPE-CD-ZV               VALUE 'ZV'.
           05  HERISK-STATUS-CD            PIC X(02).
               88  HERISK-STATUS-CD-GR             VALUE 'GR'.
               88  HERISK-STATUS-CD-JC             VALUE 'JC'.
               88  HERISK-STATUS-CD-KH             VALUE 'KH'.
               88  HERISK-STATUS-CD-KN             VALUE 'KN'.
               88  HERISK-STATUS-CD-PC             VALUE 'PC'.
               88  HERISK-STATUS-CD-UZ             VALUE 'UZ'.
           05  HERISK-DRAW-PERIOD-CD       PIC X(03).
               88  HERISK-DRAW-PERIOD-CD-CCV       VALUE 'CCV'.
               88  HERISK-DRAW-PERIOD-CD-JSU       VALUE 'JSU'.
               88  HERISK-DRAW-PERIOD-CD-KXA       VALUE 'KXA'.
               88  HERISK-DRAW-PERIOD-CD-VMR       VALUE 'VMR'.
               88  HERISK-DRAW-PERIOD-CD-WLJ       VALUE 'WLJ'.
               88  HERISK-DRAW-PERIOD-CD-XRA       VALUE 'XRA'.
           05  HERISK-HOLD-FLAG            PIC X(01).
               88  HERISK-HOLD-FLAG-YES            VALUE 'Y'.
           05  HERISK-PREFERRED-FLAG       PIC X(01).
               88  HERISK-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  HERISK-SUSPEND-FLAG         PIC X(01).
               88  HERISK-SUSPEND-FLAG-YES         VALUE 'Y'.
           05  HERISK-RETRY-CNT            PIC 9(07).
           05  HERISK-PAYMENT-CNT          PIC 9(07).
           05  HERISK-PENALTY-PCT          PIC 9(03)V9(06).
           05  HERISK-MARGIN               PIC 9(03)V9(06).
           05  HERISK-CONTACT              PIC X(20).
           05  HERISK-DESC                 PIC X(30).
           05  HERISK-RETURN-CODE          PIC 9(02).
