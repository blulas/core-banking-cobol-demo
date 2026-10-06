      *================================================================*
      * COPYBOOK : CLRISKP                                             *
      * PURPOSE  : LINKAGE FOR COLLECTIONS QUEUE RULE MODULE CLRISK    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-01-07 DPS  ORIGINAL                                       *
      * 1995-10-05 DPS  PROMISED RERUN PROTECTION CHG9696              *
      * 1997-02-07 TWH  COLLECTED LIMIT REVISED REQ0321                *
      * 1997-02-21 GAF  COLLECTED LIMIT REVISED REQ6502                *
      * 1999-11-04 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-03-01 JLM  WRITEOFF ROUNDING - HALF UP CHG5458            *
      * 2000-11-13 GAF  PAST-DUE ROUNDING - HALF UP INC9600            *
      * 2001-02-23 EWS  WRITEOFF ROUNDING - HALF UP REQ4623            *
      * 2002-08-08 PQR  WRITEOFF LIMIT REVISED REQ9656                 *
      * 2003-11-15 DPS  FEE NOTICE TEXT PER LEGAL PRB4754              *
      *================================================================*
       01  CLRISK-PARMS.
           05  CLRISK-PAST-DUE-AMT         PIC S9(13)V99 COMP-3.
           05  CLRISK-PAST-DUE-YTD         PIC S9(13)V99 COMP-3.
           05  CLRISK-COLLECTED-AMT        PIC S9(13)V99 COMP-3.
           05  CLRISK-WRITEOFF-YTD         PIC S9(13)V99 COMP-3.
           05  CLRISK-FEE-YTD              PIC S9(13)V99 COMP-3.
           05  CLRISK-PROMISED-YTD         PIC S9(13)V99 COMP-3.
           05  CLRISK-EFFECTIVE-DATE       PIC 9(08).
           05  CLRISK-OPEN-DATE            PIC 9(08).
           05  CLRISK-LAST-MAINT-DATE      PIC 9(08).
           05  CLRISK-NOTICE-DATE          PIC 9(08).
           05  CLRISK-CYCLE-CD             PIC X(02).
           05  CLRISK-REGION-CD            PIC X(02).
               88  CLRISK-REGION-CD-AT             VALUE 'AT'.
               88  CLRISK-REGION-CD-AU             VALUE 'AU'.
               88  CLRISK-REGION-CD-XR             VALUE 'XR'.
               88  CLRISK-REGION-CD-XU             VALUE 'XU'.
           05  CLRISK-ACTION-CD            PIC X(02).
               88  CLRISK-ACTION-CD-KJ             VALUE 'KJ'.
               88  CLRISK-ACTION-CD-TM             VALUE 'TM'.
               88  CLRISK-ACTION-CD-VL             VALUE 'VL'.
               88  CLRISK-ACTION-CD-ZS             VALUE 'ZS'.
           05  CLRISK-STATUS-CD            PIC X(02).
               88  CLRISK-STATUS-CD-PS             VALUE 'PS'.
               88  CLRISK-STATUS-CD-UR             VALUE 'UR'.
               88  CLRISK-STATUS-CD-XM             VALUE 'XM'.
               88  CLRISK-STATUS-CD-YA             VALUE 'YA'.
           05  CLRISK-NOTICE-SENT-FLAG     PIC X(01).
               88  CLRISK-NOTICE-SENT-FLAG-YES     VALUE 'Y'.
           05  CLRISK-REVIEW-FLAG          PIC X(01).
               88  CLRISK-REVIEW-FLAG-YES          VALUE 'Y'.
           05  CLRISK-EXEMPT-FLAG          PIC X(01).
               88  CLRISK-EXEMPT-FLAG-YES          VALUE 'Y'.
           05  CLRISK-ITEM-CNT             PIC 9(07).
           05  CLRISK-RETRY-CNT            PIC 9(07).
           05  CLRISK-FEE-PCT              PIC 9(03)V9(06).
           05  CLRISK-MARGIN               PIC 9(03)V9(06).
           05  CLRISK-DESC                 PIC X(30).
           05  CLRISK-CONTACT              PIC X(20).
           05  CLRISK-RETURN-CODE          PIC 9(02).
