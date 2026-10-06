      *================================================================*
      * COPYBOOK : CLELGP                                              *
      * PURPOSE  : LINKAGE FOR COLLECTIONS QUEUE RULE MODULE CLELG     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-04-11 LMB  ORIGINAL                                       *
      * 1996-04-01 EWS  PROMISED FEE SCHEDULE UPDATE PRB4372           *
      * 1996-11-20 LMB  WRITEOFF EXCEPTIONS TO OPS QUEUE CHG3817       *
      * 1996-12-03 RJT  NEW COLLECTED REPORT COLUMNS INC6852           *
      * 2004-07-07 TWH  WRITEOFF NOTICE TEXT PER LEGAL INC9574         *
      * 2004-11-16 JLM  COLLECTED STATUS CODES ADDED INC6249           *
      * 2004-02-12 CRP  COLLECTED TABLE EXPANDED CHG9201               *
      *================================================================*
       01  CLELG-PARMS.
           05  CLELG-WRITEOFF-AMT          PIC S9(13)V99 COMP-3.
           05  CLELG-COLLECTED-AMT         PIC S9(13)V99 COMP-3.
           05  CLELG-FEE-AMT               PIC S9(13)V99 COMP-3.
           05  CLELG-WRITEOFF-YTD          PIC S9(13)V99 COMP-3.
           05  CLELG-PAST-DUE-AMT          PIC S9(13)V99 COMP-3.
           05  CLELG-FEE-YTD               PIC S9(13)V99 COMP-3.
           05  CLELG-EFFECTIVE-DATE        PIC 9(08).
           05  CLELG-EXPIRY-DATE           PIC 9(08).
           05  CLELG-LAST-MAINT-DATE       PIC 9(08).
           05  CLELG-NEXT-DUE-DATE         PIC 9(08).
           05  CLELG-CYCLE-CD              PIC X(02).
           05  CLELG-QUEUE-CD              PIC X(02).
           05  CLELG-STATUS-CD             PIC X(02).
               88  CLELG-STATUS-CD-PS              VALUE 'PS'.
               88  CLELG-STATUS-CD-UR              VALUE 'UR'.
               88  CLELG-STATUS-CD-XM              VALUE 'XM'.
               88  CLELG-STATUS-CD-YA              VALUE 'YA'.
           05  CLELG-REASON-CD             PIC X(02).
               88  CLELG-REASON-CD-AT              VALUE 'AT'.
               88  CLELG-REASON-CD-BJ              VALUE 'BJ'.
               88  CLELG-REASON-CD-GG              VALUE 'GG'.
               88  CLELG-REASON-CD-ZR              VALUE 'ZR'.
           05  CLELG-NOTICE-SENT-FLAG      PIC X(01).
               88  CLELG-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  CLELG-EXEMPT-FLAG           PIC X(01).
               88  CLELG-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  CLELG-OVERRIDE-FLAG         PIC X(01).
               88  CLELG-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  CLELG-NOTICE-CNT            PIC 9(07).
           05  CLELG-ITEM-CNT              PIC 9(07).
           05  CLELG-MARGIN                PIC 9(03)V9(06).
           05  CLELG-FEE-PCT               PIC 9(03)V9(06).
           05  CLELG-DESC                  PIC X(30).
           05  CLELG-ADDR                  PIC X(30).
           05  CLELG-RETURN-CODE           PIC 9(02).
