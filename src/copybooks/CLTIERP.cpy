      *================================================================*
      * COPYBOOK : CLTIERP                                             *
      * PURPOSE  : LINKAGE FOR COLLECTIONS QUEUE RULE MODULE CLTIER    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-07-01 PQR  ORIGINAL                                       *
      * 1995-03-21 CRP  PROMISED CUTOFF MOVED PRB2506                  *
      * 1998-01-27 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-02-27 LMB  FEE RERUN PROTECTION INC4459                   *
      * 2001-08-15 LMB  NEW PROMISED REPORT COLUMNS PRB8219            *
      * 2003-08-20 DPS  FEE NOTICE TEXT PER LEGAL CHG2616              *
      * 2003-07-01 RJT  PAST-DUE LIMIT REVISED REQ5064                 *
      * 2004-03-04 PQR  PROMISED STATUS CODES ADDED REQ3786            *
      * 2004-02-17 GAF  WRITEOFF TABLE EXPANDED REQ6226                *
      *================================================================*
       01  CLTIER-PARMS.
           05  CLTIER-COLLECTED-YTD        PIC S9(13)V99 COMP-3.
           05  CLTIER-PAST-DUE-YTD         PIC S9(13)V99 COMP-3.
           05  CLTIER-PROMISED-AMT         PIC S9(13)V99 COMP-3.
           05  CLTIER-PAST-DUE-AMT         PIC S9(13)V99 COMP-3.
           05  CLTIER-COLLECTED-AMT        PIC S9(13)V99 COMP-3.
           05  CLTIER-PROMISED-YTD         PIC S9(13)V99 COMP-3.
           05  CLTIER-NEXT-DUE-DATE        PIC 9(08).
           05  CLTIER-OPEN-DATE            PIC 9(08).
           05  CLTIER-LAST-PAY-DATE        PIC 9(08).
           05  CLTIER-CLOSE-DATE           PIC 9(08).
           05  CLTIER-STATUS-CD            PIC X(02).
               88  CLTIER-STATUS-CD-PS             VALUE 'PS'.
               88  CLTIER-STATUS-CD-UR             VALUE 'UR'.
               88  CLTIER-STATUS-CD-XM             VALUE 'XM'.
               88  CLTIER-STATUS-CD-YA             VALUE 'YA'.
           05  CLTIER-REASON-CD            PIC X(02).
               88  CLTIER-REASON-CD-AT             VALUE 'AT'.
               88  CLTIER-REASON-CD-BJ             VALUE 'BJ'.
               88  CLTIER-REASON-CD-GG             VALUE 'GG'.
               88  CLTIER-REASON-CD-ZR             VALUE 'ZR'.
           05  CLTIER-ACTION-CD            PIC X(02).
               88  CLTIER-ACTION-CD-KJ             VALUE 'KJ'.
               88  CLTIER-ACTION-CD-TM             VALUE 'TM'.
               88  CLTIER-ACTION-CD-VL             VALUE 'VL'.
               88  CLTIER-ACTION-CD-ZS             VALUE 'ZS'.
           05  CLTIER-CYCLE-CD             PIC X(02).
           05  CLTIER-EXEMPT-FLAG          PIC X(01).
               88  CLTIER-EXEMPT-FLAG-YES          VALUE 'Y'.
           05  CLTIER-SUSPEND-FLAG         PIC X(01).
               88  CLTIER-SUSPEND-FLAG-YES         VALUE 'Y'.
           05  CLTIER-OVERRIDE-FLAG        PIC X(01).
               88  CLTIER-OVERRIDE-FLAG-YES        VALUE 'Y'.
           05  CLTIER-CYCLE-CNT            PIC 9(07).
           05  CLTIER-RETRY-CNT            PIC 9(07).
           05  CLTIER-FEE-PCT              PIC 9(03)V9(06).
           05  CLTIER-MARGIN               PIC 9(03)V9(06).
           05  CLTIER-CONTACT              PIC X(20).
           05  CLTIER-CUST-ID              PIC X(10).
           05  CLTIER-RETURN-CODE          PIC 9(02).
