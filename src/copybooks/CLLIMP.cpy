      *================================================================*
      * COPYBOOK : CLLIMP                                              *
      * PURPOSE  : LINKAGE FOR COLLECTIONS QUEUE RULE MODULE CLLIM     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-04-01 EWS  ORIGINAL                                       *
      * 2000-07-25 SKN  PROMISED FEE SCHEDULE UPDATE CHG6867           *
      * 2001-01-18 BHS  FEE CALCULATION CORRECTED INC9517              *
      * 2003-01-05 DPS  COLLECTED EXCEPTIONS TO OPS QUEUE CHG5044      *
      * 2004-09-26 BHS  PAST-DUE EXCEPTIONS TO OPS QUEUE INC8962       *
      * 2004-02-06 CRP  PERFORMANCE - PAST-DUE READ AHEAD PRB9731      *
      * 2004-11-03 JLM  COLLECTED FEE SCHEDULE UPDATE INC3922          *
      *================================================================*
       01  CLLIM-PARMS.
           05  CLLIM-COLLECTED-YTD         PIC S9(13)V99 COMP-3.
           05  CLLIM-COLLECTED-AMT         PIC S9(13)V99 COMP-3.
           05  CLLIM-PAST-DUE-AMT          PIC S9(13)V99 COMP-3.
           05  CLLIM-PROMISED-YTD          PIC S9(13)V99 COMP-3.
           05  CLLIM-WRITEOFF-YTD          PIC S9(13)V99 COMP-3.
           05  CLLIM-FEE-YTD               PIC S9(13)V99 COMP-3.
           05  CLLIM-CLOSE-DATE            PIC 9(08).
           05  CLLIM-NOTICE-DATE           PIC 9(08).
           05  CLLIM-OPEN-DATE             PIC 9(08).
           05  CLLIM-LAST-PAY-DATE         PIC 9(08).
           05  CLLIM-CYCLE-CD              PIC X(02).
           05  CLLIM-REASON-CD             PIC X(02).
               88  CLLIM-REASON-CD-AT              VALUE 'AT'.
               88  CLLIM-REASON-CD-BJ              VALUE 'BJ'.
               88  CLLIM-REASON-CD-GG              VALUE 'GG'.
               88  CLLIM-REASON-CD-ZR              VALUE 'ZR'.
           05  CLLIM-QUEUE-CD              PIC X(02).
           05  CLLIM-REGION-CD             PIC X(02).
               88  CLLIM-REGION-CD-AT              VALUE 'AT'.
               88  CLLIM-REGION-CD-AU              VALUE 'AU'.
               88  CLLIM-REGION-CD-XR              VALUE 'XR'.
               88  CLLIM-REGION-CD-XU              VALUE 'XU'.
           05  CLLIM-REVIEW-FLAG           PIC X(01).
               88  CLLIM-REVIEW-FLAG-YES           VALUE 'Y'.
           05  CLLIM-OVERRIDE-FLAG         PIC X(01).
               88  CLLIM-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  CLLIM-PREFERRED-FLAG        PIC X(01).
               88  CLLIM-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  CLLIM-ITEM-CNT              PIC 9(07).
           05  CLLIM-NOTICE-CNT            PIC 9(07).
           05  CLLIM-FEE-PCT               PIC 9(03)V9(06).
           05  CLLIM-MARGIN                PIC 9(03)V9(06).
           05  CLLIM-CONTACT               PIC X(20).
           05  CLLIM-NAME                  PIC X(30).
           05  CLLIM-RETURN-CODE           PIC 9(02).
