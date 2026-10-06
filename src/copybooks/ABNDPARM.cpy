      *================================================================*
      * COPYBOOK : ABNDPARM                                            *
      * PURPOSE  : LINKAGE FOR ABENDRTN - STANDARD ERROR TERMINATION   *
      *   CALLER FILLS IN FIELDS THEN CALLS 'ABENDRTN'. CONTROL DOES   *
      *   NOT RETURN. RETURN-CODE IS SET TO ABND-CODE (DEFAULT 16).    *
      *================================================================*
       01  ABEND-PARMS.
           05  ABND-PROGRAM                PIC X(08).
           05  ABND-PARAGRAPH              PIC X(30).
           05  ABND-FILE-NAME              PIC X(08).
           05  ABND-FILE-STATUS            PIC X(02).
           05  ABND-CODE                   PIC 9(04).
           05  ABND-MESSAGE                PIC X(80).
