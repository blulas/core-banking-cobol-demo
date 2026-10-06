      *================================================================*
      * COPYBOOK : ACINQS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET ACINQS MAP ACINQM         *
      *            TRANSACTION CB04 - ACCOUNT INQUIRY                  *
      *            GENERATED FROM BMS SOURCE ACINQS - DO NOT EDIT      *
      *================================================================*
       01  ACINQMI.
           02  FILLER                      PIC X(12).
           02  TRANIDL                     PIC S9(4) COMP.
           02  TRANIDF                     PIC X.
           02  FILLER REDEFINES TRANIDF.
               03  TRANIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  TRANIDI                     PIC X(004).
           02  TITLEL                      PIC S9(4) COMP.
           02  TITLEF                      PIC X.
           02  FILLER REDEFINES TITLEF.
               03  TITLEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  TITLEI                      PIC X(040).
           02  CURDATEL                    PIC S9(4) COMP.
           02  CURDATEF                    PIC X.
           02  FILLER REDEFINES CURDATEF.
               03  CURDATEA                PIC X.
           02  FILLER                      PIC X(02).
           02  CURDATEI                    PIC X(010).
           02  ACCTNOL                     PIC S9(4) COMP.
           02  ACCTNOF                     PIC X.
           02  FILLER REDEFINES ACCTNOF.
               03  ACCTNOA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ACCTNOI                     PIC X(012).
           02  PRODNML                     PIC S9(4) COMP.
           02  PRODNMF                     PIC X.
           02  FILLER REDEFINES PRODNMF.
               03  PRODNMA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PRODNMI                     PIC X(030).
           02  ASTATL                      PIC S9(4) COMP.
           02  ASTATF                      PIC X.
           02  FILLER REDEFINES ASTATF.
               03  ASTATA                  PIC X.
           02  FILLER                      PIC X(02).
           02  ASTATI                      PIC X(011).
           02  OWNERL                      PIC S9(4) COMP.
           02  OWNERF                      PIC X.
           02  FILLER REDEFINES OWNERF.
               03  OWNERA                  PIC X.
           02  FILLER                      PIC X(02).
           02  OWNERI                      PIC X(040).
           02  OPENDTL                     PIC S9(4) COMP.
           02  OPENDTF                     PIC X.
           02  FILLER REDEFINES OPENDTF.
               03  OPENDTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OPENDTI                     PIC X(010).
           02  JOINTL                      PIC S9(4) COMP.
           02  JOINTF                      PIC X.
           02  FILLER REDEFINES JOINTF.
               03  JOINTA                  PIC X.
           02  FILLER                      PIC X(02).
           02  JOINTI                      PIC X(040).
           02  LASTACTL                    PIC S9(4) COMP.
           02  LASTACTF                    PIC X.
           02  FILLER REDEFINES LASTACTF.
               03  LASTACTA                PIC X.
           02  FILLER                      PIC X(02).
           02  LASTACTI                    PIC X(010).
           02  LEDGERL                     PIC S9(4) COMP.
           02  LEDGERF                     PIC X.
           02  FILLER REDEFINES LEDGERF.
               03  LEDGERA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LEDGERI                     PIC X(018).
           02  AVAILL                      PIC S9(4) COMP.
           02  AVAILF                      PIC X.
           02  FILLER REDEFINES AVAILF.
               03  AVAILA                  PIC X.
           02  FILLER                      PIC X(02).
           02  AVAILI                      PIC X(018).
           02  HOLDAMTL                    PIC S9(4) COMP.
           02  HOLDAMTF                    PIC X.
           02  FILLER REDEFINES HOLDAMTF.
               03  HOLDAMTA                PIC X.
           02  FILLER                      PIC X(02).
           02  HOLDAMTI                    PIC X(018).
           02  ACCRINTL                    PIC S9(4) COMP.
           02  ACCRINTF                    PIC X.
           02  FILLER REDEFINES ACCRINTF.
               03  ACCRINTA                PIC X.
           02  FILLER                      PIC X(02).
           02  ACCRINTI                    PIC X(018).
           02  RATEL                       PIC S9(4) COMP.
           02  RATEF                       PIC X.
           02  FILLER REDEFINES RATEF.
               03  RATEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  RATEI                       PIC X(010).
           02  PROMOL                      PIC S9(4) COMP.
           02  PROMOF                      PIC X.
           02  FILLER REDEFINES PROMOF.
               03  PROMOA                  PIC X.
           02  FILLER                      PIC X(02).
           02  PROMOI                      PIC X(022).
           02  ODLIML                      PIC S9(4) COMP.
           02  ODLIMF                      PIC X.
           02  FILLER REDEFINES ODLIMF.
               03  ODLIMA                  PIC X.
           02  FILLER                      PIC X(02).
           02  ODLIMI                      PIC X(013).
           02  MATDTL                      PIC S9(4) COMP.
           02  MATDTF                      PIC X.
           02  FILLER REDEFINES MATDTF.
               03  MATDTA                  PIC X.
           02  FILLER                      PIC X(02).
           02  MATDTI                      PIC X(010).
           02  REGDWDL                     PIC S9(4) COMP.
           02  REGDWDF                     PIC X.
           02  FILLER REDEFINES REGDWDF.
               03  REGDWDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REGDWDI                     PIC X(003).
           02  ODOPTL                      PIC S9(4) COMP.
           02  ODOPTF                      PIC X.
           02  FILLER REDEFINES ODOPTF.
               03  ODOPTA                  PIC X.
           02  FILLER                      PIC X(02).
           02  ODOPTI                      PIC X(001).
           02  NSFCNTL                     PIC S9(4) COMP.
           02  NSFCNTF                     PIC X.
           02  FILLER REDEFINES NSFCNTF.
               03  NSFCNTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NSFCNTI                     PIC X(003).
           02  HOLDLNL                     PIC S9(4) COMP.
           02  HOLDLNF                     PIC X.
           02  FILLER REDEFINES HOLDLNF.
               03  HOLDLNA                 PIC X.
           02  FILLER                      PIC X(02).
           02  HOLDLNI                     PIC X(064).
           02  TLN01L                      PIC S9(4) COMP.
           02  TLN01F                      PIC X.
           02  FILLER REDEFINES TLN01F.
               03  TLN01A                  PIC X.
           02  FILLER                      PIC X(02).
           02  TLN01I                      PIC X(078).
           02  TLN02L                      PIC S9(4) COMP.
           02  TLN02F                      PIC X.
           02  FILLER REDEFINES TLN02F.
               03  TLN02A                  PIC X.
           02  FILLER                      PIC X(02).
           02  TLN02I                      PIC X(078).
           02  TLN03L                      PIC S9(4) COMP.
           02  TLN03F                      PIC X.
           02  FILLER REDEFINES TLN03F.
               03  TLN03A                  PIC X.
           02  FILLER                      PIC X(02).
           02  TLN03I                      PIC X(078).
           02  TLN04L                      PIC S9(4) COMP.
           02  TLN04F                      PIC X.
           02  FILLER REDEFINES TLN04F.
               03  TLN04A                  PIC X.
           02  FILLER                      PIC X(02).
           02  TLN04I                      PIC X(078).
           02  TLN05L                      PIC S9(4) COMP.
           02  TLN05F                      PIC X.
           02  FILLER REDEFINES TLN05F.
               03  TLN05A                  PIC X.
           02  FILLER                      PIC X(02).
           02  TLN05I                      PIC X(078).
           02  TLN06L                      PIC S9(4) COMP.
           02  TLN06F                      PIC X.
           02  FILLER REDEFINES TLN06F.
               03  TLN06A                  PIC X.
           02  FILLER                      PIC X(02).
           02  TLN06I                      PIC X(078).
           02  TLN07L                      PIC S9(4) COMP.
           02  TLN07F                      PIC X.
           02  FILLER REDEFINES TLN07F.
               03  TLN07A                  PIC X.
           02  FILLER                      PIC X(02).
           02  TLN07I                      PIC X(078).
           02  TLN08L                      PIC S9(4) COMP.
           02  TLN08F                      PIC X.
           02  FILLER REDEFINES TLN08F.
               03  TLN08A                  PIC X.
           02  FILLER                      PIC X(02).
           02  TLN08I                      PIC X(078).
           02  TLN09L                      PIC S9(4) COMP.
           02  TLN09F                      PIC X.
           02  FILLER REDEFINES TLN09F.
               03  TLN09A                  PIC X.
           02  FILLER                      PIC X(02).
           02  TLN09I                      PIC X(078).
           02  TLN10L                      PIC S9(4) COMP.
           02  TLN10F                      PIC X.
           02  FILLER REDEFINES TLN10F.
               03  TLN10A                  PIC X.
           02  FILLER                      PIC X(02).
           02  TLN10I                      PIC X(078).
           02  PAGENOL                     PIC S9(4) COMP.
           02  PAGENOF                     PIC X.
           02  FILLER REDEFINES PAGENOF.
               03  PAGENOA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PAGENOI                     PIC X(003).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
           02  PFKEYSL                     PIC S9(4) COMP.
           02  PFKEYSF                     PIC X.
           02  FILLER REDEFINES PFKEYSF.
               03  PFKEYSA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PFKEYSI                     PIC X(079).
       01  ACINQMO REDEFINES ACINQMI.
           02  FILLER                      PIC X(12).
           02  FILLER                      PIC X(03).
           02  TRANIDC                     PIC X.
           02  TRANIDH                     PIC X.
           02  TRANIDO                     PIC X(004).
           02  FILLER                      PIC X(03).
           02  TITLEC                      PIC X.
           02  TITLEH                      PIC X.
           02  TITLEO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  CURDATEC                    PIC X.
           02  CURDATEH                    PIC X.
           02  CURDATEO                    PIC X(010).
           02  FILLER                      PIC X(03).
           02  ACCTNOC                     PIC X.
           02  ACCTNOH                     PIC X.
           02  ACCTNOO                     PIC X(012).
           02  FILLER                      PIC X(03).
           02  PRODNMC                     PIC X.
           02  PRODNMH                     PIC X.
           02  PRODNMO                     PIC X(030).
           02  FILLER                      PIC X(03).
           02  ASTATC                      PIC X.
           02  ASTATH                      PIC X.
           02  ASTATO                      PIC X(011).
           02  FILLER                      PIC X(03).
           02  OWNERC                      PIC X.
           02  OWNERH                      PIC X.
           02  OWNERO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  OPENDTC                     PIC X.
           02  OPENDTH                     PIC X.
           02  OPENDTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  JOINTC                      PIC X.
           02  JOINTH                      PIC X.
           02  JOINTO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  LASTACTC                    PIC X.
           02  LASTACTH                    PIC X.
           02  LASTACTO                    PIC X(010).
           02  FILLER                      PIC X(03).
           02  LEDGERC                     PIC X.
           02  LEDGERH                     PIC X.
           02  LEDGERO                     PIC X(018).
           02  FILLER                      PIC X(03).
           02  AVAILC                      PIC X.
           02  AVAILH                      PIC X.
           02  AVAILO                      PIC X(018).
           02  FILLER                      PIC X(03).
           02  HOLDAMTC                    PIC X.
           02  HOLDAMTH                    PIC X.
           02  HOLDAMTO                    PIC X(018).
           02  FILLER                      PIC X(03).
           02  ACCRINTC                    PIC X.
           02  ACCRINTH                    PIC X.
           02  ACCRINTO                    PIC X(018).
           02  FILLER                      PIC X(03).
           02  RATEC                       PIC X.
           02  RATEH                       PIC X.
           02  RATEO                       PIC X(010).
           02  FILLER                      PIC X(03).
           02  PROMOC                      PIC X.
           02  PROMOH                      PIC X.
           02  PROMOO                      PIC X(022).
           02  FILLER                      PIC X(03).
           02  ODLIMC                      PIC X.
           02  ODLIMH                      PIC X.
           02  ODLIMO                      PIC X(013).
           02  FILLER                      PIC X(03).
           02  MATDTC                      PIC X.
           02  MATDTH                      PIC X.
           02  MATDTO                      PIC X(010).
           02  FILLER                      PIC X(03).
           02  REGDWDC                     PIC X.
           02  REGDWDH                     PIC X.
           02  REGDWDO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  ODOPTC                      PIC X.
           02  ODOPTH                      PIC X.
           02  ODOPTO                      PIC X(001).
           02  FILLER                      PIC X(03).
           02  NSFCNTC                     PIC X.
           02  NSFCNTH                     PIC X.
           02  NSFCNTO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  HOLDLNC                     PIC X.
           02  HOLDLNH                     PIC X.
           02  HOLDLNO                     PIC X(064).
           02  FILLER                      PIC X(03).
           02  TLN01C                      PIC X.
           02  TLN01H                      PIC X.
           02  TLN01O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  TLN02C                      PIC X.
           02  TLN02H                      PIC X.
           02  TLN02O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  TLN03C                      PIC X.
           02  TLN03H                      PIC X.
           02  TLN03O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  TLN04C                      PIC X.
           02  TLN04H                      PIC X.
           02  TLN04O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  TLN05C                      PIC X.
           02  TLN05H                      PIC X.
           02  TLN05O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  TLN06C                      PIC X.
           02  TLN06H                      PIC X.
           02  TLN06O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  TLN07C                      PIC X.
           02  TLN07H                      PIC X.
           02  TLN07O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  TLN08C                      PIC X.
           02  TLN08H                      PIC X.
           02  TLN08O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  TLN09C                      PIC X.
           02  TLN09H                      PIC X.
           02  TLN09O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  TLN10C                      PIC X.
           02  TLN10H                      PIC X.
           02  TLN10O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  PAGENOC                     PIC X.
           02  PAGENOH                     PIC X.
           02  PAGENOO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
           02  FILLER                      PIC X(03).
           02  PFKEYSC                     PIC X.
           02  PFKEYSH                     PIC X.
           02  PFKEYSO                     PIC X(079).
