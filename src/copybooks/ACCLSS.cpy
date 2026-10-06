      *================================================================*
      * COPYBOOK : ACCLSS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET ACCLSS MAP ACCLSM         *
      *            TRANSACTION CB05 - CLOSE ACCOUNT                    *
      *            GENERATED FROM BMS SOURCE ACCLSS - DO NOT EDIT      *
      *================================================================*
       01  ACCLSMI.
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
           02  PAYACTL                     PIC S9(4) COMP.
           02  PAYACTF                     PIC X.
           02  FILLER REDEFINES PAYACTF.
               03  PAYACTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PAYACTI                     PIC X(012).
           02  RSNCDL                      PIC S9(4) COMP.
           02  RSNCDF                      PIC X.
           02  FILLER REDEFINES RSNCDF.
               03  RSNCDA                  PIC X.
           02  FILLER                      PIC X(02).
           02  RSNCDI                      PIC X(002).
           02  WAIVEL                      PIC S9(4) COMP.
           02  WAIVEF                      PIC X.
           02  FILLER REDEFINES WAIVEF.
               03  WAIVEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  WAIVEI                      PIC X(001).
           02  LEDGERL                     PIC S9(4) COMP.
           02  LEDGERF                     PIC X.
           02  FILLER REDEFINES LEDGERF.
               03  LEDGERA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LEDGERI                     PIC X(018).
           02  ACCRINTL                    PIC S9(4) COMP.
           02  ACCRINTF                    PIC X.
           02  FILLER REDEFINES ACCRINTF.
               03  ACCRINTA                PIC X.
           02  FILLER                      PIC X(02).
           02  ACCRINTI                    PIC X(017).
           02  PENALTYL                    PIC S9(4) COMP.
           02  PENALTYF                    PIC X.
           02  FILLER REDEFINES PENALTYF.
               03  PENALTYA                PIC X.
           02  FILLER                      PIC X(02).
           02  PENALTYI                    PIC X(018).
           02  ECFEEL                      PIC S9(4) COMP.
           02  ECFEEF                      PIC X.
           02  FILLER REDEFINES ECFEEF.
               03  ECFEEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  ECFEEI                      PIC X(018).
           02  NETAMTL                     PIC S9(4) COMP.
           02  NETAMTF                     PIC X.
           02  FILLER REDEFINES NETAMTF.
               03  NETAMTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NETAMTI                     PIC X(018).
           02  CHK01L                      PIC S9(4) COMP.
           02  CHK01F                      PIC X.
           02  FILLER REDEFINES CHK01F.
               03  CHK01A                  PIC X.
           02  FILLER                      PIC X(02).
           02  CHK01I                      PIC X(075).
           02  CHK02L                      PIC S9(4) COMP.
           02  CHK02F                      PIC X.
           02  FILLER REDEFINES CHK02F.
               03  CHK02A                  PIC X.
           02  FILLER                      PIC X(02).
           02  CHK02I                      PIC X(075).
           02  CHK03L                      PIC S9(4) COMP.
           02  CHK03F                      PIC X.
           02  FILLER REDEFINES CHK03F.
               03  CHK03A                  PIC X.
           02  FILLER                      PIC X(02).
           02  CHK03I                      PIC X(075).
           02  CHK04L                      PIC S9(4) COMP.
           02  CHK04F                      PIC X.
           02  FILLER REDEFINES CHK04F.
               03  CHK04A                  PIC X.
           02  FILLER                      PIC X(02).
           02  CHK04I                      PIC X(075).
           02  CHK05L                      PIC S9(4) COMP.
           02  CHK05F                      PIC X.
           02  FILLER REDEFINES CHK05F.
               03  CHK05A                  PIC X.
           02  FILLER                      PIC X(02).
           02  CHK05I                      PIC X(075).
           02  CLSSTATL                    PIC S9(4) COMP.
           02  CLSSTATF                    PIC X.
           02  FILLER REDEFINES CLSSTATF.
               03  CLSSTATA                PIC X.
           02  FILLER                      PIC X(02).
           02  CLSSTATI                    PIC X(040).
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
       01  ACCLSMO REDEFINES ACCLSMI.
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
           02  PAYACTC                     PIC X.
           02  PAYACTH                     PIC X.
           02  PAYACTO                     PIC X(012).
           02  FILLER                      PIC X(03).
           02  RSNCDC                      PIC X.
           02  RSNCDH                      PIC X.
           02  RSNCDO                      PIC X(002).
           02  FILLER                      PIC X(03).
           02  WAIVEC                      PIC X.
           02  WAIVEH                      PIC X.
           02  WAIVEO                      PIC X(001).
           02  FILLER                      PIC X(03).
           02  LEDGERC                     PIC X.
           02  LEDGERH                     PIC X.
           02  LEDGERO                     PIC X(018).
           02  FILLER                      PIC X(03).
           02  ACCRINTC                    PIC X.
           02  ACCRINTH                    PIC X.
           02  ACCRINTO                    PIC X(017).
           02  FILLER                      PIC X(03).
           02  PENALTYC                    PIC X.
           02  PENALTYH                    PIC X.
           02  PENALTYO                    PIC X(018).
           02  FILLER                      PIC X(03).
           02  ECFEEC                      PIC X.
           02  ECFEEH                      PIC X.
           02  ECFEEO                      PIC X(018).
           02  FILLER                      PIC X(03).
           02  NETAMTC                     PIC X.
           02  NETAMTH                     PIC X.
           02  NETAMTO                     PIC X(018).
           02  FILLER                      PIC X(03).
           02  CHK01C                      PIC X.
           02  CHK01H                      PIC X.
           02  CHK01O                      PIC X(075).
           02  FILLER                      PIC X(03).
           02  CHK02C                      PIC X.
           02  CHK02H                      PIC X.
           02  CHK02O                      PIC X(075).
           02  FILLER                      PIC X(03).
           02  CHK03C                      PIC X.
           02  CHK03H                      PIC X.
           02  CHK03O                      PIC X(075).
           02  FILLER                      PIC X(03).
           02  CHK04C                      PIC X.
           02  CHK04H                      PIC X.
           02  CHK04O                      PIC X(075).
           02  FILLER                      PIC X(03).
           02  CHK05C                      PIC X.
           02  CHK05H                      PIC X.
           02  CHK05O                      PIC X(075).
           02  FILLER                      PIC X(03).
           02  CLSSTATC                    PIC X.
           02  CLSSTATH                    PIC X.
           02  CLSSTATO                    PIC X(040).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
           02  FILLER                      PIC X(03).
           02  PFKEYSC                     PIC X.
           02  PFKEYSH                     PIC X.
           02  PFKEYSO                     PIC X(079).
