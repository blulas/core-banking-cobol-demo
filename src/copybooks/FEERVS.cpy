      *================================================================*
      * COPYBOOK : FEERVS                                                *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET FEERVS MAP FEERVM          *
      *            TRANSACTION CB22 - FEE REVERSAL                      *
      *            GENERATED FROM BMS SOURCE FEERVS - DO NOT EDIT     **
      *================================================================*
       01  FEERVMI.
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
           02  OWNERL                      PIC S9(4) COMP.
           02  OWNERF                      PIC X.
           02  FILLER REDEFINES OWNERF.
               03  OWNERA                  PIC X.
           02  FILLER                      PIC X(02).
           02  OWNERI                      PIC X(030).
           02  FEETIDL                     PIC S9(4) COMP.
           02  FEETIDF                     PIC X.
           02  FILLER REDEFINES FEETIDF.
               03  FEETIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FEETIDI                     PIC X(016).
           02  RFAMTL                      PIC S9(4) COMP.
           02  RFAMTF                      PIC X.
           02  FILLER REDEFINES RFAMTF.
               03  RFAMTA                  PIC X.
           02  FILLER                      PIC X(02).
           02  RFAMTI                      PIC X(010).
           02  REASONL                     PIC S9(4) COMP.
           02  REASONF                     PIC X.
           02  FILLER REDEFINES REASONF.
               03  REASONA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REASONI                     PIC X(002).
           02  FEECDL                      PIC S9(4) COMP.
           02  FEECDF                      PIC X.
           02  FILLER REDEFINES FEECDF.
               03  FEECDA                  PIC X.
           02  FILLER                      PIC X(02).
           02  FEECDI                      PIC X(004).
           02  FEEDESCL                    PIC S9(4) COMP.
           02  FEEDESCF                    PIC X.
           02  FILLER REDEFINES FEEDESCF.
               03  FEEDESCA                PIC X.
           02  FILLER                      PIC X(02).
           02  FEEDESCI                    PIC X(040).
           02  FEEAMTL                     PIC S9(4) COMP.
           02  FEEAMTF                     PIC X.
           02  FILLER REDEFINES FEEAMTF.
               03  FEEAMTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FEEAMTI                     PIC X(012).
           02  FEEDTL                      PIC S9(4) COMP.
           02  FEEDTF                      PIC X.
           02  FILLER REDEFINES FEEDTF.
               03  FEEDTA                  PIC X.
           02  FILLER                      PIC X(02).
           02  FEEDTI                      PIC X(010).
           02  RFCNTL                      PIC S9(4) COMP.
           02  RFCNTF                      PIC X.
           02  FILLER REDEFINES RFCNTF.
               03  RFCNTA                  PIC X.
           02  FILLER                      PIC X(02).
           02  RFCNTI                      PIC X(003).
           02  LIMITL                      PIC S9(4) COMP.
           02  LIMITF                      PIC X.
           02  FILLER REDEFINES LIMITF.
               03  LIMITA                  PIC X.
           02  FILLER                      PIC X(02).
           02  LIMITI                      PIC X(010).
           02  ROLEL                       PIC S9(4) COMP.
           02  ROLEF                       PIC X.
           02  FILLER REDEFINES ROLEF.
               03  ROLEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  ROLEI                       PIC X(010).
           02  CONFIRML                    PIC S9(4) COMP.
           02  CONFIRMF                    PIC X.
           02  FILLER REDEFINES CONFIRMF.
               03  CONFIRMA                PIC X.
           02  FILLER                      PIC X(02).
           02  CONFIRMI                    PIC X(077).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  FEERVMO REDEFINES FEERVMI.
           02  FILLER                      PIC X(12).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TRANIDO                     PIC X(004).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TITLEO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  CURDATEO                    PIC X(010).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  ACCTNOO                     PIC X(012).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  OWNERO                      PIC X(030).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  FEETIDO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  RFAMTO                      PIC X(010).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  REASONO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  FEECDO                      PIC X(004).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  FEEDESCO                    PIC X(040).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  FEEAMTO                     PIC X(012).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  FEEDTO                      PIC X(010).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  RFCNTO                      PIC X(003).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  LIMITO                      PIC X(010).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  ROLEO                       PIC X(010).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  CONFIRMO                    PIC X(077).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  MSGO                        PIC X(079).
