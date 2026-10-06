      *================================================================*
      * COPYBOOK : ESREVS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET ESREVS MAP ESREVM         *
      *            TRANSACTION ES89 - MORTGAGE ESCROW                  *
      *            GENERATED FROM BMS SOURCE ESREVS - DO NOT EDIT      *
      *================================================================*
       01  ESREVMI.
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
           02  KEYL                        PIC S9(4) COMP.
           02  KEYF                        PIC X.
           02  FILLER REDEFINES KEYF.
               03  KEYA                    PIC X.
           02  FILLER                      PIC X(02).
           02  KEYI                        PIC X(012).
           02  NEXTDUL                     PIC S9(4) COMP.
           02  NEXTDUF                     PIC X.
           02  FILLER REDEFINES NEXTDUF.
               03  NEXTDUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEXTDUI                     PIC X(010).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(007).
           02  EXCEPTL                     PIC S9(4) COMP.
           02  EXCEPTF                     PIC X.
           02  FILLER REDEFINES EXCEPTF.
               03  EXCEPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXCEPTI                     PIC X(007).
           02  DISCOUL                     PIC S9(4) COMP.
           02  DISCOUF                     PIC X.
           02  FILLER REDEFINES DISCOUF.
               03  DISCOUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DISCOUI                     PIC X(010).
           02  CYCLECL                     PIC S9(4) COMP.
           02  CYCLECF                     PIC X.
           02  FILLER REDEFINES CYCLECF.
               03  CYCLECA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CYCLECI                     PIC X(007).
           02  SURPLUL                     PIC S9(4) COMP.
           02  SURPLUF                     PIC X.
           02  FILLER REDEFINES SURPLUF.
               03  SURPLUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SURPLUI                     PIC X(016).
           02  SUSPENL                     PIC S9(4) COMP.
           02  SUSPENF                     PIC X.
           02  FILLER REDEFINES SUSPENF.
               03  SUSPENA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SUSPENI                     PIC X(001).
           02  LASTSTL                     PIC S9(4) COMP.
           02  LASTSTF                     PIC X.
           02  FILLER REDEFINES LASTSTF.
               03  LASTSTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTSTI                     PIC X(010).
           02  CUSHIOL                     PIC S9(4) COMP.
           02  CUSHIOF                     PIC X.
           02  FILLER REDEFINES CUSHIOF.
               03  CUSHIOA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CUSHIOI                     PIC X(016).
           02  ANALYSL                     PIC S9(4) COMP.
           02  ANALYSF                     PIC X.
           02  FILLER REDEFINES ANALYSF.
               03  ANALYSA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ANALYSI                     PIC X(002).
           02  FEEPCTL                     PIC S9(4) COMP.
           02  FEEPCTF                     PIC X.
           02  FILLER REDEFINES FEEPCTF.
               03  FEEPCTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FEEPCTI                     PIC X(010).
           02  HOLDFLL                     PIC S9(4) COMP.
           02  HOLDFLF                     PIC X.
           02  FILLER REDEFINES HOLDFLF.
               03  HOLDFLA                 PIC X.
           02  FILLER                      PIC X(02).
           02  HOLDFLI                     PIC X(001).
           02  RATEL                       PIC S9(4) COMP.
           02  RATEF                       PIC X.
           02  FILLER REDEFINES RATEF.
               03  RATEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  RATEI                       PIC X(010).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  ESREVMO REDEFINES ESREVMI.
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
           02  KEYC                        PIC X.
           02  KEYH                        PIC X.
           02  KEYO                        PIC X(012).
           02  FILLER                      PIC X(03).
           02  NEXTDUC                     PIC X.
           02  NEXTDUH                     PIC X.
           02  NEXTDUO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  EXCEPTC                     PIC X.
           02  EXCEPTH                     PIC X.
           02  EXCEPTO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  DISCOUC                     PIC X.
           02  DISCOUH                     PIC X.
           02  DISCOUO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CYCLECC                     PIC X.
           02  CYCLECH                     PIC X.
           02  CYCLECO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  SURPLUC                     PIC X.
           02  SURPLUH                     PIC X.
           02  SURPLUO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  SUSPENC                     PIC X.
           02  SUSPENH                     PIC X.
           02  SUSPENO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  LASTSTC                     PIC X.
           02  LASTSTH                     PIC X.
           02  LASTSTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CUSHIOC                     PIC X.
           02  CUSHIOH                     PIC X.
           02  CUSHIOO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  ANALYSC                     PIC X.
           02  ANALYSH                     PIC X.
           02  ANALYSO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  FEEPCTC                     PIC X.
           02  FEEPCTH                     PIC X.
           02  FEEPCTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  HOLDFLC                     PIC X.
           02  HOLDFLH                     PIC X.
           02  HOLDFLO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  RATEC                       PIC X.
           02  RATEH                       PIC X.
           02  RATEO                       PIC X(010).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
