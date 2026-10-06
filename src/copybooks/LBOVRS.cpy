      *================================================================*
      * COPYBOOK : LBOVRS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET LBOVRS MAP LBOVRM         *
      *            TRANSACTION LB72 - LOCKBOX REMITTANCE               *
      *            GENERATED FROM BMS SOURCE LBOVRS - DO NOT EDIT      *
      *================================================================*
       01  LBOVRMI.
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
           02  PENALTL                     PIC S9(4) COMP.
           02  PENALTF                     PIC X.
           02  FILLER REDEFINES PENALTF.
               03  PENALTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PENALTI                     PIC X(010).
           02  AUTOREL                     PIC S9(4) COMP.
           02  AUTOREF                     PIC X.
           02  FILLER REDEFINES AUTOREF.
               03  AUTOREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  AUTOREI                     PIC X(001).
           02  REMITYL                     PIC S9(4) COMP.
           02  REMITYF                     PIC X.
           02  FILLER REDEFINES REMITYF.
               03  REMITYA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REMITYI                     PIC X(016).
           02  NAMEL                       PIC S9(4) COMP.
           02  NAMEF                       PIC X.
           02  FILLER REDEFINES NAMEF.
               03  NAMEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  NAMEI                       PIC X(030).
           02  FLOATAL                     PIC S9(4) COMP.
           02  FLOATAF                     PIC X.
           02  FILLER REDEFINES FLOATAF.
               03  FLOATAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FLOATAI                     PIC X(016).
           02  FEEPCTL                     PIC S9(4) COMP.
           02  FEEPCTF                     PIC X.
           02  FILLER REDEFINES FEEPCTF.
               03  FEEPCTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FEEPCTI                     PIC X(010).
           02  EXCEPTL                     PIC S9(4) COMP.
           02  EXCEPTF                     PIC X.
           02  FILLER REDEFINES EXCEPTF.
               03  EXCEPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXCEPTI                     PIC X(007).
           02  REMITAL                     PIC S9(4) COMP.
           02  REMITAF                     PIC X.
           02  FILLER REDEFINES REMITAF.
               03  REMITAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REMITAI                     PIC X(016).
           02  DISCOUL                     PIC S9(4) COMP.
           02  DISCOUF                     PIC X.
           02  FILLER REDEFINES DISCOUF.
               03  DISCOUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DISCOUI                     PIC X(010).
           02  CHECKAL                     PIC S9(4) COMP.
           02  CHECKAF                     PIC X.
           02  FILLER REDEFINES CHECKAF.
               03  CHECKAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CHECKAI                     PIC X(016).
           02  CYCLECL                     PIC S9(4) COMP.
           02  CYCLECF                     PIC X.
           02  FILLER REDEFINES CYCLECF.
               03  CYCLECA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CYCLECI                     PIC X(003).
           02  LASTPAL                     PIC S9(4) COMP.
           02  LASTPAF                     PIC X.
           02  FILLER REDEFINES LASTPAF.
               03  LASTPAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTPAI                     PIC X(010).
           02  EXEMPTL                     PIC S9(4) COMP.
           02  EXEMPTF                     PIC X.
           02  FILLER REDEFINES EXEMPTF.
               03  EXEMPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXEMPTI                     PIC X(001).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  LBOVRMO REDEFINES LBOVRMI.
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
           02  PENALTC                     PIC X.
           02  PENALTH                     PIC X.
           02  PENALTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  AUTOREC                     PIC X.
           02  AUTOREH                     PIC X.
           02  AUTOREO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  REMITYC                     PIC X.
           02  REMITYH                     PIC X.
           02  REMITYO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NAMEC                       PIC X.
           02  NAMEH                       PIC X.
           02  NAMEO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  FLOATAC                     PIC X.
           02  FLOATAH                     PIC X.
           02  FLOATAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  FEEPCTC                     PIC X.
           02  FEEPCTH                     PIC X.
           02  FEEPCTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  EXCEPTC                     PIC X.
           02  EXCEPTH                     PIC X.
           02  EXCEPTO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  REMITAC                     PIC X.
           02  REMITAH                     PIC X.
           02  REMITAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  DISCOUC                     PIC X.
           02  DISCOUH                     PIC X.
           02  DISCOUO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CHECKAC                     PIC X.
           02  CHECKAH                     PIC X.
           02  CHECKAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  CYCLECC                     PIC X.
           02  CYCLECH                     PIC X.
           02  CYCLECO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  LASTPAC                     PIC X.
           02  LASTPAH                     PIC X.
           02  LASTPAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  EXEMPTC                     PIC X.
           02  EXEMPTH                     PIC X.
           02  EXEMPTO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
