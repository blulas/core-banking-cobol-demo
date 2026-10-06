      *================================================================*
      * COPYBOOK : DRHSTS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET DRHSTS MAP DRHSTM         *
      *            TRANSACTION DR64 - DIVIDEND REINVESTMENT            *
      *            GENERATED FROM BMS SOURCE DRHSTS - DO NOT EDIT      *
      *================================================================*
       01  DRHSTMI.
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
           02  REINVEL                     PIC S9(4) COMP.
           02  REINVEF                     PIC X.
           02  FILLER REDEFINES REINVEF.
               03  REINVEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REINVEI                     PIC X(016).
           02  NEXTDUL                     PIC S9(4) COMP.
           02  NEXTDUF                     PIC X.
           02  FILLER REDEFINES NEXTDUF.
               03  NEXTDUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEXTDUI                     PIC X(010).
           02  REINVE1L                    PIC S9(4) COMP.
           02  REINVE1F                    PIC X.
           02  FILLER REDEFINES REINVE1F.
               03  REINVE1A                PIC X.
           02  FILLER                      PIC X(02).
           02  REINVE1I                    PIC X(016).
           02  EFFECTL                     PIC S9(4) COMP.
           02  EFFECTF                     PIC X.
           02  FILLER REDEFINES EFFECTF.
               03  EFFECTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EFFECTI                     PIC X(010).
           02  CHANNEL                     PIC S9(4) COMP.
           02  CHANNEF                     PIC X.
           02  FILLER REDEFINES CHANNEF.
               03  CHANNEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CHANNEI                     PIC X(002).
           02  ITEMCNL                     PIC S9(4) COMP.
           02  ITEMCNF                     PIC X.
           02  FILLER REDEFINES ITEMCNF.
               03  ITEMCNA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ITEMCNI                     PIC X(007).
           02  WITHHOL                     PIC S9(4) COMP.
           02  WITHHOF                     PIC X.
           02  FILLER REDEFINES WITHHOF.
               03  WITHHOA                 PIC X.
           02  FILLER                      PIC X(02).
           02  WITHHOI                     PIC X(016).
           02  CONTACL                     PIC S9(4) COMP.
           02  CONTACF                     PIC X.
           02  FILLER REDEFINES CONTACF.
               03  CONTACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CONTACI                     PIC X(020).
           02  PREFERL                     PIC S9(4) COMP.
           02  PREFERF                     PIC X.
           02  FILLER REDEFINES PREFERF.
               03  PREFERA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PREFERI                     PIC X(001).
           02  NAMEL                       PIC S9(4) COMP.
           02  NAMEF                       PIC X.
           02  FILLER REDEFINES NAMEF.
               03  NAMEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  NAMEI                       PIC X(030).
           02  DIVIDEL                     PIC S9(4) COMP.
           02  DIVIDEF                     PIC X.
           02  FILLER REDEFINES DIVIDEF.
               03  DIVIDEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DIVIDEI                     PIC X(016).
           02  OVERRIL                     PIC S9(4) COMP.
           02  OVERRIF                     PIC X.
           02  FILLER REDEFINES OVERRIF.
               03  OVERRIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OVERRII                     PIC X(001).
           02  FEEYTDL                     PIC S9(4) COMP.
           02  FEEYTDF                     PIC X.
           02  FILLER REDEFINES FEEYTDF.
               03  FEEYTDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FEEYTDI                     PIC X(016).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  DRHSTMO REDEFINES DRHSTMI.
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
           02  REINVEC                     PIC X.
           02  REINVEH                     PIC X.
           02  REINVEO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NEXTDUC                     PIC X.
           02  NEXTDUH                     PIC X.
           02  NEXTDUO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  REINVE1C                    PIC X.
           02  REINVE1H                    PIC X.
           02  REINVE1O                    PIC X(016).
           02  FILLER                      PIC X(03).
           02  EFFECTC                     PIC X.
           02  EFFECTH                     PIC X.
           02  EFFECTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CHANNEC                     PIC X.
           02  CHANNEH                     PIC X.
           02  CHANNEO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  ITEMCNC                     PIC X.
           02  ITEMCNH                     PIC X.
           02  ITEMCNO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  WITHHOC                     PIC X.
           02  WITHHOH                     PIC X.
           02  WITHHOO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  CONTACC                     PIC X.
           02  CONTACH                     PIC X.
           02  CONTACO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  PREFERC                     PIC X.
           02  PREFERH                     PIC X.
           02  PREFERO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  NAMEC                       PIC X.
           02  NAMEH                       PIC X.
           02  NAMEO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  DIVIDEC                     PIC X.
           02  DIVIDEH                     PIC X.
           02  DIVIDEO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  OVERRIC                     PIC X.
           02  OVERRIH                     PIC X.
           02  OVERRIO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  FEEYTDC                     PIC X.
           02  FEEYTDH                     PIC X.
           02  FEEYTDO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
