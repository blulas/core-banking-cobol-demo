      *================================================================*
      * COPYBOOK : EUINQS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET EUINQS MAP EUINQM         *
      *            TRANSACTION EU73 - EURO CONVERSION                  *
      *            GENERATED FROM BMS SOURCE EUINQS - DO NOT EDIT      *
      *================================================================*
       01  EUINQMI.
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
           02  CHANNEL                     PIC S9(4) COMP.
           02  CHANNEF                     PIC X.
           02  FILLER REDEFINES CHANNEF.
               03  CHANNEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CHANNEI                     PIC X(003).
           02  PENALTL                     PIC S9(4) COMP.
           02  PENALTF                     PIC X.
           02  FILLER REDEFINES PENALTF.
               03  PENALTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PENALTI                     PIC X(010).
           02  NEXTREL                     PIC S9(4) COMP.
           02  NEXTREF                     PIC X.
           02  FILLER REDEFINES NEXTREF.
               03  NEXTREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEXTREI                     PIC X(010).
           02  CYCLECL                     PIC S9(4) COMP.
           02  CYCLECF                     PIC X.
           02  FILLER REDEFINES CYCLECF.
               03  CYCLECA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CYCLECI                     PIC X(007).
           02  FEEYTDL                     PIC S9(4) COMP.
           02  FEEYTDF                     PIC X.
           02  FILLER REDEFINES FEEYTDF.
               03  FEEYTDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FEEYTDI                     PIC X(016).
           02  STATUSL                     PIC S9(4) COMP.
           02  STATUSF                     PIC X.
           02  FILLER REDEFINES STATUSF.
               03  STATUSA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STATUSI                     PIC X(001).
           02  REGIONL                     PIC S9(4) COMP.
           02  REGIONF                     PIC X.
           02  FILLER REDEFINES REGIONF.
               03  REGIONA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REGIONI                     PIC X(003).
           02  OPENDAL                     PIC S9(4) COMP.
           02  OPENDAF                     PIC X.
           02  FILLER REDEFINES OPENDAF.
               03  OPENDAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OPENDAI                     PIC X(010).
           02  FEEAMTL                     PIC S9(4) COMP.
           02  FEEAMTF                     PIC X.
           02  FILLER REDEFINES FEEAMTF.
               03  FEEAMTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FEEAMTI                     PIC X(016).
           02  TYPECDL                     PIC S9(4) COMP.
           02  TYPECDF                     PIC X.
           02  FILLER REDEFINES TYPECDF.
               03  TYPECDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  TYPECDI                     PIC X(003).
           02  REVIEWL                     PIC S9(4) COMP.
           02  REVIEWF                     PIC X.
           02  FILLER REDEFINES REVIEWF.
               03  REVIEWA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REVIEWI                     PIC X(001).
           02  RESIDUL                     PIC S9(4) COMP.
           02  RESIDUF                     PIC X.
           02  FILLER REDEFINES RESIDUF.
               03  RESIDUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RESIDUI                     PIC X(016).
           02  CURRENL                     PIC S9(4) COMP.
           02  CURRENF                     PIC X.
           02  FILLER REDEFINES CURRENF.
               03  CURRENA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CURRENI                     PIC X(002).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  EUINQMO REDEFINES EUINQMI.
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
           02  CHANNEC                     PIC X.
           02  CHANNEH                     PIC X.
           02  CHANNEO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  PENALTC                     PIC X.
           02  PENALTH                     PIC X.
           02  PENALTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  NEXTREC                     PIC X.
           02  NEXTREH                     PIC X.
           02  NEXTREO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CYCLECC                     PIC X.
           02  CYCLECH                     PIC X.
           02  CYCLECO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  FEEYTDC                     PIC X.
           02  FEEYTDH                     PIC X.
           02  FEEYTDO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  STATUSC                     PIC X.
           02  STATUSH                     PIC X.
           02  STATUSO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  REGIONC                     PIC X.
           02  REGIONH                     PIC X.
           02  REGIONO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  OPENDAC                     PIC X.
           02  OPENDAH                     PIC X.
           02  OPENDAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  FEEAMTC                     PIC X.
           02  FEEAMTH                     PIC X.
           02  FEEAMTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  TYPECDC                     PIC X.
           02  TYPECDH                     PIC X.
           02  TYPECDO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  REVIEWC                     PIC X.
           02  REVIEWH                     PIC X.
           02  REVIEWO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  RESIDUC                     PIC X.
           02  RESIDUH                     PIC X.
           02  RESIDUO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  CURRENC                     PIC X.
           02  CURRENH                     PIC X.
           02  CURRENO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
