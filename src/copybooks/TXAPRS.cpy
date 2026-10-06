      *================================================================*
      * COPYBOOK : TXAPRS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET TXAPRS MAP TXAPRM         *
      *            TRANSACTION TX12 - TELEX WIRE MESSAGING             *
      *            GENERATED FROM BMS SOURCE TXAPRS - DO NOT EDIT      *
      *================================================================*
       01  TXAPRMI.
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
           02  WIREAML                     PIC S9(4) COMP.
           02  WIREAMF                     PIC X.
           02  FILLER REDEFINES WIREAMF.
               03  WIREAMA                 PIC X.
           02  FILLER                      PIC X(02).
           02  WIREAMI                     PIC X(016).
           02  BRANCHL                     PIC S9(4) COMP.
           02  BRANCHF                     PIC X.
           02  FILLER REDEFINES BRANCHF.
               03  BRANCHA                 PIC X.
           02  FILLER                      PIC X(02).
           02  BRANCHI                     PIC X(002).
           02  SETTLEL                     PIC S9(4) COMP.
           02  SETTLEF                     PIC X.
           02  FILLER REDEFINES SETTLEF.
               03  SETTLEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SETTLEI                     PIC X(016).
           02  COVERAL                     PIC S9(4) COMP.
           02  COVERAF                     PIC X.
           02  FILLER REDEFINES COVERAF.
               03  COVERAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  COVERAI                     PIC X(016).
           02  CONTACL                     PIC S9(4) COMP.
           02  CONTACF                     PIC X.
           02  FILLER REDEFINES CONTACF.
               03  CONTACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CONTACI                     PIC X(020).
           02  WIREYTL                     PIC S9(4) COMP.
           02  WIREYTF                     PIC X.
           02  FILLER REDEFINES WIREYTF.
               03  WIREYTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  WIREYTI                     PIC X(016).
           02  CHARGEL                     PIC S9(4) COMP.
           02  CHARGEF                     PIC X.
           02  FILLER REDEFINES CHARGEF.
               03  CHARGEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CHARGEI                     PIC X(016).
           02  TYPECDL                     PIC S9(4) COMP.
           02  TYPECDF                     PIC X.
           02  FILLER REDEFINES TYPECDF.
               03  TYPECDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  TYPECDI                     PIC X(001).
           02  EXCEPTL                     PIC S9(4) COMP.
           02  EXCEPTF                     PIC X.
           02  FILLER REDEFINES EXCEPTF.
               03  EXCEPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXCEPTI                     PIC X(007).
           02  RATEL                       PIC S9(4) COMP.
           02  RATEF                       PIC X.
           02  FILLER REDEFINES RATEF.
               03  RATEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  RATEI                       PIC X(010).
           02  DISCOUL                     PIC S9(4) COMP.
           02  DISCOUF                     PIC X.
           02  FILLER REDEFINES DISCOUF.
               03  DISCOUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DISCOUI                     PIC X(010).
           02  TESTKEL                     PIC S9(4) COMP.
           02  TESTKEF                     PIC X.
           02  FILLER REDEFINES TESTKEF.
               03  TESTKEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  TESTKEI                     PIC X(002).
           02  PREFERL                     PIC S9(4) COMP.
           02  PREFERF                     PIC X.
           02  FILLER REDEFINES PREFERF.
               03  PREFERA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PREFERI                     PIC X(001).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  TXAPRMO REDEFINES TXAPRMI.
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
           02  WIREAMC                     PIC X.
           02  WIREAMH                     PIC X.
           02  WIREAMO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  BRANCHC                     PIC X.
           02  BRANCHH                     PIC X.
           02  BRANCHO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  SETTLEC                     PIC X.
           02  SETTLEH                     PIC X.
           02  SETTLEO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  COVERAC                     PIC X.
           02  COVERAH                     PIC X.
           02  COVERAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  CONTACC                     PIC X.
           02  CONTACH                     PIC X.
           02  CONTACO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  WIREYTC                     PIC X.
           02  WIREYTH                     PIC X.
           02  WIREYTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  CHARGEC                     PIC X.
           02  CHARGEH                     PIC X.
           02  CHARGEO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  TYPECDC                     PIC X.
           02  TYPECDH                     PIC X.
           02  TYPECDO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  EXCEPTC                     PIC X.
           02  EXCEPTH                     PIC X.
           02  EXCEPTO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  RATEC                       PIC X.
           02  RATEH                       PIC X.
           02  RATEO                       PIC X(010).
           02  FILLER                      PIC X(03).
           02  DISCOUC                     PIC X.
           02  DISCOUH                     PIC X.
           02  DISCOUO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  TESTKEC                     PIC X.
           02  TESTKEH                     PIC X.
           02  TESTKEO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  PREFERC                     PIC X.
           02  PREFERH                     PIC X.
           02  PREFERO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
