      *================================================================*
      * COPYBOOK : FNMNTS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET FNMNTS MAP FNMNTM         *
      *            TRANSACTION FN58 - FOREIGN BANKNOTES                *
      *            GENERATED FROM BMS SOURCE FNMNTS - DO NOT EDIT      *
      *================================================================*
       01  FNMNTMI.
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
           02  ORDERTL                     PIC S9(4) COMP.
           02  ORDERTF                     PIC X.
           02  FILLER REDEFINES ORDERTF.
               03  ORDERTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ORDERTI                     PIC X(003).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(010).
           02  CURRENL                     PIC S9(4) COMP.
           02  CURRENF                     PIC X.
           02  FILLER REDEFINES CURRENF.
               03  CURRENA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CURRENI                     PIC X(001).
           02  MARGINL                     PIC S9(4) COMP.
           02  MARGINF                     PIC X.
           02  FILLER REDEFINES MARGINF.
               03  MARGINA                 PIC X.
           02  FILLER                      PIC X(02).
           02  MARGINI                     PIC X(016).
           02  EXEMPTL                     PIC S9(4) COMP.
           02  EXEMPTF                     PIC X.
           02  FILLER REDEFINES EXEMPTF.
               03  EXEMPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXEMPTI                     PIC X(001).
           02  CYCLECL                     PIC S9(4) COMP.
           02  CYCLECF                     PIC X.
           02  FILLER REDEFINES CYCLECF.
               03  CYCLECA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CYCLECI                     PIC X(007).
           02  REVIEWL                     PIC S9(4) COMP.
           02  REVIEWF                     PIC X.
           02  FILLER REDEFINES REVIEWF.
               03  REVIEWA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REVIEWI                     PIC X(001).
           02  SHIPPIL                     PIC S9(4) COMP.
           02  SHIPPIF                     PIC X.
           02  FILLER REDEFINES SHIPPIF.
               03  SHIPPIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SHIPPII                     PIC X(016).
           02  NOTICE1L                    PIC S9(4) COMP.
           02  NOTICE1F                    PIC X.
           02  FILLER REDEFINES NOTICE1F.
               03  NOTICE1A                PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICE1I                    PIC X(007).
           02  BRANCHL                     PIC S9(4) COMP.
           02  BRANCHF                     PIC X.
           02  FILLER REDEFINES BRANCHF.
               03  BRANCHA                 PIC X.
           02  FILLER                      PIC X(02).
           02  BRANCHI                     PIC X(003).
           02  BUYBACL                     PIC S9(4) COMP.
           02  BUYBACF                     PIC X.
           02  FILLER REDEFINES BUYBACF.
               03  BUYBACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  BUYBACI                     PIC X(016).
           02  OVERRIL                     PIC S9(4) COMP.
           02  OVERRIF                     PIC X.
           02  FILLER REDEFINES OVERRIF.
               03  OVERRIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OVERRII                     PIC X(001).
           02  OPENDAL                     PIC S9(4) COMP.
           02  OPENDAF                     PIC X.
           02  FILLER REDEFINES OPENDAF.
               03  OPENDAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OPENDAI                     PIC X(010).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  FNMNTMO REDEFINES FNMNTMI.
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
           02  ORDERTC                     PIC X.
           02  ORDERTH                     PIC X.
           02  ORDERTO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CURRENC                     PIC X.
           02  CURRENH                     PIC X.
           02  CURRENO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  MARGINC                     PIC X.
           02  MARGINH                     PIC X.
           02  MARGINO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  EXEMPTC                     PIC X.
           02  EXEMPTH                     PIC X.
           02  EXEMPTO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  CYCLECC                     PIC X.
           02  CYCLECH                     PIC X.
           02  CYCLECO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  REVIEWC                     PIC X.
           02  REVIEWH                     PIC X.
           02  REVIEWO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  SHIPPIC                     PIC X.
           02  SHIPPIH                     PIC X.
           02  SHIPPIO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NOTICE1C                    PIC X.
           02  NOTICE1H                    PIC X.
           02  NOTICE1O                    PIC X(007).
           02  FILLER                      PIC X(03).
           02  BRANCHC                     PIC X.
           02  BRANCHH                     PIC X.
           02  BRANCHO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  BUYBACC                     PIC X.
           02  BUYBACH                     PIC X.
           02  BUYBACO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  OVERRIC                     PIC X.
           02  OVERRIH                     PIC X.
           02  OVERRIO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  OPENDAC                     PIC X.
           02  OPENDAH                     PIC X.
           02  OPENDAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
