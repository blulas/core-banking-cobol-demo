      *================================================================*
      * COPYBOOK : AROVRS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET AROVRS MAP AROVRM         *
      *            TRANSACTION AR85 - ACCOUNT RECONCILEMENT            *
      *            GENERATED FROM BMS SOURCE AROVRS - DO NOT EDIT      *
      *================================================================*
       01  AROVRMI.
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
           02  CYCLECL                     PIC S9(4) COMP.
           02  CYCLECF                     PIC X.
           02  FILLER REDEFINES CYCLECF.
               03  CYCLECA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CYCLECI                     PIC X(007).
           02  LASTACL                     PIC S9(4) COMP.
           02  LASTACF                     PIC X.
           02  FILLER REDEFINES LASTACF.
               03  LASTACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTACI                     PIC X(010).
           02  REGIONL                     PIC S9(4) COMP.
           02  REGIONF                     PIC X.
           02  FILLER REDEFINES REGIONF.
               03  REGIONA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REGIONI                     PIC X(002).
           02  PAIDAML                     PIC S9(4) COMP.
           02  PAIDAMF                     PIC X.
           02  FILLER REDEFINES PAIDAMF.
               03  PAIDAMA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PAIDAMI                     PIC X(016).
           02  SOURCEL                     PIC S9(4) COMP.
           02  SOURCEF                     PIC X.
           02  FILLER REDEFINES SOURCEF.
               03  SOURCEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SOURCEI                     PIC X(002).
           02  PENALTL                     PIC S9(4) COMP.
           02  PENALTF                     PIC X.
           02  FILLER REDEFINES PENALTF.
               03  PENALTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PENALTI                     PIC X(010).
           02  LASTMAL                     PIC S9(4) COMP.
           02  LASTMAF                     PIC X.
           02  FILLER REDEFINES LASTMAF.
               03  LASTMAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTMAI                     PIC X(010).
           02  ISSUEDL                     PIC S9(4) COMP.
           02  ISSUEDF                     PIC X.
           02  FILLER REDEFINES ISSUEDF.
               03  ISSUEDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ISSUEDI                     PIC X(016).
           02  NEXTDUL                     PIC S9(4) COMP.
           02  NEXTDUF                     PIC X.
           02  FILLER REDEFINES NEXTDUF.
               03  NEXTDUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEXTDUI                     PIC X(010).
           02  CYCLEC1L                    PIC S9(4) COMP.
           02  CYCLEC1F                    PIC X.
           02  FILLER REDEFINES CYCLEC1F.
               03  CYCLEC1A                PIC X.
           02  FILLER                      PIC X(02).
           02  CYCLEC1I                    PIC X(002).
           02  EFFECTL                     PIC S9(4) COMP.
           02  EFFECTF                     PIC X.
           02  FILLER REDEFINES EFFECTF.
               03  EFFECTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EFFECTI                     PIC X(010).
           02  VOIDYTL                     PIC S9(4) COMP.
           02  VOIDYTF                     PIC X.
           02  FILLER REDEFINES VOIDYTF.
               03  VOIDYTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  VOIDYTI                     PIC X(016).
           02  STOPFLL                     PIC S9(4) COMP.
           02  STOPFLF                     PIC X.
           02  FILLER REDEFINES STOPFLF.
               03  STOPFLA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STOPFLI                     PIC X(001).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  AROVRMO REDEFINES AROVRMI.
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
           02  CYCLECC                     PIC X.
           02  CYCLECH                     PIC X.
           02  CYCLECO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  LASTACC                     PIC X.
           02  LASTACH                     PIC X.
           02  LASTACO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  REGIONC                     PIC X.
           02  REGIONH                     PIC X.
           02  REGIONO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  PAIDAMC                     PIC X.
           02  PAIDAMH                     PIC X.
           02  PAIDAMO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  SOURCEC                     PIC X.
           02  SOURCEH                     PIC X.
           02  SOURCEO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  PENALTC                     PIC X.
           02  PENALTH                     PIC X.
           02  PENALTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  LASTMAC                     PIC X.
           02  LASTMAH                     PIC X.
           02  LASTMAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  ISSUEDC                     PIC X.
           02  ISSUEDH                     PIC X.
           02  ISSUEDO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NEXTDUC                     PIC X.
           02  NEXTDUH                     PIC X.
           02  NEXTDUO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CYCLEC1C                    PIC X.
           02  CYCLEC1H                    PIC X.
           02  CYCLEC1O                    PIC X(002).
           02  FILLER                      PIC X(03).
           02  EFFECTC                     PIC X.
           02  EFFECTH                     PIC X.
           02  EFFECTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  VOIDYTC                     PIC X.
           02  VOIDYTH                     PIC X.
           02  VOIDYTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  STOPFLC                     PIC X.
           02  STOPFLH                     PIC X.
           02  STOPFLO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
