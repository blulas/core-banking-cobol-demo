      *================================================================*
      * COPYBOOK : CUINQS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET CUINQS MAP CUINQM         *
      *            TRANSACTION CB01 - CUSTOMER INQUIRY                 *
      *            GENERATED FROM BMS SOURCE CUINQS - DO NOT EDIT      *
      *================================================================*
       01  CUINQMI.
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
           02  CUSTIDL                     PIC S9(4) COMP.
           02  CUSTIDF                     PIC X.
           02  FILLER REDEFINES CUSTIDF.
               03  CUSTIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CUSTIDI                     PIC X(010).
           02  CNAMEL                      PIC S9(4) COMP.
           02  CNAMEF                      PIC X.
           02  FILLER REDEFINES CNAMEF.
               03  CNAMEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  CNAMEI                      PIC X(040).
           02  CTYPEL                      PIC S9(4) COMP.
           02  CTYPEF                      PIC X.
           02  FILLER REDEFINES CTYPEF.
               03  CTYPEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  CTYPEI                      PIC X(012).
           02  CSTATL                      PIC S9(4) COMP.
           02  CSTATF                      PIC X.
           02  FILLER REDEFINES CSTATF.
               03  CSTATA                  PIC X.
           02  FILLER                      PIC X(02).
           02  CSTATI                      PIC X(012).
           02  CSEGL                       PIC S9(4) COMP.
           02  CSEGF                       PIC X.
           02  FILLER REDEFINES CSEGF.
               03  CSEGA                   PIC X.
           02  FILLER                      PIC X(02).
           02  CSEGI                       PIC X(016).
           02  CSINCEL                     PIC S9(4) COMP.
           02  CSINCEF                     PIC X.
           02  FILLER REDEFINES CSINCEF.
               03  CSINCEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CSINCEI                     PIC X(010).
           02  CADDRL                      PIC S9(4) COMP.
           02  CADDRF                      PIC X.
           02  FILLER REDEFINES CADDRF.
               03  CADDRA                  PIC X.
           02  FILLER                      PIC X(02).
           02  CADDRI                      PIC X(060).
           02  CPHONEL                     PIC S9(4) COMP.
           02  CPHONEF                     PIC X.
           02  FILLER REDEFINES CPHONEF.
               03  CPHONEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CPHONEI                     PIC X(015).
           02  CEMAILL                     PIC S9(4) COMP.
           02  CEMAILF                     PIC X.
           02  FILLER REDEFINES CEMAILF.
               03  CEMAILA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CEMAILI                     PIC X(040).
           02  CDOBL                       PIC S9(4) COMP.
           02  CDOBF                       PIC X.
           02  FILLER REDEFINES CDOBF.
               03  CDOBA                   PIC X.
           02  FILLER                      PIC X(02).
           02  CDOBI                       PIC X(010).
           02  CTAXIDL                     PIC S9(4) COMP.
           02  CTAXIDF                     PIC X.
           02  FILLER REDEFINES CTAXIDF.
               03  CTAXIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CTAXIDI                     PIC X(011).
           02  CRELIDL                     PIC S9(4) COMP.
           02  CRELIDF                     PIC X.
           02  FILLER REDEFINES CRELIDF.
               03  CRELIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CRELIDI                     PIC X(010).
           02  CKYCL                       PIC S9(4) COMP.
           02  CKYCF                       PIC X.
           02  FILLER REDEFINES CKYCF.
               03  CKYCA                   PIC X.
           02  FILLER                      PIC X(02).
           02  CKYCI                       PIC X(016).
           02  CTIERL                      PIC S9(4) COMP.
           02  CTIERF                      PIC X.
           02  FILLER REDEFINES CTIERF.
               03  CTIERA                  PIC X.
           02  FILLER                      PIC X(02).
           02  CTIERI                      PIC X(012).
           02  CKYCRVL                     PIC S9(4) COMP.
           02  CKYCRVF                     PIC X.
           02  FILLER REDEFINES CKYCRVF.
               03  CKYCRVA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CKYCRVI                     PIC X(010).
           02  CRISKL                      PIC S9(4) COMP.
           02  CRISKF                      PIC X.
           02  FILLER REDEFINES CRISKF.
               03  CRISKA                  PIC X.
           02  FILLER                      PIC X(02).
           02  CRISKI                      PIC X(012).
           02  CSCOREL                     PIC S9(4) COMP.
           02  CSCOREF                     PIC X.
           02  FILLER REDEFINES CSCOREF.
               03  CSCOREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CSCOREI                     PIC X(003).
           02  CPEPL                       PIC S9(4) COMP.
           02  CPEPF                       PIC X.
           02  FILLER REDEFINES CPEPF.
               03  CPEPA                   PIC X.
           02  FILLER                      PIC X(02).
           02  CPEPI                       PIC X(001).
           02  CMSBL                       PIC S9(4) COMP.
           02  CMSBF                       PIC X.
           02  FILLER REDEFINES CMSBF.
               03  CMSBA                   PIC X.
           02  FILLER                      PIC X(02).
           02  CMSBI                       PIC X(001).
           02  CEMPLL                      PIC S9(4) COMP.
           02  CEMPLF                      PIC X.
           02  FILLER REDEFINES CEMPLF.
               03  CEMPLA                  PIC X.
           02  FILLER                      PIC X(02).
           02  CEMPLI                      PIC X(001).
           02  COFACL                      PIC S9(4) COMP.
           02  COFACF                      PIC X.
           02  FILLER REDEFINES COFACF.
               03  COFACA                  PIC X.
           02  FILLER                      PIC X(02).
           02  COFACI                      PIC X(016).
           02  COFDTL                      PIC S9(4) COMP.
           02  COFDTF                      PIC X.
           02  FILLER REDEFINES COFDTF.
               03  COFDTA                  PIC X.
           02  FILLER                      PIC X(02).
           02  COFDTI                      PIC X(010).
           02  CRELBALL                    PIC S9(4) COMP.
           02  CRELBALF                    PIC X.
           02  FILLER REDEFINES CRELBALF.
               03  CRELBALA                PIC X.
           02  FILLER                      PIC X(02).
           02  CRELBALI                    PIC X(020).
           02  CMAINTL                     PIC S9(4) COMP.
           02  CMAINTF                     PIC X.
           02  FILLER REDEFINES CMAINTF.
               03  CMAINTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CMAINTI                     PIC X(020).
           02  ALN01L                      PIC S9(4) COMP.
           02  ALN01F                      PIC X.
           02  FILLER REDEFINES ALN01F.
               03  ALN01A                  PIC X.
           02  FILLER                      PIC X(02).
           02  ALN01I                      PIC X(078).
           02  ALN02L                      PIC S9(4) COMP.
           02  ALN02F                      PIC X.
           02  FILLER REDEFINES ALN02F.
               03  ALN02A                  PIC X.
           02  FILLER                      PIC X(02).
           02  ALN02I                      PIC X(078).
           02  ALN03L                      PIC S9(4) COMP.
           02  ALN03F                      PIC X.
           02  FILLER REDEFINES ALN03F.
               03  ALN03A                  PIC X.
           02  FILLER                      PIC X(02).
           02  ALN03I                      PIC X(078).
           02  ALN04L                      PIC S9(4) COMP.
           02  ALN04F                      PIC X.
           02  FILLER REDEFINES ALN04F.
               03  ALN04A                  PIC X.
           02  FILLER                      PIC X(02).
           02  ALN04I                      PIC X(078).
           02  ALN05L                      PIC S9(4) COMP.
           02  ALN05F                      PIC X.
           02  FILLER REDEFINES ALN05F.
               03  ALN05A                  PIC X.
           02  FILLER                      PIC X(02).
           02  ALN05I                      PIC X(078).
           02  ALN06L                      PIC S9(4) COMP.
           02  ALN06F                      PIC X.
           02  FILLER REDEFINES ALN06F.
               03  ALN06A                  PIC X.
           02  FILLER                      PIC X(02).
           02  ALN06I                      PIC X(078).
           02  ALN07L                      PIC S9(4) COMP.
           02  ALN07F                      PIC X.
           02  FILLER REDEFINES ALN07F.
               03  ALN07A                  PIC X.
           02  FILLER                      PIC X(02).
           02  ALN07I                      PIC X(078).
           02  PAGENOL                     PIC S9(4) COMP.
           02  PAGENOF                     PIC X.
           02  FILLER REDEFINES PAGENOF.
               03  PAGENOA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PAGENOI                     PIC X(003).
           02  ACCTCNTL                    PIC S9(4) COMP.
           02  ACCTCNTF                    PIC X.
           02  FILLER REDEFINES ACCTCNTF.
               03  ACCTCNTA                PIC X.
           02  FILLER                      PIC X(02).
           02  ACCTCNTI                    PIC X(004).
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
       01  CUINQMO REDEFINES CUINQMI.
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
           02  CUSTIDC                     PIC X.
           02  CUSTIDH                     PIC X.
           02  CUSTIDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CNAMEC                      PIC X.
           02  CNAMEH                      PIC X.
           02  CNAMEO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  CTYPEC                      PIC X.
           02  CTYPEH                      PIC X.
           02  CTYPEO                      PIC X(012).
           02  FILLER                      PIC X(03).
           02  CSTATC                      PIC X.
           02  CSTATH                      PIC X.
           02  CSTATO                      PIC X(012).
           02  FILLER                      PIC X(03).
           02  CSEGC                       PIC X.
           02  CSEGH                       PIC X.
           02  CSEGO                       PIC X(016).
           02  FILLER                      PIC X(03).
           02  CSINCEC                     PIC X.
           02  CSINCEH                     PIC X.
           02  CSINCEO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CADDRC                      PIC X.
           02  CADDRH                      PIC X.
           02  CADDRO                      PIC X(060).
           02  FILLER                      PIC X(03).
           02  CPHONEC                     PIC X.
           02  CPHONEH                     PIC X.
           02  CPHONEO                     PIC X(015).
           02  FILLER                      PIC X(03).
           02  CEMAILC                     PIC X.
           02  CEMAILH                     PIC X.
           02  CEMAILO                     PIC X(040).
           02  FILLER                      PIC X(03).
           02  CDOBC                       PIC X.
           02  CDOBH                       PIC X.
           02  CDOBO                       PIC X(010).
           02  FILLER                      PIC X(03).
           02  CTAXIDC                     PIC X.
           02  CTAXIDH                     PIC X.
           02  CTAXIDO                     PIC X(011).
           02  FILLER                      PIC X(03).
           02  CRELIDC                     PIC X.
           02  CRELIDH                     PIC X.
           02  CRELIDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CKYCC                       PIC X.
           02  CKYCH                       PIC X.
           02  CKYCO                       PIC X(016).
           02  FILLER                      PIC X(03).
           02  CTIERC                      PIC X.
           02  CTIERH                      PIC X.
           02  CTIERO                      PIC X(012).
           02  FILLER                      PIC X(03).
           02  CKYCRVC                     PIC X.
           02  CKYCRVH                     PIC X.
           02  CKYCRVO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CRISKC                      PIC X.
           02  CRISKH                      PIC X.
           02  CRISKO                      PIC X(012).
           02  FILLER                      PIC X(03).
           02  CSCOREC                     PIC X.
           02  CSCOREH                     PIC X.
           02  CSCOREO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  CPEPC                       PIC X.
           02  CPEPH                       PIC X.
           02  CPEPO                       PIC X(001).
           02  FILLER                      PIC X(03).
           02  CMSBC                       PIC X.
           02  CMSBH                       PIC X.
           02  CMSBO                       PIC X(001).
           02  FILLER                      PIC X(03).
           02  CEMPLC                      PIC X.
           02  CEMPLH                      PIC X.
           02  CEMPLO                      PIC X(001).
           02  FILLER                      PIC X(03).
           02  COFACC                      PIC X.
           02  COFACH                      PIC X.
           02  COFACO                      PIC X(016).
           02  FILLER                      PIC X(03).
           02  COFDTC                      PIC X.
           02  COFDTH                      PIC X.
           02  COFDTO                      PIC X(010).
           02  FILLER                      PIC X(03).
           02  CRELBALC                    PIC X.
           02  CRELBALH                    PIC X.
           02  CRELBALO                    PIC X(020).
           02  FILLER                      PIC X(03).
           02  CMAINTC                     PIC X.
           02  CMAINTH                     PIC X.
           02  CMAINTO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  ALN01C                      PIC X.
           02  ALN01H                      PIC X.
           02  ALN01O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  ALN02C                      PIC X.
           02  ALN02H                      PIC X.
           02  ALN02O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  ALN03C                      PIC X.
           02  ALN03H                      PIC X.
           02  ALN03O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  ALN04C                      PIC X.
           02  ALN04H                      PIC X.
           02  ALN04O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  ALN05C                      PIC X.
           02  ALN05H                      PIC X.
           02  ALN05O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  ALN06C                      PIC X.
           02  ALN06H                      PIC X.
           02  ALN06O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  ALN07C                      PIC X.
           02  ALN07H                      PIC X.
           02  ALN07O                      PIC X(078).
           02  FILLER                      PIC X(03).
           02  PAGENOC                     PIC X.
           02  PAGENOH                     PIC X.
           02  PAGENOO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  ACCTCNTC                    PIC X.
           02  ACCTCNTH                    PIC X.
           02  ACCTCNTO                    PIC X(004).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
           02  FILLER                      PIC X(03).
           02  PFKEYSC                     PIC X.
           02  PFKEYSH                     PIC X.
           02  PFKEYSO                     PIC X(079).
