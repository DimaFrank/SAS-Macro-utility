/**=============================================================================
-------------------------------------------------------------------------------
                             Program Information
-------------------------------------------------------------------------------
 Program                : CmprDashBrd.sas (Version 1)
 Brief Description      : Compares permanent PRODUCTION and QC data set based on the list
                          provided. This macro will compare one or several set of data sets and
                          will generate a report indicating whether the data set passed or
                          failed the PROC COMPARE. If a COMPARE failed, this report will indicate
                          due to what reason the compare failed.
                          If a PRODUCTION date is > QC date, QC has not done the FINAL run
                          after PRODUCTION. QC should then re-run the program and re-validate.
                          This utility will perform the date comparison if permanent data set
                          is used as opposed to temporary (WORK) data set.
                          Variable number ordering of the data set can also be compared if analysis
                          files are compared. This ordering may not be relevant when QCing
                          electronic outputs and this option can be suppressed (variable order will
                          not be checked if it is not relevant: _VarNum=No).
                          When comparing character variables, compare can be done "as is" when comparing
                          analysis files or by compressing blanks (or ignore blanks) when comparing
                          outputs. More than one character can be compressed if needed.
 Notes / Assumptions    :

 Required Parameters:
    _DSET   : Dataset containing the information on the list of data sets to compare
    _PROGDIR: Programming Directory minus the Root directory
    _File:    File name used to run this macro
    _Root:    Root directory (_ROOT and _PROGDIR together are used only for footnote)
             (_Root can be abbreviated if needed)
    _Title1 =%str(Sponsor ABCD Inc.)
    _Title2 =%str(Protocol A0000001)
             (Only two titles are allowed)
    _CmpresC=%str() : in this case, it is null and comparing will be done "as is"
                      if %str(@) is used "@" will be ignored,
                      if %str( ) is used spacing will be ignored
                      NULL is default
    _Criter =%str() : to be used in "PROC COMPARE" for CRITERION= option.
                      When _Criter=%Str(), PROC COMPARE default is used.
                      When _Criter=0.000000001, (for example), then "PROC COMPARE" compares the
                            absolute relative difference to the value specified (say 0.000000001)
    _VarNum=Yes     : if Yes is used, Variable order of creation will also be compared.
                      Default it No and this will not be compared.

 Usage Examples:
    When using the code below remove the asterisk after the percentage symbol

    %*CmprDashbrd(_Dset=%str(WORK.PREP),
                  _Root   =%str(SponsorABCD\Compound\Study\BIOSTATISTICS),
                  _PROGDIR=%str(qc\Tables),
                  _File   =%str(RunCompare),
                  _Title1 =%str(Sponsor ABCD Inc.),
                  _Title2 =%str(Protocol A0000001),
                  _CmpresC=%str(),
                  _Criter =%str(),
                  _VarNum =No
                  ) ;
    See example program "RunCompare.sas" regarding how to use this macro
==============================================================================**/
%macro CmprDashbrd(_Dset   =%str(WORK.PREP),
                   _Root   =%str(StudyRootDirectoryGoesHere),
                   _PROGDIR=%str(ProgrammingDirectory),
                   _File   =%str(RunCompare),
                   _Title1 =%str(Sponsor ABCD Inc.),
                   _Title2 =%str(Protocol A0000001),
                   _CmpresC=%str(),_Criter=%str(),_VarNum=No) ;
%let ERR = %str(Err)%str(or) ;

proc format ;
 value CmpRslt  1 = "Data set labels differ                         "  /* 0000000000000001 DSLABEL */
                2 = "Data set types differ                          "  /* 0000000000000010 DSTYPE  */
                3 = "Variable has different informat                "  /* 0000000000000100 INFORMAT*/
                4 = "Variable has different format                  "  /* 0000000000001000 FORMAT  */
                5 = "Variable has different length                  "  /* 0000000000010000 LENGTH  */
                6 = "Variable has different label                   "  /* 0000000000100000 LABEL   */
                7 = "Base data set has observation not in comparison"  /* 0000000001000000 BASEOBS */
                8 = "Comparison data set has observation not in base"  /* 0000000010000000 COMPOBS */
                9 = "Base data set has BY group not in comparison   "  /* 0000000100000000 BASEBY  */
               10 = "Comparison data set has BY group not in base   "  /* 0000001000000000 COMPBY  */
               11 = "Base data set has variable not in comparison   "  /* 0000010000000000 BASEVAR */
               12 = "Comparison data set has variable not in base   "  /* 0000100000000000 COMPVAR */
               13 = "A value comparison was unequal                 "  /* 0001000000000000 VALUE   */
               14 = "Conflicting variable types                     "  /* 0010000000000000 TYPE    */
               15 = "BY variables do not match                      "  /* 0100000000000000 BYVAR   */
               16 = "Fatal &ERR.: comparison not done               "  /* 1000000000000000 ERR-OR  */
;
 value CmpVar   1 = "DSLABEL "
                2 = "DSTYPE  "
                3 = "INFORMAT"
                4 = "FORMAT  "
                5 = "LENGTH  "
                6 = "LABEL   "
                7 = "BASEOBS "
                8 = "COMPOBS "
                9 = "BASEBY  "
               10 = "COMPBY  "
               11 = "BASEVAR "
               12 = "COMPVAR "
               13 = "VALUE   "
               14 = "TYPE    "
               15 = "BYVAR   "
               16 = "%upcase(&ERR.)"
;
run ;
**************************************************************************************************;
* If "PROC COMPARE" CRITERION option need to be used ;
%global CRITERION ;
%if "&_Criter."="%str()" %then
   %do ;
    %let CRITERION=;
   %end ;
   %else
   %do ;
     %let CRITERION=CRITERION=&_Criter. ;
   %end ;
**************************************************************************************************;
%macro DateLbl ;
* To Check Whether PRODUCTION data set date is greater than QC data set date
  and extract data set label ;
%global spdate qcdate ComprVar SPDstLbl QCDstLbl ;
%let spdate= ;
%let qcdate= ;
%let SPDstLbl=; * Dataset Label for PROD ;
%let QCDstLbl=; * Dataset Label for QC   ;

%let ComprVar= ; * If there is any difference in the variable order creation between the two data sets ;
proc sql noprint ;
 select MODATE into :SPDATE
 from sashelp.vtable
 where upcase(LIBNAME) = "%upcase(&ProdLib.)" and upcase(MEMNAME) = "%upcase(&ProdDset.)"
 ;

 select MODATE into :QCDATE
 from sashelp.vtable
 where upcase(LIBNAME) = "%upcase(&QcLib.)" and upcase(MEMNAME) = "%upcase(&QcDset.)"
 ;
quit ;

data _null_ ;
 set sashelp.vtable ;
where upcase(LIBNAME) = "%upcase(&ProdLib.)" and upcase(MEMNAME) = "%upcase(&ProdDset.)" ;
call symputx('SPDstLbl',MEMLABEL) ;
run ;

data _null_ ;
 set sashelp.vtable ;
where upcase(LIBNAME) = "%upcase(&QcLib.)" and upcase(MEMNAME) = "%upcase(&QcDset.)" ;
call symputx('QCDstLbl',MEMLABEL) ;
run ;

%if %superq(SPDstLbl) ne  %then %let SPDstLbl=(Label="%superq(SPDstLbl)") ;
%if %superq(QCDstLbl) ne  %then %let QCDstLbl=(Label="%superq(QCDstLbl)") ;
%mend  DateLbl ;

%macro compare_(ProdLib=,QcLib=,ProdDset=,QcDset=) ;
%DateLbl;
* If production or qc data set does not exist ;
%if &SPDATE. eq or &QCDATE. eq %then
    %do ;
      %let ComprC=32768; *** assign return code ;
    %end ;
    %else
    %do ;
     %if "&_CmpresC."="%str()" %then
     %do ;
      %put %str($)%str($): No characters compressed - Compare "as is" ;
      proc compare base=&QcLib..&QcDset compare=&ProdLib..&ProdDset &CRITERION. noprint ; run ;
      %let ComprC=&sysinfo; *** assign return code ;
     %end ;
     %else
     %do ;
       %put %str($)%str($): Some Characters are ignored ;
      data qc_&QcDset %superq(QCDstLbl);
       length _x_x_J_C $1. ;
       set &QcLib..&QcDset ;
       _x_x_J_C = ' ' ;
       array _x_x_ {*} _character_;
       do _x_x_J = 1 to dim(_x_x_);
          %if "&_CmpresC."="%str()" %then %put %str($)%str($): Macro var _CmpresC is NULL. Compared "as is" ;
            %else
            %do ;
              %put %str($)%str($): Macro var _CmpresC is not NULL. Compare ignores ->&_CmpresC.<- ;
               _x_x_{_x_x_J} = compress(_x_x_{_x_x_J},"&_CmpresC.") ;
               _x_x_{_x_x_J} = trim(_x_x_{_x_x_J}) ;
            %end ;
       end;
      drop _x_x_J_C _x_x_J ;
      run ;

      data sp_&ProdDset %superq(SPDstLbl);
       length _x_x_J_C $1. ;
       set &ProdLib..&ProdDset ;
       _x_x_J_C = ' ' ;
       array _x_x_ {*} _character_;
       do _x_x_J = 1 to dim(_x_x_);
          %if "&_CmpresC."="%str()" %then %put %str($)%str($): Macro var _CmpresC is NULL. Compared "as is" ;
            %else
            %do ;
              %put %str($)%str($): Macro var _CmpresC is not NULL. Compare ignores ->&_CmpresC.-< ;
               _x_x_{_x_x_J} = compress(_x_x_{_x_x_J},"&_CmpresC.") ;
               _x_x_{_x_x_J} = trim(_x_x_{_x_x_J}) ;
            %end ;
       end;
      drop _x_x_J_C _x_x_J ;
      run ;

      proc compare base=qc_&QcDset compare=sp_&ProdDset &CRITERION. noprint ; run ;
      %let ComprC=&sysinfo; *** assign return code ;
     %end ;


      %if %upcase(&_VarNum)=YES %then %do ;
       proc contents data = &QcLib..&QcDset
                     out = qc_&QcDset._(keep=name varnum)   noprint ; run ;
       proc contents data = &ProdLib..&ProdDset
                     out = sp_&ProdDset._(keep=name varnum) noprint ; run ;
       proc sort data = qc_&QcDset._   ; by varnum ; run ;
       proc sort data = sp_&ProdDset._ ; by varnum ; run ;

       proc compare base=qc_&QcDset._ compare=sp_&ProdDset._ noprint ; run ;
       %let ComprVar=&sysinfo; *** assign return code for Compare Variable order ;

       proc datasets library=work nodetails nolist memtype=data;
        delete qc_&QcDset sp_&ProdDset qc_&QcDset._ sp_&ProdDset._ ;
       run ;
      %end ;
    %end ;

data temp ;
ComprC=&ComprC;
cmprcbin=put(ComprC,binary16.);
* If production or qc data set does not exist ;
%if &SPDATE. eq or &QCDATE. eq %then
    %do ;
      PROD_GT  = . ;
    %end ;
    %else
    %do ;
      spdate = "&SpDate."dt ;
      qcdate = "&QcDate."dt ;
      format spdate QcDate datetime16. ;
      if spdate > qcdate > . then PROD_GT  = 1 ;
    %end ;
run ;

data temp ;
 length CmpRsltC $200. OutCome $4. DsetName DsetLib $100.;
 array M{*} DSLABEL DSTYPE INFORMAT FORMAT LENGTH LABEL BASEOBS COMPOBS BASEBY
            COMPBY BASEVAR COMPVAR VALUE TYPE BYVAR ERR ;
 set temp ;
DsetName = "&ProdDset." ;
DsetLib  = "&ProdLib." ;
do i = 1 to 16 ;
   _value = input(substr(cmprcbin,17-i,1),??best32.) ;
   CmpRsltC = put(I,CmpRslt.) ;
   if _value = 1 then M[I] = 1 ;
   HasERR = input(cmprcbin,best32.) ;
   if PROD_GT = 1 then HasERR = 1 ;
   if HasERR = 0 and PROD_GT = . then HasERR = 0 ;
   if HasERR = 0 then Outcome = "Pass" ;
      else OutCome = "Fail" ;

end ;
run ;

data temp ;
 set temp ;
%if &ComprVar.=%str() %then
          %do ;
           Varnum = .N ;
          %end ;
    %else %if &ComprVar.=0 %then
          %do ;
           Varnum = . ;
          %end ;
    %else
          %do ;
           Varnum = 1 ;
          %end ;
%if &SPDATE. eq or &QCDATE. eq %then
    %do ;
           Varnum = . ; * Set to Null if any one data set does not exist ;
    %end ;

if HasERR = 0 and NOT (Varnum eq 1) then HasERR = 0 ;
   else HasERR = 1 ;

if HasERR = 0 then Outcome = "Pass" ;
   else OutCome = "Fail" ;
run ;

data compare ;
 set compare temp ;
run ;
%mend compare_ ;

* Creating an empty data set ;
data compare ;
if _n_ =  1 then delete ;
run ;
****************************************************************;
* Reading the prepared file and run the compares ;
data _null_ ;
 set &_Dset. END=EOF ;
call symputx('PLib'||compress(put(_n_,8.)),trim(left(ProdLib))) ;
call symputx('QLib'||compress(put(_n_,8.)),trim(left(QcLib)))   ;
call symputx('PDst'||compress(put(_n_,8.)),trim(left(ProdDset)));
call symputx('QDst'||compress(put(_n_,8.)),trim(left(QcDset)))  ;
if EOF=1 then call symputx('TotFile',trim(left(put(_n_,8.))))   ;
run ;

%macro _run ;
  %do i = 1 %to &Totfile. ;
    %put I=&I.
         PLib&I.=&&PLib&I.
         QLib&I.=&&QLib&I.
         PDst&I.=&&PDst&I.
         QDst&I.=&&QDst&I. ;
    %compare_(ProdLib=&&PLib&I.,QcLib=&&QLib&I.,ProdDset=&&PDst&I.,QcDset=&&QDst&I.) ;
  %end ;
%mend  _run ;
%_run ;

data compare ;
 set compare ;
* Pull RunOrder from &_Dset. ;
RunOrder = _n_ ;
run ;

proc sort data = compare ; by OutCome RunOrder ; run ;
**************************************************************************************************;
Proc format;
 Value $PassTL " "   = "    "
               "Pass"="Green"
               "Fail"="Red"
               Other ="Black"
               ;
 value ChkTL 1 ="Red"
             ;
 Value Chk   1 ="X"
            .N ="NA"
             ;
run ;
****************************************************************;
*         Template, Title, Footnote and Proc Print              ;
%let f8_  = ^S={font=('COURIER NEW',8pt)};
%let f8b_ = ^S={font=('COURIER NEW',8pt,bold)};
%let f10b_= ^S={font=('COURIER NEW',10pt,Bold italic)};
%let f12b_= ^S={font=('COURIER NEW',12pt,bold)};

options nocenter byline orientation=landscape nonumber nodate missing = ' ' ls = 132 ;
ODS ESCAPECHAR = '^';

proc template;
 DEFINE style MYRTFSTYLE; * create a new custom style named MYRTFSTYLE ;
 PARENT=styles.rtf; * copy the SAS default style template definition for RTF;
 * override the font definition of the parent ;
   replace fonts /
           'TitleFont2'          = ("Courier New, Courier",8pt,Bold Italic)
           'TitleFont'           = ("Courier New, Courier",8pt,Bold Italic)
           'StrongFont'          = ("Courier New, Courier",8pt,Bold)
           'EmphasisFont'        = ("Courier New, Courier",8pt,Italic)
           'FixedEmphasisFont'   = ("Courier New, Courier",8pt,Italic)
           'FixedStrongFont'     = ("Courier New, Courier",8pt,Bold)
           'FixedHeadingFont'    = ("Courier New, Courier",8pt,Bold)
           'BatchFixedFont'      = ("SAS Monospace, Courier New, Courier",8pt)
           'FixedFont'           = ("Courier New, Courier",8pt)
           'headingEmphasisFont' = ("Courier New, Courier",8pt,Bold Italic)
           'headingFont'         = ("Courier New, Courier",8pt,Bold)
           'docFont'             = ("Courier New, Courier",8pt);

   replace Body from Document
      "Undef margins so we get the margins from the printer or SYS option" /
      bottommargin  = 0.50in
      topmargin     = 0.50in
      rightmargin   = 1.00in
      leftmargin    = 1.00in
      pagebreakhtml = html('PageBreakLine');

style systemtitle from systemtitle
      "Style element responsible for titles " / asis=on;
style systemfooter from systemfooter
      "Style element responsible for footnotes " / asis=on;
end;

proc template;
 DEFINE style NOGRIDLINE ; * create a new custom style named MYRTFSTYLE ;
 PARENT=MYRTFSTYLE; * copy the SAS default style template definition for RTF;

       style Table from output /
             background=_undef_
             Rules=none
             Frame=void;
       style header from header /
             background=_undef_;
       style rowheader from rowheader /
             background=_undef_;
end;
**************************************************************************************************;
title1 j=l "&f8_.&_Title1."       j=r "&f8_.Production" ;
title2 j=l "&f8_.&_Title2."       j=r "&f8_.Page ^{pageof}" ;
%if "&_CmpresC."="%str()" %then
    %do ;
      title3 j=l "Compare is done 'as is' when comparing character variables. Macro variable VARNUM=&_VarNum.. " ;
    %end ;
    %else
    %do ;
      title3 j=l "Compare will ignore characters within left and right arrow ->&_CmpresC.<- when comparing. Macro variable VARNUM=&_VarNum.. " ;
    %end ;
%if "&_Criter."="%str()" %then
    %do ;
      title4 j=l "Default CRITERION (0.00001) is used in 'PROC COMPARE'" ;
    %end ;
    %else
    %do ;
      title4 j=l "CRITERION=&_Criter. option is used in 'PROC COMPARE'" ;
    %end ;
title5 j=c "&f10b_.Compare Result" ;

%global footer sorcdt ;
data _null_;
  length temptm1 $5 tempdt1 $9 tempdt2 $11 tempv $17;
  _dttm = PUT(datetime(),DATETIME13.);
  temptm1=SUBSTR(LEFT(_dttm),9,5);
  tempdt1=LEFT(PUT("&sysdate9."d,DATE9.));
  tempdt2=UPCASE(SUBSTR(tempdt1,1,2)||'-'||SUBSTR(tempdt1,3,3)||'-'||SUBSTR(tempdt1,6,4));
  tempv=tempdt2||' '||temptm1;
  CALL SYMPUT("sorcdt",tempv);
run;
%let footer=%STR(Source:  [&_Root.\&_progdir] %upcase(&_file).SAS, QUINTILES %(US%));
****************************************************************;
ods listing close ;
ods rtf style=MYRTFSTYLE file="&_File..rtf" ;
proc print data = compare style(data header obs)={cellwidth=0.30in just=c} split='*' ;
  var OutCome  / style=[just=left foreground=$PassTL. cellwidth=0.65in];
  var RunOrder    ;
  var DsetLib  / style=[just=left cellwidth=0.95in];
  var DsetName / style=[just=left cellwidth=1.15in];
  var DSLABEL  / style=[foreground=ChkTL.];
  var DSTYPE   / style=[foreground=ChkTL.];
  var INFORMAT / style=[foreground=ChkTL.];
  var FORMAT   / style=[foreground=ChkTL.];
  var LENGTH   / style=[foreground=ChkTL.];
  var LABEL    / style=[foreground=ChkTL.];
  var BASEOBS  / style=[foreground=ChkTL.];
  var COMPOBS  / style=[foreground=ChkTL.];
  var BASEBY   / style=[foreground=ChkTL.];
  var COMPBY   / style=[foreground=ChkTL.];
  var BASEVAR  / style=[foreground=ChkTL.];
  var COMPVAR  / style=[foreground=ChkTL.];
  var VALUE    / style=[foreground=ChkTL.];
  var TYPE     / style=[foreground=ChkTL.];
  var BYVAR    / style=[foreground=ChkTL.];
  var ERR      / style=[foreground=ChkTL.];
  var PROD_GT  / style=[foreground=ChkTL.];
  var Varnum   / style=[foreground=ChkTL.];
  format DSLABEL DSTYPE INFORMAT FORMAT LENGTH LABEL BASEOBS COMPOBS BASEBY
            COMPBY BASEVAR COMPVAR VALUE TYPE BYVAR ERR PROD_GT Varnum chk. ;
label RunOrder = "R*u*n*O*r*d*e*r"
      DsetLib  = "PROD Libref    "
      DsetName = "PROD Dset Name "
      DSLABEL  = "D*S*L*A*B*E*L  "
      DSTYPE   = "D*S*T*Y*P*E*   "
      INFORMAT = "I*N*F*O*R*M*A*T"
      FORMAT   = "F*O*R*M*A*T*   "
      LENGTH   = "L*E*N*G*T*H*   "
      LABEL    = "L*A*B*E*L      "
      BASEOBS  = "B*A*S*E*O*B*S  "
      COMPOBS  = "C*O*M*P*O*B*S  "
      BASEBY   = "B*A*S*E*B*Y*   "
      COMPBY   = "C*O*M*P*B*Y*   "
      BASEVAR  = "B*A*S*E*V*A*R  "
      COMPVAR  = "C*O*M*P*V*A*R  "
      VALUE    = "V*A*L*U*E      "
      TYPE     = "T*Y*P*E        "
      BYVAR    = "B*Y*V*A*R      "
      ERR      = "E*R*R*O*R      "
      PROD_GT  = "P*R*O*D*_*G*T  "
      VARNUM   = "V*A*R*N*U*M    "
      ;
footnote1  j=l "DSLABEL : Data set labels differ                         , BASEBY  : Base data set has BY group not in comparison" ;
footnote2  j=l "DSTYPE  : Data set types differ                          , COMPBY  : Comparison data set has BY group not in base" ;
footnote3  j=l "INFORMAT: Variable has different informat                , BASEVAR : Base data set has variable not in comparison" ;
footnote4  j=l "FORMAT  : Variable has different format                  , COMPVAR : Comparison data set has variable not in base" ;
footnote5  j=l "LENGTH  : Variable has different length                  , VALUE   : A value comparison was unequal              " ;
footnote6  j=l "LABEL   : Variable has different label                   , TYPE    : Conflicting variable types                  " ;
footnote7  j=l "BASEOBS : Base data set has observation not in comparison, BYVAR   : BY variables do not match                   " ;
footnote8  j=l "COMPOBS : Comparison data set has observation not in base, &ERR.   : Fatal &ERR.: comparison not done            " ;
footnote9  j=l "PROD_GT : Production data set Date > QC, re-run QC       , VARNUM  : Variable order (NA means Not applicable)    " ;
footnote10 j=l height=0.25 "&footer &sorcdt";
run ;
**************************************************************************************************;
ods rtf close ;
ods listing ;
%mend CmprDashbrd ;