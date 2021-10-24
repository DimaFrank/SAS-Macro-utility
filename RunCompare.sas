/**=============================================================================
 Program                : RunCompare.sas
 Brief Description      : Create a list of data sets to compare and run the macro
                          CmprDashBrd.sas
 Raw Data Sets Used     : N/A
 Derived Data Sets Used : N/A
 Data Set Created       : N/A
 Output Files           : N/A
 Notes / Assumptions    : Create a work file containing the following variables
                          ORDER:    Order in which the list of compare should be done
                          ProdLib:  Lib name for PRODUCTION
                          QCLib:    Lib name for QC
                          ProdDset: Dataset name for Production
                          QcDset:   Dataset name for QC
                          For the usage of the macro CmprDashBrd, refer to the
                          program CmprDashBrd.sas
==============================================================================**/
* Simulated data sets are temporary (WORK). Utility should be used on permanent SAS data sets ;
*++++++++++++++++++++++++++;
data class1(label="Student Data") ;
 retain Age ;
 set sashelp.class ;
if height = 69 then height = height + 0.000000001 ;
run ;
*++++++++++++++++++++++++++;
data PROD ;
 set sashelp.cars(drop=origin) ;
if Type='SUV' then Type='  SUV' ;
format type $15. ;
informat type $15. ;
label Type = 'TYPE' ;
run ;

data qc(label="Unbalanced Quotes:'(") ;
 set sashelp.cars(drop=msrp) ;
run ;
*++++++++++++++++++++++++++;
data prep(where=(order>0));
 infile datalines dsd;
 input order ProdLib :$8. QCLib :$8. ProdDset :$30. QcDset :$30. ;
datalines;
1,work,sashelp,class1,class
1,sashelp,work,class,class1
1,sashelp,sashelp,cars,cars
1,work,work,NotExist,NotExist
1,work,work,prod,qc
;
run ;
%include "CmprDashBrd.sas" ;

**************************************************************************;
* In the example 1 to 4, _File uses different name. It is good practice to use one program to generate
  one output. Also this ensures the SAS file name and the RTF file name are same. ;
* Example 1 ;
%CmprDashbrd(_Dset=%str(WORK.PREP),
             _Root   =%str(StudyRootDirectoryGoesHere),
             _PROGDIR=%str(ProgrammingDirectory),
             _File   =%str(RunCompare1),
             _Title1 =%str(Sponsor ABCD Inc.),
             _Title2 =%str(Protocol A0000001),
             _CmpresC=%str(),
             _Criter =%str(),
             _VarNum=Yes
              ) ;

* Example 2 ;
%CmprDashbrd(_File   =%str(RunCompare2),
             _CmpresC=%str(),
             _Criter =%str(0.000000001),
             _VarNum=Yes
              ) ;

* Example 3 ;
%CmprDashbrd(_File   =%str(RunCompare3),
             _CmpresC=%str( ),
             _Criter =%str(),
             _VarNum=Yes
              ) ;

* Example 4 ;
%CmprDashbrd(_File   =%str(RunCompare4),
             _CmpresC=%str( ),
             _Criter =%str(0.000000001),
             _VarNum=No
              ) ;