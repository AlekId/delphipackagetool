Delphi Package Tool
*******************
NOTE: THIS TOOL DOES NOT CHANGE ANY SOURCE FILES OF YOUR PROJECT!

Who can use this Tool ?
***********************
1) Everyone who has taken a backup of his sources!
2) You can try to compile your projects/packages with different compiler versions, but this is only possible if
   you have the full source code of all your components and packges! 

Why shall I use this Tool ?
***************************

The DelphiPackageTool supports you when working with Delphi Package/Project Group Files (.bpg-Files):
If you have serveral Packages and Projects and you want to recompile/install in one go, then
this Tool may save you a lot of time.

Two disadvantages of the Delphi IDE can be avoided when using DelphiPackgeTool:

1. Bug in the Delphi IDE.
If you open a big project (.bpg) and Press <Rebuild All Projects> in the Delphi IDE, then for some reason
not all memory is released by the IDE. This means you have to close Delphi and start again. This is not very nice.

2. The Delphi IDE has no function to install all packages automatically.
DelphiPackageTool compiles and installes all packages listed in the bpg-file.


Features:
- compile all .dpr,.dpk projects of a Delphi Package Group.(bpg)
- installs Designtime Packages automatically into the Delphi IDE. 
- can also be used as command line tool. ( for automatic rebuild of your projects).
- can rebuild projects from several different delphi versions.

Licence:
The DelphiPackageTool is Freeware!
You use this Tool at your own Responsibility!
SOURCECODE IS NOT AVAILABLE.

Autor:
Samuel Herzog, sam_herzog@yahoo.com


Command Line Examples:
**********************
The DelphiPackageTool return value is 0 if every thing compiled ok.
If an error happens, the return value will be 1.
Additionally also a log-file is created which contains the compiler output messages.

Example 1:
The following line recompiles all projects and packages and installs the packages into the IDE.

DelphiPackageTool.exe –o”myComponents.ini” –rebuild

The following steps are done by the DelphiPackageTool.

1. Delete Registry Entries. (This removes the Components from the IDE)
2. bpl/dcp will be deleted.
3. bpl/dcp will be recompiled.
4. Write Registry Entries. (Components will be installed)

Example 2:
The following line demonstrates how to compile and install <Package1.dpk>

DelphiPackageTool.exe -p"C:\Projects\Packages\OwnComp\Package1.dpk" -install


*************************************************************************************************************
IMPORTANT: Packages can only be re-compiled and re-installed if the Delphi IDE is CLOSED!
*************************************************************************************************************