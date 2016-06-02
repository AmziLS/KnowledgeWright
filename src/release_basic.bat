mkdir \kw4
mkdir \kw4\workshop
mkdir \kw4\workshop\jigs
mkdir \kw4\workshop\jre
mkdir \kw4\workshop\lib
mkdir \kw4\docs
mkdir \kw4\docs\images
mkdir \kw4\docs\jigs
mkdir \kw4\docs\jigs\basic
mkdir \kw4\docs\jigs\support
mkdir \kw4\docs\runtime
mkdir \kw4\docs\workshop
mkdir \kw4\docs\overview
mkdir \kw4\docs\overview\sales
mkdir \kw4\samples
mkdir \kw4\samples\basic
mkdir \kw4\samples\basic\tutorial
mkdir \kw4\samples\support
rem mkdir \kw4\samples\sales
mkdir \kw4\runtime
mkdir \kw4\runtime\vb
mkdir \kw4\runtime\cgi
mkdir \kw4\runtime\cgi\acgi
mkdir \kw4\runtime\java
mkdir \kw4\runtime\java\amzi
mkdir \kw4\runtime\java\amzi\ls
mkdir \kw4\runtime\delphi
mkdir \kw4\temp

copy temp\empty.kws \kw4\temp\

copy %AMZI_DIR%\bin\amzi.dll \kw4\workshop
copy %AMZI_DIR%\bin\amzijni.dll \kw4\workshop
copy %AMZI_DIR%\bin\aosutils.lsx \kw4\workshop
copy %AMZI_DIR%\bin\aodbc.lsx \kw4\workshop

copy ..\workshop\amzi.cfg \kw4\workshop\
copy ..\workshop\kw.bat \kw4\workshop\
copy ..\workshop\kw_orig.cfg \kw4\workshop\kw.cfg
copy ..\workshop\kw.jar \kw4\workshop\
copy ide\kwauthor.xpl \kw4\workshop\
copy ..\workshop\lib\*.jar \kw4\workshop\lib

copy ..\workshop\jigs\basic.jig \kw4\workshop\jigs\
copy ide\jigs\basic.xpl \kw4\workshop\jigs\
copy ..\workshop\jigs\support.jig \kw4\workshop\jigs\
copy ide\jigs\support.xpl \kw4\workshop\jigs\
rem copy ide\jigs\sales.jig \kw4\workshop\jigs\
rem copy ide\jigs\sales.xpl \kw4\workshop\jigs\

rem xcopy ..\workshop\jre \kw4\workshop\jre /S

cd docs\jigs
copy fig_basic.pro fig.pro
arun \amzi\dev\a6\src\docs\htmlidx\htmlidx
copy fig_support.pro fig.pro
arun \amzi\dev\a6\src\docs\htmlidx\htmlidx
cd ..\runtime
arun \amzi\dev\a6\src\docs\htmlidx\htmlidx
cd ..\workshop
arun \amzi\dev\a6\src\docs\htmlidx\htmlidx
cd ..
cd ..

copy docs\*.html \kw4\docs
copy docs\*.png \kw4\docs
copy docs\images\*.* \kw4\docs\images\
copy docs\jigs\*.html \kw4\docs\jigs\
copy docs\jigs\*.png \kw4\docs\jigs\
copy docs\jigs\basic\*.html \kw4\docs\jigs\basic\
copy docs\jigs\basic\*.png \kw4\docs\jigs\basic\
copy docs\jigs\support\*.html \kw4\docs\jigs\support\
copy docs\jigs\support\*.png \kw4\docs\jigs\support\
copy docs\overview\*.html \kw4\docs\overview\
copy docs\overview\*.png \kw4\docs\overview\
rem copy docs\overview\sales\*.html \kw4\docs\overview\sales\
rem copy docs\overview\sales\*.png \kw4\docs\overview\sales\
copy docs\runtime\*.html \kw4\docs\runtime\
copy docs\runtime\*.png \kw4\docs\runtime\
copy docs\workshop\*.html \kw4\docs\workshop\
copy docs\workshop\*.png \kw4\docs\workshop\

copy samples\*.kb \kw4\samples\
copy samples\basic\*.kb \kw4\samples\basic\
copy samples\basic\tutorial\*.kb \kw4\samples\basic\tutorial\
copy samples\basic\tutorial\*.mdb \kw4\samples\basic\tutorial\
copy samples\support\*.kb \kw4\samples\support
rem copy samples\sales\*.kb \kw4\samples\sales\

rem copy ..\workshop\amzi.dll \kw4\runtime\vb\
rem copy ..\workshop\amzi.cfg \kw4\runtime\vb\
rem copy ..\workshop\aosutils.lsx \kw4\runtime\vb\
rem copy ..\workshop\aodbc.lsx \kw4\runtime\vb\
copy vb_run\vb_run.exe \kw4\runtime\vb\
copy vb_run\vb_run.vbp \kw4\runtime\vb\
copy vb_run\*.frm \kw4\runtime\vb\
rem copy vb_run\*.dll \kw4\runtime\vb\
copy vb_run\amzi.bas \kw4\runtime\vb\
rem copy \amzi\dev\a6\lsapis\vb\amzi.bas \kw4\runtime\vb
rem copy ..ide\jigs\basic.xpl \kw4\runtime\vb\
rem copy samples\basic\tutorial\shapes.kb \kw4\runtime\vb\

copy cgi_run\kwcgirun.exe \kw4\runtime\cgi\kwcgibasic.exe
copy ..\workshop\jigs\basic.xpl \kw4\runtime\cgi\kwcgibasic.xpl
cd cgi_run
acmp kwcgirun.pro
cd ..
copy cgi_run\kwcgirun.plm \kw4\runtime\cgi\
copy cgi_run\kwcgirun.pro \kw4\runtime\cgi\
copy cgi_run\kwtestcgi.exe \kw4\runtime\cgi\
copy cgi_run\acgi\amzi*.c \kw4\runtime\cgi\acgi\
copy cgi_run\acgi\makefile.* \kw4\runtime\cgi\acgi\
copy cgi_run\acgi\wacgi.dsw \kw4\runtime\cgi\acgi\
copy cgi_run\acgi\wacgi.dsp \kw4\runtime\cgi\acgi\

xcopy %AMZI_DIR%\lsapis\java20\amzi java_run
copy java_run\Application.java \kw4\runtime\java\
copy java_run\AskFieldDialog.java \kw4\runtime\java\
copy java_run\AskMenuDialog.java \kw4\runtime\java\
copy java_run\MainFrame.java \kw4\runtime\java\
copy java_run\Utils.java \kw4\runtime\java\
copy java_run\java_run.jpx \kw4\runtime\java\
copy %AMZI_DIR%\lsapis\java20\amzi\ls\*.class \kw4\runtime\java\amzi\ls\

copy delphi_run\delphi_run.dpr \kw4\runtime\delphi\
copy delphi_run\delphi_run.res \kw4\runtime\delphi\
copy delphi_run\delphi_run.exe \kw4\runtime\delphi\
copy delphi_run\main.pas \kw4\runtime\delphi\
copy delphi_run\main.dfm \kw4\runtime\delphi\
copy delphi_run\askquestion.pas \kw4\runtime\delphi\
copy delphi_run\askquestion.dfm \kw4\runtime\delphi\
copy %AMZI_DIR%\lsapis\delphi\amzi.pas \kw4\runtime\delphi\
