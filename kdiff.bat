if "%1"=="" goto diffAll
"c:\Program Files\KDiff3\kdiff3.exe" %1 %2 
goto end
:diffAll
@For /f "tokens=1 delims=." %%a in ('dir /B regTest\*.last') do @call %0 regTest\%%a.expected regTest\%%a.last
:end