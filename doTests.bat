@mnh.exe regTest\test.mnh mnh.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
@if errorlevel 1 goto end 
@mnh.exe regTest\test.mnh bin32\mnh.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
@if not errorlevel 0 goto end 
@mnh.exe regTest\test.mnh mnh_light.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
@if not errorlevel 0 goto end 
@mnh.exe regTest\test.mnh bin32\mnh_light.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
@if not errorlevel 0 goto end 
@if "%1"=="" call make distro
:end
