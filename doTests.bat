@mnh.exe regTest\test.mnh mnh.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
@if errorlevel 2 goto end 
@bin32\mnh.exe regTest\test.mnh bin32\mnh.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
@if errorlevel 2 goto end 
@mnh_light.exe regTest\test.mnh mnh_light.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
@if errorlevel 2 goto end 
@bin32\mnh_light.exe regTest\test.mnh bin32\mnh_light.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
@if errorlevel 1 goto end 
@if "%1"=="" call make distro
:end
