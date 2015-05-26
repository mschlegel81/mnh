@mnh.exe regTest\regTest.mnh mnh.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
@if errorlevel 2 goto end 
@bin32\mnh.exe regTest\regTest.mnh bin32\mnh.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
@if errorlevel 2 goto end 
@mnh_light.exe regTest\regTest.mnh mnh_light.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
@if errorlevel 2 goto end 
@bin32\mnh_light.exe regTest\regTest.mnh bin32\mnh_light.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
@if errorlevel 1 goto end 
@rem if "%1"=="" call make distro
:end
