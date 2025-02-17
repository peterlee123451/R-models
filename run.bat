@echo off
REM Get the directory where this batch file is located
set "current_folder=%~dp0"

REM Run Processor.R using Rscript.exe from R 4.3.0
"C:\Program Files\R\R-4.3.0\bin\Rscript.exe" "%current_folder%Processor.R"

pause