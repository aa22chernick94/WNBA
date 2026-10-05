@echo off
REM Edit the line below if Rscript isn't on your PATH, e.g.:
REM "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "%~dp0build_dashboards.R"
cd /d "%~dp0"
Rscript build_dashboards.R
if %ERRORLEVEL% NEQ 0 (
  echo.
  echo Build failed -- see the message above.
  pause
  exit /b 1
)

REM Define Google Drive destination path
set "DEST_DIR=G:\My Drive\WNBA"

REM Create the destination directory if it does not exist
if not exist "%DEST_DIR%" mkdir "%DEST_DIR%"

REM Copy the HTML dashboard file to Google Drive
copy /Y "%~dp0wnba_dashboards.html" "%DEST_DIR%\wnba_dashboards.html"

REM Open the copied file from Google Drive
start "" "%DEST_DIR%\wnba_dashboards.html"