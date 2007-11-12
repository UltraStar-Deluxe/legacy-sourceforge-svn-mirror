@echo off

SET FILE=%U3_DEVICE_PATH%\System\Apps\0B458BEB-A9BB-4E93-B48F-F1EBA58D05BD\Data\config.ini

for /F "Tokens=*" %%i in (%FILE%) do Set Var=%%i

type %FILE% | find "[Path]"
if errorlevel 1 goto write
if errorlevel 0 goto start

:write

ECHO. >> %FILE%
ECHO [Path] >> %FILE%
ECHO Songs=%U3_DEVICE_PATH%\System\Apps\0B458BEB-A9BB-4E93-B48F-F1EBA58D05BD\Data\Songs >> %FILE%

goto start

:start

start U3.lnk
