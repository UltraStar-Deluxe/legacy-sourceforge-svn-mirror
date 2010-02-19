@echo off

cd ..\..\

SET PATH=.\Installer\u3portable\Host

xcopy .\Covers\*.* %PATH% /Y /T /E
xcopy .\Languages\*.* %PATH% /Y /T /E
xcopy .\Plugins\*.* %PATH% /Y /T /E
xcopy .\Skins\*.* %PATH% /Y /T /E
xcopy .\Sounds\*.* %PATH% /Y /T /E
xcopy .\Themes\*.* %PATH% /Y /T /E
xcopy .\Covers\*.* %PATH% /Y /T /E
copy  .\UltraStar.exe %PATH% /Y /T /E

xcopy .\InstallerDependencies\dll\*.* %PATH% /Y /T /E

cd .\Installer\u3portable\Host

ren UltraStar.exe USdx.exe