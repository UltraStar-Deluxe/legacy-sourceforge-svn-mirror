del UltraStar.res
"C:\Program Files\Borland\BDS\4.0\Bin\BRC32 -r UltraStar.RC

"C:\Program Files\Borland\BDS\4.0\Bin\dcc32.exe" -U"lib\JEDI-SDLv1.0\SDL\Pas" -O"lib\JEDI-SDLv1.0\SDL\Pas" -I"lib\JEDI-SDLv1.0\SDL\Pas" -R"lib\JEDI-SDLv1.0\SDL\Pas" UltraStar.dpr
copy UltraStar.exe ..\..\
cd ..\..\


cd ScoreConverter
"C:\Program Files\Borland\BDS\4.0\Bin\dcc32.exe" -U"..\Game\Code\Lib\SQLite" -O"..\Game\Code\Lib\SQLite" -I"..\Game\Code\Lib\SQLite" -R"..\Game\Code\Lib\SQLite" ScoreConverter.dpr
copy ScoreConverter.exe ..\

cd ..\

upx --force -9 *.exe

cd Installer
"C:\Program Files\NSIS\makeNSIS.exe" UltraStarDeluxe.nsi

cd ..\Game\Code