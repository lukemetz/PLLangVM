@ECHO OFF
python run.py %1
if "%2" == "-r" (
echo ---------------------
echo Running the output...
echo ---------------------
test.exe)