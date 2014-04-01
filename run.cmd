@ECHO OFF
python run.py %1
echo ---------------------
echo Running the output...
echo ---------------------
IF "%2" == "-r" test.exe