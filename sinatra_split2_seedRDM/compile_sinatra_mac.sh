echo 'Common' &> gav.junk
#gfortran-4.8 -g -fcheck=all -Wall -c -fno-align-commons -ffixed-line-length-0 Common.FOR &> gav.junk
##gfortran-4.8 -c -fno-align-commons -ffixed-line-length-0 Common.FOR &> gav.junk
gfortran -c -fno-align-commons -ffixed-line-length-0 Common.FOR &> gav.junk
#gfortran -c -fno-align-commons -ffixed-line-length-0 Common.FOR &> Common.junk
#echo 'C' &>>gav.junk
#gfortran -c -fno-align-commons -ffree-line-length-0 C.F90 &>>gav.junk
#echo 'Dminim' &>>gav.junk
#gfortran -c -fno-align-commons -ffree-line-length-0 Dminim.For &>>gav.junk
#echo 'Matrix' &>>gav.junk
#gfortran -c -fno-align-commons -ffree-line-length-0 Matrix.for &>>gav.junk
#echo 'GenData' &>>gav.junk
#gfortran -c -fno-align-commons -ffree-line-length-0 GenData.f90 &>>gav.junk
echo 'Compile Sinatra' &> gav.junk
#gfortran -fno-align-commons  -ffree-line-length-0 sinatra.f90 -o sinatra.exe  &> Sinatra.junk
#gfortran-4.8 -fno-align-commons -fbounds-check -mcmodel=medium -ffree-line-length-0 sinatra.f90 -o sinatra.exe  &> gav.junk
#gfortran-4.8 -fno-align-commons -mcmodel=medium -ffree-line-length-0 sinatra.f90 -o sinatra.exe  &> gav.junk
gfortran -fno-align-commons -ffree-line-length-0 sinatra.f90 -o sinatra.exe  &> gav.junk
#gfortran-4.8 -g -fbacktrace -Wall -Wextra -fno-automatic -finit-local-zero -mcmodel=medium -fcheck=all -Wall -fno-align-commons -fbacktrace -fbounds-check -mcmodel=medium -ffree-line-length-0 sinatra.f90 -o sinatra.exe  &> gav.junk

#echo 'Happy?' &>> gav.junk
## 2018-03-05 
## gfortran -fno-align-commons  -ffree-line-length-0 sinatra.f90 -shared -o sinatra.dll  &> Sinatra.junk

#-mcmodel=large 