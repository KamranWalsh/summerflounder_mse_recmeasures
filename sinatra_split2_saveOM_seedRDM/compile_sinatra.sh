echo 'Common' &>gav.junk
gfortran -c -fno-align-commons -ffixed-line-length-0 Common.FOR &>>gav.junk
#echo 'C' &>>gav.junk
#gfortran -c -fno-align-commons -ffree-line-length-0 C.F90 &>>gav.junk
#echo 'Dminim' &>>gav.junk
#gfortran -c -fno-align-commons -ffree-line-length-0 Dminim.For &>>gav.junk
#echo 'Matrix' &>>gav.junk
#gfortran -c -fno-align-commons -ffree-line-length-0 Matrix.for &>>gav.junk
#echo 'GenData' &>>gav.junk
#gfortran -c -fno-align-commons -ffree-line-length-0 GenData.f90 &>>gav.junk
echo 'Compile Sinatra' &>>gav.junk
#gfortran -fno-align-commons -mcmodel=medium -ffree-line-length-0 sinatra.f90 -o sinatra.exe  &>>gav.junk
gfortran -fno-align-commons -fbacktrace -fbounds-check -mcmodel=medium -ffree-line-length-0 sinatra.f90 -o sinatra.exe  &>>gav.junk
echo 'Happy?' &>>gav.junk
