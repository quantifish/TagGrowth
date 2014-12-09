echo
echo == Building helpfiles ==
R CMD BATCH update-description.R
R CMD BATCH run-roxygen.R


echo
echo == Building source ==
cd ../../
R CMD build TagGrowth


echo == Installing ==
R CMD INSTALL TagGrowth_*.tar.gz


echo
echo == Building for windows ==
mkdir localRlib
rm -r TagGrowth.zip
R CMD INSTALL -l localRlib TagGrowth_*.tar.gz
cd localRlib
zip -r TagGrowth TagGrowth
mv TagGrowth.zip ..
cd ..
rm -r localRlib/

mv TagGrowth_*.tar.gz TagGrowth/
mv TagGrowth.zip TagGrowth/

cd TagGrowth
chmod 777 DESCRIPTION
