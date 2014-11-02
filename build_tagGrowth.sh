echo
echo == Building helpfiles ==
R CMD BATCH update-description.R
R CMD BATCH run-roxygen.R


echo
echo == Building source ==
R CMD build tagGrowth


echo == Installing ==
R CMD INSTALL tagGrowth_*.tar.gz


echo
echo == Building for windows ==
mkdir localRlib
rm -r tagGrowth.zip
R CMD INSTALL -l localRlib tagGrowth_*.tar.gz
cd localRlib
zip -r tagGrowth tagGrowth
mv tagGrowth.zip ..
cd ..
rm -r localRlib/

cd tagGrowth
chmod 777 DESCRIPTION
