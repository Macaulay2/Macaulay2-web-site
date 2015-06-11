(1) Unpack the numerical_packages/ directory

tar xfz numerical_packages.tar.gz

(2) Put numerical_packages/ on M2 "path": add the line 

path = {"where_you_unpacked/numerical_packages/"} | path

either to the scripts you run or to init.m2

(3) Type the following to use a package and read its documentation

needsPackage "PACKAGE"
help "PACKAGE"

where PACKAGE is Bertini, PHCpack, NAGtypes, NumericalAlgebraicGeometry.

(4) External software download instructions: Bertini and PHCpack

http://www3.nd.edu/~sommese/bertini/download.html

http://homepages.math.uic.edu/~jan/download.html

