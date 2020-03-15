# This is totally copied from Sacha Epskamp's bootnet package.
# I mean, he borrowed it from lavaan, so I dont feel all that bad


.onAttach <- function(libname, pkgname) {
  version <- read.dcf(file=system.file("DESCRIPTION", package=pkgname),
                      fields="Version")
  packageStartupMessage("This is ",paste(pkgname, version))
  # packageStartupMessage(pkgname, " is BETA software! Please report any bugs.")
  packageStartupMessage("The (unofficial) version name is: Kitten Mittens")
  packageStartupMessage("For questions and issues, please see github.com/sdparsons/splithalf")
  packageStartupMessage("There is full documentation at https://sdparsons.github.io/splithalf_documentation/")
  packageStartupMessage("or email sam.parsons@psy.ox.ac.uk")
}
