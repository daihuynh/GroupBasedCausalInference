#'@title require.packages
#'@description Install missing packages and load all required packages.
#'@param packageNames package names
require.packages <- function(packageNames) {
  installedPackages <- installed.packages()[, 'Package']
  
  needInstallPackages <-
    packageNames[!(packageNames %in% installedPackages)]
  if (length(needInstallPackages) > 0) {
    install.packages(needInstallPackages)
  }
  
  for (packageName in packageNames) {
    library(packageName, character.only = TRUE)
  }
}
