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

#'@title require.bio_packages
#'@description Install missing BioConductor packages and load all required packages.
#'@param packageNames package names
require.bio_packages <- function(packageNames) {
  installedPackages <- installed.packages()[, 'Package']
  
  needInstallPackages <-
    packageNames[!(packageNames %in% installedPackages)]
  if (length(needInstallPackages) > 0) {
    BiocManager::install(needInstallPackages)
  }
  
  for (packageName in packageNames) {
    library(packageName, character.only = TRUE)
  }
}