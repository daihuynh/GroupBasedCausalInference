if(!exists("require.packages", mode="function")) source("require_packages.R")

require.packages(c("jdx", "rJava"))

#'@title grouping_using_module_network
#'@description Grouping using Module Network algorithm via the Java implementation, which is in a jar file.
#'@param jarPath The path to Module Network jar file.
#'@param rawData Data matrix
#'@param numModules The number of desired modules.
#'@param numIterations The number of iteration. (Default is 30)
#'@return TRUE if group A and group B conditional independent given group C, FALSE for otherwise
grouping_using_module_network <- function(jarPath, rawData, numModules, numIterations = 30) {
  if (!is.matrix(rawData) & !is.data.frame(rawData))
    
    stop("'rawData' must be a matrix or data frame")
  if (!is.numeric(numModules) | !is.numeric(numModules))
    stop("'numModules' and 'numIterations' must be numeric value")
  if (numModules <= 0 | numModules > dim(rawData)[2])
    stop("'numModules' should be positive and no large than the variables number")
  rawDataPath <- "rawData.tab"
  #convert the raw data to the format of ModuleNetwork required data format
  rawDataDim = dim(rawData)
  rawData <- t(rawData)
  rawData <- round(rawData, 2)
  rawData <- data.frame(rawData)
  rawData[, 4:(rawDataDim[1] + 3)] <- rawData[, 1:rawDataDim[1]]
  for (i in 1:rawDataDim[2]) {
    rawData[i, 1] <- paste0("G", i)
  }
  rawData[, 2] <- rownames(rawData)
  rawData[, 3] <- ""
  colnames(rawData)[1] <- "GENE"
  colnames(rawData)[2] <- "NAME"
  colnames(rawData)[3] <- "DESCRIPTION"
  for (i in 4:(rawDataDim[1] + 3)) {
    colnames(rawData)[i] <- paste0("E", i - 3)
  }
  #save the data to tab file
  write.table(
    rawData,
    rawDataPath,
    row.names = FALSE,
    sep = "\t",
    quote = FALSE
  )
  #use rJava package
  .jinit(classpath = jarPath)
  jobject <- J("GeneXPressPackage/ModuleNetworks/MNMain")
  JrawDataPath <- new(J("java.lang.String"), rawDataPath)
  JnumModules <-
    new(J("java.lang.Integer"), as.character(numModules))
  JnumIterations <-
    new(J("java.lang.Integer"), as.character(numIterations))
  result <-
    jobject$learnModules(JrawDataPath, JnumModules, JnumIterations)
  file.remove("rawData.tab")
  result <- convertToR(result)
  resultModules <- list()
  for (i in 1:as.numeric(length(result))) {
    geneNames <- c()
    for (j in 1:length(result[[i]])) {
      geneName <- gsub("\\.", "-", rawData[result[[i]][j] + 1, 2])
      
      geneNames <- c(geneNames, geneName)
    }
    resultModules[[i]] <- geneNames
  }
  return(resultModules)
}

#'@title grouping_using_module_network
#'@description Grouping using Module Network algorithm via the Java implementation, which is in a jar file.
#'@param jarPath The path to Module Network jar file.
#'@param rawData Data matrix
#'@param numModules The number of desired modules.
#'@param numIterations The number of iteration. (Default is 30)
#'@return TRUE if group A and group B conditional independent given group C, FALSE for otherwise
convert_to_col_numbers <- function(modules, rawData) {
  colNumberModules <- list()
  for (i in 1:length(modules)) {
    geneColNumbers <- c()
    for (j in 1:length(modules[[i]])) {
      geneColNumber <-
        as.character(which(colnames(rawData) == modules[[i]][j]))
      geneColNumbers <- c(geneColNumbers, geneColNumber)
    }
    colNumberModules[[i]] = geneColNumbers
  }
  return(colNumberModules)
}