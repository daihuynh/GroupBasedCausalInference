#'@title conditional_independence_group_test
#'@description Learn a skeleton DAG by using PC algorithm along with our proposed conditional independence test, delta-d-separation
#'@param dag The variable DAG learnt by using PC algorithm.
#'@param groups The country where the data was recorded 
#'@param setA Set of vertices in group A.
#'@param setB Set of vertices in group B.
#'@param setC Set of vertices in conditioning group C.
#'@param ratio ratio for delta-d-separation
#'@return TRUE if group A and group B conditional independent given group C, FALSE for otherwise
conditional_independence_group_test <- function(dag, groups, setA, setB, setC, ratio) {
  if (length(setC) == 0)
    cond_set = c()
  else{
    cond_set = c()
    for (i in 1:length(setC))
      cond_set = c(cond_set, groups[[setC[i]]])
  }
  groupA <- groups[[setA]]
  groupB <- groups[[setB]]
  print(groupA)
  print(groupB)
  print(setC)
  print(cond_set)
  lengthA <- length(groupA)
  lengthB <- length(groupB)
  limiteA <- ratio * lengthA
  limiteB <- ratio * lengthB
  if (limiteA < 1) {
    limiteA = 1
  }
  if (limiteB < 1) {
    limiteB = 1
  }
  flag1 <- array(FALSE, dim = lengthA)
  flag2 <- array(FALSE, dim = lengthB)
  for (i in 1:lengthA) {
    for (j in 1:lengthB) {
      if (!dsep(groupA[i], groupB[j], cond_set, dag)) {
        flag1[i] = TRUE
        flag2[j] = TRUE
      }
    }
    if (sum(flag1 == TRUE) >= round(limiteA) &&
        sum(flag2 == TRUE) >= round(limiteB)) {
      return(FALSE)
    }
  }
  return(TRUE)
}