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
  cond_set = c()
  if (length(setC) > 0) {
    for (i in 1:length(setC))
      cond_set = c(cond_set, groups[[setC[i]]])
  }
  groupA <- groups[[setA]]
  groupB <- groups[[setB]]
  lengthA <- length(groupA)
  lengthB <- length(groupB)
  limA <- round(ratio * lengthA)
  limB <- round(ratio * lengthB)
  
  if (limA < 1) {
    limA = 1
  }
  if (limB < 1) {
    limB = 1
  }
  
  result <- foreach(i = 1:lengthA, .combine='cbind') %:%
    foreach(j = 1:lengthB, .combine='c', .packages='pcalg') %dopar% {
      ifelse(dsep(groupA[i], groupB[j], cond_set, dag), 0, 1)
    }
  
  # d-connected if
  # If A(active) >= A(total * ratio) &&
  #    B(active) >= B(active * ratio)
  if (sum(colSums(result) > 0) < limA ||
      sum(rowSums(result) > 0) < limB) {
    return(T)
  }
  
  # flag1 <- array(FALSE, dim = lengthA)
  # flag2 <- array(FALSE, dim = lengthB)
  
  # for (i in 1:lengthA) {
  #   for (j in 1:lengthB) {
  #     if (!dsep(groupA[i], groupB[j], cond_set, dag)) {
  #       return(F)
  #     }
  #   }
    # flag2 <- foreach (j = 1:lengthB,
    #                   .combine = 'c',
    #                   .packages = "pcalg") %dopar% {
    #                     return(!dsep(groupA[i], groupB[j], cond_set, dag))
    #                   }
    # 
    # if (sum(flag2 == T) >= round(limiteB)) {
    #   flag1[i] <- T 
    # }
    # 
    # if (sum(flag1 == T) >= round(limiteA)) {
    #   return(F)
    # }
  # }
  return(F)
}