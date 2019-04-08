distance <- function(vA, vB){
  d = vA - vB
  e = d^2
  s =sum(e)
  sqrt(s)
}

knn <- function(data, labels, instance, k){
  distances <- c()
  for(i in 1:nrow(data)){
    distances <- c(distances, distance(instance, data[i,]))
  }
  k_nearests <- labels[sort.list(distances)[1:k]]
}