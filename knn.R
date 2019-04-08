distance <- function(vA, vB){
  d = vA - vB
  e = d^2
  s =sum(e)
  sqrt(s)
}

mode <- function(v){
  sort(table(v), decreasing=T)[1]
}

knn <- function(data, labels, instance, k){
  distances <- c()
  for(i in 1:nrow(data)){
    distances <- c(distances, distance(instance, data[i,]))
  }
  k_nearests <- labels[sort.list(distances)[1:k]]
}

knn_classifier <- function(data, labels, instance, k){
  k_nearests <- knn(data, labels, instance, k)
  mode(k_nearests)
}

knn_regression <- function(data, labels, instance, k){
  k_nearests <- knn(data, labels, instance, k)
  mean(k_nearests)
}
