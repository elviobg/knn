distance <- function(vA, vB){
  d = vA - vB
  e = d^2
  s =sum(e)
  sqrt(s)
}

distances <- function(data, instance){
  distances <- c()
  for(i in 1:nrow(data)){
    distances <- c(distances, distance(instance, data[i,]))
  }
  return(distances)
}

mode <- function(v){
  sort(table(v), decreasing=T)[1]
}

knn <- function(data, labels, instance, k){
  distances <- distances(data, instance)
  labels[sort.list(distances)[1:k]]
}

knn_classifier <- function(data, labels, instance, k){
  k_nearests <- knn(data, labels, instance, k)
  mode(k_nearests)
}

knn_regression <- function(data, labels, instance, k){
  k_nearests <- knn(data, labels, instance, k)
  mean(k_nearests)
}

wknn <- function(data, labels, instance, k){
  distances <- distances(data, instance)
  #TODO: verify if has one or more "0" in distances
  weights = 1/distances
  k_nearests <- sort(weights, decreasing=TRUE, index.return=T)$ix[1:k]
  k_nearest_weights = weights[k_nearests]
  k_nearest_label=labels[k_nearests]
  
  data_weights <- data.frame(labels=k_nearest_label, weights=k_nearest_weights)
  sums = aggregate(.~labels, data_weights, sum)
  sums$labels[which.max(sums$weights)]
}