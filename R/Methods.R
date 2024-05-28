kmeans_method <- function(data,mesh,method="lda",n) {
  data$class <- factor(LETTERS[kmeans(data[,1:2],n)$cluster])
  do.call(get_classify,args = list(data = data, mesh = mesh, 
                                       method = method))
}

# Rewrite to use euclidian distance to classify

#something with triangles