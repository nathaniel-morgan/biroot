get_classify <- function(data, mesh, method, args=NULL) {
  formula <- formula(fill~x+y)
  if ( typeof(method)=="character" ) {
    
    if ( method == "randomForest" ) {
      disc <- do.call(method, args = list(formula, data=data))
      mesh$fill = predict(disc, mesh)
    } else if ( method == "knn" ) {
      if( class(data$fill) != "factor" ) cli_alert_danger("response must be a factor for KNN")
      disc <- kknn::train.kknn(formula, data=data)
      mesh$fill = predict(disc, mesh)
    } else if ( method %in% c("lda", "qda") ) {
      disc <- do.call(method, args = list(formula, data=data))
      pred <- predict(disc, mesh)
      mesh$fill = pred$class
      mesh$probability = apply(pred$posterior, MARGIN = 1, max)
    } else {
      cli_alert("Method shortcuts currents recognized are lda, qda, knn, or randomForest.")
    }
    return(mesh)
  } else {
    if ( inherits(method, "model_spec") ) {
      mesh$fill <- predict(fit(method, formula, data=data), mesh)$.pred_class
      mesh
    } else if ( inherits(method, "unsupervised_spec") ) {
      mesh$fill <- as.double(predict(fit(method, ~x+y, data=data), mesh)$.pred_cluster)
      mesh
    } else {
      if ( is.null(args) ) {
        args <- list(data=data, mesh=mesh)
        do.call(method, args)
      } else {
        args <- c(args,list(data=data, mesh=mesh))
        do.call(method, args)
      }
    }
  }
}

get_classify_train <- function(data, method, args=NULL, grid = NULL) {
  formula <- formula(fill~x+y)
  if( !is.null(grid) ){
    formula <- as.formula(paste0("fill~","x+y","+",grid$rows))
    if( !is.null(grid$cols) ){
      formula <- as.formula(paste0("fill~","x+y","+",grid$rows,"+",grid$cols)) 
    }
    formula
  }
  if ( typeof(method)=="character" ) {
    
    if ( method == "randomForest" ) {
      mod <- do.call(method, args = list(formula, data=data))
    } else if ( method == "knn" ) {
      if( class(data$fill) != "factor" ) cli_alert_danger("response must be a factor for KNN")
      mod <- kknn::train.kknn(formula, data=data)
    } else if ( method %in% c("lda", "qda") ) {
      mod <- do.call(method, args = list(formula, data=data))
    } else {
      cli_alert("Method shortcuts currents recognized are lda, qda, knn, or randomForest.")
    }
    return(mod)
  } else {
    if ( inherits(method, "model_spec") ) {
      mod <- fit(method, formula, data=data)
    } else if ( inherits(method, "unsupervised_spec") ) {
      mod <- fit(method, ~x+y, data=data)
    } else {
      mod <- method
    }
    return(mod)
  }
}

get_classify_predict <- function(data, mesh, mod, method, args=NULL) {
  formula <- formula(fill~x+y)
  if ( typeof(method)=="character" ) {
    
    if ( method == "randomForest" ) {
      mesh$fill = predict(mod, mesh)
    } else if ( method == "knn" ) {
      mesh$fill = predict(mod, mesh)
    } else if ( method %in% c("lda", "qda") ) {
      pred <- predict(mod, mesh)
      mesh$fill = pred$class
      mesh$probability = apply(pred$posterior, MARGIN = 1, max)
    } else {
      print("placeholder")
    }
    return(mesh)
  } else {
    if ( inherits(method, "model_spec") ) {
      mesh$fill <- predict(mod, mesh)$.pred_class
      mesh
    } else if ( inherits(method, "unsupervised_spec") ) {
      mesh$fill <- as.double(predict(mod, mesh)$.pred_cluster)
      mesh
    } else {
      if ( is.null(args) ) {
        args <- list(data=data, mesh=mesh)
        do.call(method, args)
      } else {
        args <- c(args,list(data=data, mesh=mesh))
        do.call(method, args)
      }
    }
  }
}
