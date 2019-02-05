# En aquest fitxer implemento les funcions de lasso, ridge regression i elastic net en general

require(glmnet)

logistic_EN = function(df, alpha=1, train, y_index, weights=FALSE, plots=TRUE, lambda="1se"){
  
  # Funció: Calcula lasso, ridge regression i elastic net segons el valor d'alpha que li passem. 
  
  if (sum(is.na(grip)>0)){
    stop("Hi ha missings al dataframe, si us plau treu-los")
  }
  
  test=(-train)

  x<- df[,-c(y_index)]
  y<- df[,y_index]
  data<-cbind(x,y)
  x_train <- x[train,]
  y_train <- data$y[train]
  x_test <- x[test,]
  y_test <- data$y[test]
  
  if (!weights){
    model <- model.matrix(y~., data=data[train,])
    fit <- cv.glmnet(model,y_train, alpha=alpha, family="binomial")
    }
  else {
    model <- model.matrix(y~. - weights, data=data[train,])
    fit <- cv.glmnet(model,y_train,alpha=alpha, family="binomial", weights = weights)
    }
  
  if(plots){plot(fit)}
  
  if (lambda=="1se"){
    lam = "lambda.1se"
    lambda_min = fit$lambda.1se
  }
  else {
    lam= "lambda.min"
    lambda_min = fit$lambda.min
  }

  cc = predict(fit, type='coefficients', s = lam)
  coefs = cc[1:dim(cc)[1],]
  ffc = coefs[coefs!=0]
  
  if (!weights){
    newX <- model.matrix(~.,data=x_test)
  }
  else {
    newX <- model.matrix(~.-weights,data=x_test, weights = x_test$weights)
  }
  
  fit_test<-predict(fit,s=lambda_min, newx=newX, type='response')
  fit.pred=rep(0,nrow(x_test))
  fit.pred[fit_test >.5]=1
  ttt=table(fit.pred, y_test)
  rend = (ttt[1]+ttt[4])/sum(ttt)
  
  returns = list(ffc, ttt, rend)
  names(returns) = c("Coeficients", "Matriu de confusió", "Encert")

  return(returns)
}
