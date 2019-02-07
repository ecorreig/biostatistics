

require(kableExtra)

regressio_lineal = function(df, var_ind){
  quin = which(names(df)==var_ind)
  ind = df[,quin]
  df[,quin]=NULL
  
  fit = lm(ind~., df)
  ci = confint(fit)
  sum = summary(fit)
  resum = as.data.frame(cbind(ci[,1], sum$coefficients[,1],
                              ci[,2],sum$coefficients[,4]))
  resum = round(resum, 3)
  colnames(resum) = c("IC-", "Coeficient", "IC+", "p-valor")
  kk = resum %>% kable("html") %>% kable_styling()
  print(kk)
}


odd_ratio = function(df, var_ind){
  quin = which(names(df)==var_ind)
  ind = df[,quin]
  df[,quin]=NULL
  
  continues = names(Filter(is.numeric, df))
  fit = lm(ind~., df)
  ci = confint(fit)
  sum = summary(fit)
  resum = as.data.frame(cbind(exp(ci[,1]), exp(sum$coefficients[,1]),
                              exp(ci[,2]),sum$coefficients[,4]))
  
  
  resum = resum[!rownames(resum) %in% continues, ]
  resum = resum[-1,]
  resum = round(resum, 3)
  
  colnames(resum) = c("IC-", "Odd Ratio", "IC+", "p-valor")
  kk = resum %>% kable("html") %>% kable_styling()
  print(kk)
}

odd_ratio(df, "Edad")