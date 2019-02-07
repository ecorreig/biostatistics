# En aquest fitxer afegeixo models de supervivència, tant cox com competing risks

cox_model = function(df, mort_si_no, temps_mort, var_independent, weights=FALSE){
  
  # Funció: calcula un model de Cox
  # Arguments: df: dataframe amb només les variables que ens interessen
  #            mort_si_no: nom de la variable categòrica de mort
  #            temps_mort: nom de la variable que compta el temps fins a la mort
  #            var_independent: nom de la variable independent de la que estiguem interessats
  # Resposta: model de Cox.
  
  resposta = Surv(as.numeric(temps_mort), mort_si_no)
  
  if (!weights){
    model = coxph(resposta ~ . - var_independent + strata(var_independent), data = df)
  }
  
  else {
    model = coxph(resposta ~ . - var_independent + strata(var_independent), data = df, weights = weights)
  }
  
  return(model)
}

cr_model = function(df, esdeveniment, temps_esdeveniment, weights=FALSE){
  
  # Funció: competing risks analysis
  # Argument: df: dataframe
  #           esdeveniment: mort, alta o censura, els nivels han d'estar ordenats com "alta", "censura" i "mort"
  #           temps_esdeveniment: el temps de cadascun dels de dalt
  #           weights: en cas que necessitem pesos
  # Retorn: dos models de cox, un per mort i l'altra per alta.
  
  resposta = Surv(temps_esdeveniment, esdeveniment)
  fg = finegray(resposta ~ ., data = df, etype = levels(esdeveniment)[1])
  
  if (!weights){
    cox_fg = coxph(Surv(fgstart, fgstop, fgstatus) ~ ., data = fg)
  }
  
  else {
  cox_fg = coxph(Surv(fgstart, fgstop, fgstatus) ~ ., data = fg, weights = weights)
  }
  
  df$crvar = relevel(as.factor(df$crvar), "mort")
  fg = finegray(Surv(estancia, crvar) ~ ., data = df, etype = "alta")
  
  if (!weights){
    cox_fg2 = coxph(Surv(fgstart, fgstop, fgstatus) ~ ., data = fg)
  }
  
  else {
    cox_fg2 = coxph(Surv(fgstart, fgstop, fgstatus) ~ ., data = fg, weights = weights)
  }
  
  coxs = list(cox_fg, cox_fg2)
  names(coxs) = c("mort", "alta")
  
  return (coxs)
}