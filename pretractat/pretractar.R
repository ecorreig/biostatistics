# En aquest fitxer afegeixo funcions per pretractar les dades.

require(missForest)

format_data = function(df){
  
  # Funció: 1. Neteja els noms de caràcters estranys (i que poden donar problemes més tard)
  #         2. Converteix les variables en el format que creu més adequat, eliminant les que tenen format de caràcter
  #         3. Imputa missings utilitzant random forest
  # Argument: dataframe
  # Retorn: dataframe
  
  names(df) = make.names(df)
  fora = c()
  for (i in 1:ncol(df)){
    if(length(unique(df[,i]))<6){
      df[,i] = as.factor(df[,i])
    }
    
    else if (is.character(df[,i])){
      fora = c(fora, i)
    }
    
    else if (is.factor(df[,i])){
      next
    }
    
    else {
      df[,i] = as.vector(scale(df[,i]))
    }
  }
  df = df[,-fora]
  
  df = missForest(df)
  return(df)
}


propensity_matching = function(df, var_imp, ratio, tipus="full"){
  
  # Funció: aquesta funció és bàsicament per tenir aquí les funcions de matchit, ja que no fa massa cosa a més
  #         de la funció en si
  # Argument: df: dataframe
  #           var_imp: variable important, sobre la qual volem matchejar
  #           ratio: ratio
  #           tipus: tipus de matchejat, les opcions són: 
  #                 "exact" (exact matching), "full" (full matching) [per defecte], 
  #                 "genetic" (genetic matching), "nearest" (nearest neighbor matching), 
  #                 "optimal" (optimal matching), and "subclass" (subclassification).
  # retorn: dataframe matchejat: alerta perquè apareixen dues columnes, de pes i de distància
  
  m.full = matchit(var_imp ~ ., data = df, method = tipus, ratio = ratio)
  m.data = match.data(m.full)
  
  return(m.data)
}
