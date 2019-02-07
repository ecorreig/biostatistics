# En aquest fitxer hi poso gràfics interessants en general, dels que es fan servir sempre

require(ggplot2)
require(plotly)
require(ggcorrplot)


# Gràfic de correlacions:

grafic_correlacions = function(df, ordenar = F, tipus="square"){
  
  # Funció: dibuixa dos heatmaps de correlacions
  # Arguments: dataframe, si volem clustering i el tipus, square o circle
  # Retorn: dos gràfics, un amb els valors de la correlació en les cel·les i un altre amb les correlacions
  #         que no són significatives tatxades
  
  corr <- round(cor(df), 2)
  
  plot1 = ggcorrplot(corr, 
                    hc.order = ordenar,
                    method = tipus, 
                    type = "lower",
                    outline.col = "white",
                    theme_bw(), 
                    colors = c("#6D9EC1", "white", "#E46726"), 
                    lab = TRUE)
  
  p.mat <- cor_pmat(df)
  
  plot2 = ggcorrplot(corr, 
                     hc.order = ordenar, 
                     method = tipus, 
                     type = "lower",
                     outline.col = "white",
                     theme_bw(), 
                     colors = c("#6D9EC1", "white", "#E46726"), 
                     p.mat = p.mat)
  
  return(list(plot1, plot2))
}


# Loop de gràfics sobre comparacions univariades:

loop_univariat = function(df, grup, log = F, tots = F, box = T, violin = T, dens = T){
  # Funció: dibuixa gràfics per comparar totes les variables segons el grup
  # 
  quin = which(names(df)==grup)
  grup = df[,quin]
  df = df[,-quin]
  
  if (log){
    df = log(df)
  }
  
  df = cbind.data.frame(df, grup)
  
  pvals = c()
  
  if (length(levels(grup))>2){
  
    for (i in 1:(ncol(df)-1)){
      test = kruskal.test(df[, i] ~ grup)
      pvals = c(pvals, test$p.value)
    }
  }
  
  else{
    
    for (i in 1:(ncol(df)-1)){
      test = kruskal.test(df[, i] ~ grup)
      pvals = c(pvals, test$p.value)
    }
    
  }
  
  
  p_adj = p.adjust(pvals, "BH")
  
  quines = which(p_adj<.05)
  
  if (tots){
    quines = 1:(ncol(df)-1)
  }
  
  
  for (i in quines){
    if (box){
    p1 = ggplot(df, aes(x = grup, y = df[, i], colour = grup)) + 
      geom_boxplot() + 
      labs(title = colnames(df)[i],
           subtitle = paste0("p valor ajustat = ", round(p_adj[i]), 4)) + 
      xlab(colnames(df)[i]) + 
      theme_bw()
    print(p1)
    }
    
    if(violin){
    p2 = ggplot(df, aes(x = grup, y = df[, i], colour = grup)) + 
      geom_boxplot() + 
      labs(title = colnames(df)[i],
           subtitle = paste0("p valor ajustat = ", round(p_adj[i]), 4)) + 
      xlab(colnames(df)[i]) + 
      theme_bw()
    print(p2)
    }
    
    if(dens){
    p3 = ggplot(df, aes(x = df[, i], fill = grup)) +
      geom_density(alpha = .5)+ 
      labs(title = colnames(df)[i],
           subtitle = paste0("p valor ajustat = ", round(p_adj[i]), 4)) + 
      xlab(colnames(df)[i]) + 
      theme_bw()
    print(p3)
    }
  }
}


grafic_3D = function(df, x, y, z, grup){
  
  # Funció: aquest gràfic és aquí bàsicament per tenier plot_ly controlat, és més útil fer servir 
  #         la funció directament.
  
  plot_ly(df, 
          x = ~ x, 
          y = ~ y, 
          z = ~ z, 
          color = ~ grup, 
          colors = c('#BF382A', '#0C4B8E'),
          size = .5)
}


grafic_PCA_3D = function(df, grup){
  
  # Funció: fa un PCA i el dibuixa en tres dimensions i amb colors diferents pels grups
  # Arguments: df: dataframe, només hi pot haver variables contínues
  #            grup: variable categòrica amb els grups
  # Retorn: res
  
  df = Filter(is.numeric, df)
  pca = prcomp(df)
  
  plot_ly(as.data.frame(pca$x), 
          x = ~PC1, 
          y = ~PC2, 
          z = ~PC3, 
          color = ~grup,
          size = .5)
}