# En aquest fitxer afegeixo gràfics per mostrar el propensity matching

require(MatchIt)
require(cobalt)
require(ggplot2)


distribucio_PM = function(df){
  
  # Funció: dibuixa dos gràfics de distribucions abans i després del propensity matching
  # Arguments: df: dataframe després de fer el PM, on una de les columnes ha de ser la distància (distance)
  # Retorn: dos gràfics
  
  p1 = ggplot(df, aes(distance)) + 
    geom_histogram(fill = "blue", alpha = 0.7, bins = 30) + 
    geom_vline(xintercept = 0.5, colour = "red") + 
    ylab("Number of patients") + 
    xlab("Propensity score") +
    ggtitle("Propensity score histogram") + 
    theme_bw()
  
  p2 = ggplot(df, aes(distance)) + 
    geom_density(fill = "blue", alpha = 0.3) + 
    geom_vline(xintercept = 0.5, colour = "red") + 
    ylab("Number of patients") + 
    xlab("Propensity score") +
    ggtitle("Propensity score density plot") +
    theme_bw()
  
  return(list(p1, p2))
}



plot.summary.matchit <- function(x, interactive = TRUE, ...) {
  
  # Funció: helper per la funció següent
  
  if ("matchit.exact" %in% class(x)){
    stop("Not appropriate for exact matching.  No plots generated.")
  }
  
  sd.pre <- abs(x$sum.all$"Mean Diff")
  sd.post <- abs(x$sum.matched$"Mean Diff")
  
  if (!is.null(x$q.table)) sd.post <- abs(x$sum.subclass$"Mean Diff") 
  
  ases.dat <- data.frame(es.unw = sd.pre, es.w = sd.post)
  par(mfrow=c(1,1))
  plot(c(0.85, 2.15), c(0, min(3, max(unlist(ases.dat[, 
                                                      1:2]), na.rm = TRUE))), type = "n", xaxt = "n", ylab = "Absolute Standardized Diff in Means", 
       xlab = "", main = "")
  abline(h = c(0.2, 0.4, 0.6, 0.8, 1.0))
  axis(side = 1, at = 1:2, labels = c("All Data", "Matched Data"))
  for (i in 1:nrow(ases.dat)) {
    points(1:2, abs(ases.dat[i, c("es.unw", "es.w")]), 
           type = "b", col = "grey", pch=19)
  }
  temp1 <- ases.dat[abs(ases.dat$es.unw) < abs(ases.dat$es.w),]
  for (i in 1:nrow(temp1)) {
    points(1:2, abs(temp1[i, c("es.unw", "es.w")]), type = "b", 
           col = "black", lwd = 2, pch=19)
  }
  if (max(ases.dat$es.w, na.rm = TRUE) > 3) 
    mtext(text = "Some standardized diffs in means > 3 after matching!", side = 3, 
          col = "red")
  
  if(interactive==TRUE) {
    print("To identify the variables, use first mouse button; to stop, use second.")
    identify(rep(1, length(sd.pre)),sd.pre,rownames(x$sum.all),atpen=T)
    identify(rep(2, length(sd.post)),sd.post,rownames(x$sum.all),atpen=T)
  }
}


grafic_interactiu_PM = function(df, var_imp, ratio=1, interactive = TRUE){
  
  # Funció: fa un gràfic interactiu de les diferències entre les dades abans i després de matchejar
  # Argument: df: dataframe
  #           var_imp: variable sobre la qual hem de fer el matching
  #           ratio: ratio del matching
  #           interactive: aquest gràfic permet anar clicant als punts de les variables per les quals 
  #                        volem que apareguin els noms. es pot treure
  # Retorn: cap
  
  ms = matchit(var_imp ~ ., data = df, method = "full", ratio = ratio)
  sms = summary(ms)
  plot.summary.matchit(sms, interactive=interactive)
}


grafics_complementaris_PM = function(ms){
  
  # Funció: en aquesta funció s'hi poden anar afegint gràfics
  # Arguments: ms: resultat de propensity matching amb matchit
  # Retorn: cap
  
  love.plot(bal.tab(ms), threshold = .1)
}
