# En aquest arxiu afegeixo gràfics que són útils per entendre els models de supervivència

require(ggplot2)
require(survival)

grafic_KM = function(mort, temps, var_imp, ylim=c(0,1)){
  
  # Funció: dibuixa dues corbes de Kaplan Meier diferenciades per la variable var_imp
  # Arguments: mort: vector categòric de mort o no
  #            temps: temps de la mort
  #            var_imp: vector de la variable que volem estudiar
  #            ylim: límits de la variable y
  # Retorn: cap
  
  surv = cbind.data.frame(temps, mort, var_imp)
  colnames(surv)=c("Temps", "Mort", "Variable")
  sfit <- survfit(Surv(Temps, Mort)~strata(var_imp), data=surv)
  
  ggsurvplot(
    sfit,                     # survfit object with calculated statistics.
    data = surv,  # data used to fit survival curves. 
    risk.table = TRUE,       # show risk table.
    legend.labs=c(levels(var_imp)[1], levels(var_imp)[2]),
    #  pval = TRUE,             # show p-value of log-rank test.
    conf.int = TRUE,         # show confidence intervals for 
    # point estimaes of survival curves.
    #xlim = c(0,90),        # present narrower X axis, but not affect
    # survival estimates.
    ylim = ylim,
    #break.time.by = 10,     # break X axis in time intervals by 500.
    ggtheme = theme_minimal(), # customize plot and risk table with a theme.
    risk.table.y.text.col = T, # colour risk table text annotations.
    risk.table.y.text = FALSE # show bars instead of names in text annotations
    #                            # in legend of risk table
    
  ) 
}

grafic_KM_2 = function(cox, df){
  
  # Funció: aquesta funció és bàsicament per tenir-la aquí, però és més útil fer servir directament la 
  #         funció ggadjustedcurves amb les nostres dades
  
  ggadjustedcurves(cox,
                   data = df,
                   individual.curves = T,
                   variable = "Variable",
                   method = "average",
                   conf.int = TRUE,
                   conf.int.style = "step",
                   conf.int.alpha = 0.2,
                   legend.labs=c("No", "Sí"),
                   xlim=c(0,90),
                   ylim=c(0.6,1),
                   ggtheme = theme_bw(),
                   break.time.by = 10,
                   risk.table = TRUE,       # show risk table.
                   risk.table.y.text.col = T, # colour risk table text annotations.
                   risk.table.y.text = F # show bars instead of names in text annotations
  )
}

grafic_CR = function(esdeveniment, temps, var_imp){
  
  # Funció: dibuixa les corbes de competing risks
  # Arguments: esdeveniment: variable categòrica de mort, alta, censura
  #            temps: temps de l'esdeveniment
  #            var_imp: variable d'interès (dicotòmica)
  # Retorn: dos gràfics, un amb panells i l'altre sense
  
  crfit = cuminc(ftime = temps, fstatus = esdeveniment, group = var_imp)
  plot1 = ggcompetingrisks(crfit, multiple_panels = F, conf.int = F)
  plot2 = ggcompetingrisks(crfit, multiple_panels = T, conf.int = T)
  plots = list(plot1, plot2)
  return(plots)
}

grafics_supervivencia_complementaris = function(cox_model){
  
  # Funció; aquesta funció és aquí per tenir controlat el forest plot, segurament es pugui ampliar quan se 
  #         m'acudeixin altres plots interessants
  
  ggforest(cox_model)
  
}