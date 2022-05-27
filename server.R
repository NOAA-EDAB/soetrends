shinyServer(
  
  function(input, output){
    #remotes::install_github("noaa-edab/ecodata",build_vignettes=TRUE)
    library(tidyverse)
    library(ecodata)
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(ecodata)
    library(here)
    library(kableExtra)
    library(ggrepel)
    library(stringr)
    library(knitr)
    library(DT)
    
    ### AICc Function #####
    AICc <- function(mod) {
      K.c <- mod$rank
      N.c <- length(mod$residuals)
      AIC.c <- round(mod$aic + (2*K.c*(K.c+1)/(N.c-K.c-1)),3)
      return(AIC.c)
    }
    
    ############### SELECT INDICATOR ################################
    ind <- reactive({ 
      # Managed Revenue ###################
      if (input$Indicator == "Revenue_Managed") { 
        apex<-ecodata::hms_landings %>% 
          dplyr::filter(stringr::str_detect(Var, "Revenue")) %>% 
          separate(Var, c("Var", "trash"), sep = "_") %>% 
          group_by(YEAR) %>% 
          summarise(Value = sum(Value)) %>% 
          rename( Time = YEAR) %>% 
          mutate(Var = c("HMS Revenue"), 
                 Units = c("metric tons"), 
                 EPU = c(input$epu_abbr))
        rev_agg <- ecodata::comdat %>% 
          dplyr::filter(stringr::str_detect(Var, "Revenue"),
                        !stringr::str_detect(Var, "Apex|prop|Other|NEFMC"), 
                        EPU == input$epu_abbr,
                        Time >= 1986) %>% 
          rbind(apex) %>% 
          dplyr::mutate(Status = ifelse(str_detect(Var, "managed"), 
                                        "Managed","Total")) %>% 
          dplyr::group_by(Status, Time) %>% 
          dplyr::summarise(Total = sum(Value)) 
        ind <- rev_agg %>% dplyr::filter(Status == "Managed") %>% 
          dplyr::rename(Value = Total)
        
        # Managed Landings #############################
      } else if (input$Indicator == "Landings_Managed") {               
        managed_landings <- ecodata::comdat  %>%
          dplyr::filter(EPU == input$epu_abbr,
                        #dplyr::mutate(Var = gsub("MAFMC", "MABFMC", .$Var)) %>% 
                        stringr::str_detect(Var, paste0("FMC managed species - Landings weight|JOINT managed species - Landings weight")),
                        !stringr::str_detect(Var, "Other"),
                        Time >= 1986)
        apex<-ecodata::hms_landings %>% 
          dplyr::filter(stringr::str_detect(Var, "Landings")) %>% 
          separate(Var, c("Var", "trash"), sep = "_") %>% 
          group_by(YEAR) %>% 
          summarise(Value = sum(Value)) %>% 
          rename( Time = YEAR) %>% 
          mutate(Var = c("HMS Landings"), 
                 Units = c("metric tons"), 
                 EPU = c(input$epu_abbr))
        total_landings <- ecodata::comdat  %>%
          dplyr::filter(!stringr::str_detect(Var, "managed species"),
                        !stringr::str_detect(Var, "Other"),
                        !stringr::str_detect(Var, "Apex"),
                        stringr::str_detect(Var, "Landings"),
                        Time >= 1986,
                        EPU == input$epu_abbr) %>% 
          rbind(apex)
        total_landings_agg <- total_landings %>%
          dplyr::group_by(Time) %>%
          dplyr::summarise(Value = sum(Value)) %>% 
          dplyr::mutate(Var = "Total",hline = mean(Value))
        managed_landings_agg <- managed_landings %>%
          dplyr::group_by(Time) %>%
          dplyr::summarise(Value = sum(Value)) %>% 
          dplyr::mutate(Var = "Managed",hline = mean(Value))
        Landings_total <- total_landings_agg
        ind <- managed_landings_agg 
        
        # Recreational Seafood Harvest ########################################
      } else if (input$Indicator == "Recreational_Seafood_Harvest") {  
        ind <- ecodata::recdat %>% 
          dplyr::mutate(EPU = dplyr::recode(EPU, "MA"="MAB") ) %>% 
          dplyr::filter(EPU == input$epu_abbr,
                        Var == "Recreational Seafood")
        
        # Recreational seafood effort  #######################################
      } else if (input$Indicator == "Recreational_Effort") { 
        ind <- ecodata::recdat %>% 
          dplyr::mutate(EPU = dplyr::recode(EPU, "MA"="MAB") ) %>% 
          dplyr::filter(EPU == input$epu_abbr, 
                        Var == "Recreational Effort")
        
        # Recreational effort diversity #########################################
      } else if (input$Indicator == "Recreational_Catch_Diversity") { 
        ind <- ecodata::recdat %>% 
          dplyr::mutate(EPU = dplyr::recode(EPU, "MA"="MAB") ) %>% 
          dplyr::filter(EPU == input$epu_abbr, 
                        Var == "Recreational Diversity of Catch")
        
        # Zooplankton Diversity ##################################################
      } else if (input$Indicator == "Zooplankton_Diversity"){
        ind <- ecodata::zoo_diversity %>% 
          dplyr::filter(EPU == input$epu_abbr)
        
        # Bottom Temperature
      } else if (input$Indicator == "Bottom_Temp"){
        ind<- ecodata::bottom_temp %>% 
          dplyr::filter(EPU == input$epu_abbr) %>% 
          tidyr::complete(Time = tidyr::full_seq(min(bottom_temp$Time):max(bottom_temp$Time),1),
                          tidyr::nesting(Var)) %>%
          dplyr::filter(Var == "bottom temp anomaly in situ", 
                        !Value == "NA")
        
        # Heatwave Cumulative Intensity #####################################  
      } else if (input$Indicator == "Heatwave_Cumulative_Intensity"){
        ind<- ecodata::heatwave %>% 
          dplyr::filter(Var == "cumulative intensity", 
                        EPU == input$epu_abbr) %>% 
          dplyr::group_by(Time, EPU, Var, Units) %>% 
          dplyr::summarise(Value = max(Value)) %>% 
          dplyr::ungroup() 
        
        # Heatwave Maximum Intensity #####################################  
      } else if (input$Indicator == "Heatwave_Maximum_Intensity"){
        ind<- ecodata::heatwave %>% 
          dplyr::filter(Var == "maximum intensity", 
                        EPU == input$epu_abbr)  %>% 
          dplyr::group_by(Time, EPU, Var, Units) %>% 
          dplyr::summarise(Value = max(Value)) %>% 
          dplyr::ungroup() 
        
        # Long term sst #####################################################
      } else if (input$Indicator == "LTSST") {                       
        ind <- ecodata::long_term_sst %>% 
          dplyr::filter(EPU == input$epu_abbr)
      } else {
        
      }

    })
    
    
    ##################### RUN MODEL AND MODEL SELECTION ###########
    dat<- reactive({
      ind<- ind()
      if (length(ind$Value)<1){
        #print("ops")
        stop(safeError("The indicator selected does not exist at the Regional/EPU scale selected. Please choose different Region/EPU."))
      }
      #print(ind)
      
      sp.len <- 200 # Spline length
      nb <- 1000  # Number of bootstrap replicates
      gam.mat <- matrix(nrow=sp.len, ncol=nb) ## create a matrix to be filled with bootstrapped splines (200 x 1000)
      dif1 <- 1 # First derivative
      dif2 <- 2 # Second derivative
      ks <- 4   #If ks=3, no relationships have edf values > 2.0
      #rand <- rep(1,length(ind$Value))  
      
      rand <- data.frame(rep(1,length(ind$Value))) %>%
        dplyr::rename("rand" = "rep.1..length.ind.Value..")
      ind<- ind %>% cbind(rand)


      #ts.length <- ind$Time
      #print(rand)
      #print(ts.length)
      #### Step 1: Fit GAM to answer whether temporal autocorrelation is important? Use the residuals 
      #### from the gam and a log likelihood ratio test to calculate the "P.ac" value. A significant p.ac 
      #### value suggests a model with autocorrelated error structure explains more of the variation in the
      #### residuals of the gam model than a model without autocorrelated error structure. Thus, using a 
      #### GAMM is necessary to account for autocorrelation in the time series...use GAMM if p.ac < 0.05. 
      #### If p.ac > 0.05, then GAM will be used to look for non-linearities, so this code will also fit 
      #### the model and provide selection criteria (i.e. edf, GCV and AIC scores from GAM and Linear model (linear) to compare)
      
      gam1  <- mgcv::gam(Value ~ s(Time, bs= "tp",k = ks), optimMmethod="GCV.Cp",se = T, data = ind)
      
      linear <- mgcv::gam(Value ~ Time, method = "GCV.Cp", se = T, data = ind)
      
      dev.resid <- data.frame(stats::residuals(gam1,type='deviance')) %>% 
        dplyr::rename("dev.resid" = "stats..residuals.gam1..type....deviance..")
      ind<- ind %>% cbind(dev.resid)
      
      lme1 <- nlme::lme(dev.resid~1,random=~1|rep(1,length(Value)),
                        correlation=nlme::corAR1(form=~Time),method='ML', data = ind)
      #lme1 <- nlme::lme(dev.resid~1,random=~1|rep(1,length(Value)),correlation=nlme::corAR1(form=~Time),method='ML', data = ind)
      lm1 <- lm(ind$dev.resid~1)
      p.ac <- 1-pchisq(2*(logLik(lme1)[1]-logLik(lm1)[1]),2)    
      delta.GCV.gam.lm <- summary(gam1)$sp.criterion - summary(linear)$sp.criterion   #A negative value means the GAM with a smoother is a better model than the linear model  
      delta.AIC.gam.lm <- AICcmodavg::AICc(gam1) - AICcmodavg::AICc(linear)                                   #A negative value means the GAM with a smoother is a better model than the linear model
      dev.diff.gam.lm <- summary(gam1)$dev.expl-summary(linear)$dev.expl
      #print(ts.length)
      #### Step 2: Fit GAMM to get selection criteria for relationships where p.ac < 0.05 (i.e. edf and AIC) and 
      #### calculate deviance explained by GAMM ("gamm.dev.expl" below)
      # try(gamm <- mgcv::gamm(Value ~ s(Time, bs= "tp",k = ks), data = ind, optimMmethod="GCV.Cp",
      #                  se = T,correlation=nlme::corAR1(form=~ind$Time)))
      #if (length(gamm)==0)
      #{
      #print(imod)
      gamm<- mgcv::gamm(Value ~ s(Time, bs= "tp",k = ks), data = ind, optimMmethod="GCV.Cp",
                        se = T,correlation=nlme::corAR1(form=~Time))  
      
      #gamm <- mgcv::gamm(Value ~ s(Time),se = T, data = ind, correlation=nlme::corAR1(form=~ts.length))
      #}
      #Fit null model to compute deviance explained by gamm
      null  <- MASS::glmmPQL(Value ~ 1,random=list(rand=~1),family='gaussian', data = ind)  
      dr <- sum(residuals(gamm$gam)^2)
      dn0 <- sum(residuals(null)^2)
      gamm.dev.expl <- (dn0-dr)/dn0
      
      
      
      #Step 3. Fit linear model with autocorrelation (LMAC) to get selection criteria (i.e. AIC) and calculate deviance explained by LMAC ("lmac.dev.expl" below).
      # try(lmac <- mgcv::gamm(Value ~ Time, optimMmethod="GCV.Cp", data = ind,
      #                  se = T,random=list(rand=~1),correlation=nlme::corAR1(form=~Time)))
      # if (length(lmac)==0) 
      # {
      #print(imod)
      lmac <- mgcv::gamm(Value ~ Time,random=list(rand=~1), se = T,
                         correlation=nlme::corAR1(form=~Time), data = ind)
      # lmac <- mgcv::gamm(Value ~ Time,random=list(rand=~1),se = T,
      #                    correlation=nlme::corAR1(form=~Time), data = ind)
      # }  
      dr2 <- sum(residuals(lmac$gam)^2)
      lmac.dev.expl <- (dn0-dr2)/dn0
      
      # Calculate difference in deviance and AIC between GAMM and LMAC ("dev.diff.gamm.lmac" and "delta.AIC.gamm.lmac" respectively below).
      dev.diff.gamm.lmac <- gamm.dev.expl-lmac.dev.expl
      delta.AIC.gamm.lmac <- summary(gamm$lme)$AIC-summary(lmac$lme)$AIC   #A negative value means the GAMM is a better model than the linear model with temporal autocorrelation
      
      # Pull out relevant model outputs:
      
      #FOR GAMM:
      summary.gamm <- as.data.frame(cbind("GAMM",                         # Model name
                                          #resp.name,                      # Response variable
                                          #dri.name,                       # Pressure variable
                                          summary(gamm$lme)$AIC,          # AICc
                                          summary(gamm$lme)$logLik,       # log likelihood
                                          gamm.dev.expl,                  # Deviance explained by gamm
                                          summary(gamm$gam)$edf,          # estimated degrees of freedom
                                          summary(gamm$gam)$s.pv,         # p-value of the smoother
                                          summary(gamm$gam)$r.sq,         # R-squared value
                                          NA,                             # placeholder for GCV value used in gam
                                          NA,                             # placeholder for delta.GCV data used as selection criteria for gams
                                          as.numeric(p.ac),                           # p-value of log likelihood ratio test that tests whether the residuals from the gam are better explained by a model with or without temporal autocorrelation; p.ac values < 0.05 suggest autocorrelation is important and a gamm (instead of a gam) should be used.
                                          delta.AIC.gamm.lmac,            # delta AIC between gamm and lmac models; negative value means gamm is a better model than LMAC
                                          dev.diff.gamm.lmac              # difference in deviance explained between gamm and lmac
      ))
      
      colnames(summary.gamm)<- c("MODEL", "AICc", "logLik","dev.expl", "edf", "Pvalue","R-squared","GCV","delta.GCV","p.ac","delta.AIC","diff.dev.expl")
      
      #FOR Linear Model with Auto Correlation
      summary.lmac <- as.data.frame(cbind("LMAC",                         # Model name
                                          #resp.name,                      # Response variable 
                                          #dri.name,                       # Pressure variable
                                          summary(lmac$lme)$AIC,          # AICc
                                          summary(lmac$lme)$logLik,       # log likelihood
                                          lmac.dev.expl,                  # Deviance explained by LMAC
                                          summary(lmac$gam)$residual.df,  # residual degrees of freedom
                                          summary(lmac$gam)$p.pv[[2]],    # p-value of the null hypothesis
                                          summary(lmac$gam)$r.sq,         # R-squared value
                                          NA,                             # placeholder for GCV value used in gam
                                          NA,                             # placeholder for delta.GCV data used as selection criteria for gams
                                          NA,                             # placeholder for p.ac for gamm
                                          NA,                             # placeholder for delta AIC                                      
                                          NA                              # placeholder for diff in deviance explained
      ))                          
      
      colnames(summary.lmac)<- c("MODEL", "AICc", "logLik","dev.expl", "edf", "Pvalue","R-squared","GCV","delta.GCV","p.ac","delta.AIC","diff.dev.expl")
      
      #FOR GAM
      summary.gam1 <- as.data.frame(cbind("GAM",                          # Model name
                                          #resp.name,                       # Response variable
                                          #dri.name,                        # Pressure variable
                                          AICcmodavg::AICc(gam1),                      # AICc
                                          logLik(gam1),                    # Log likelihood
                                          summary(gam1)$dev.expl,          # deviance explained by gam
                                          summary(gam1)$edf,               # estimated degrees of freedom
                                          summary(gam1)$s.pv,              # p-value of the smoother
                                          summary(gam1)$r.sq,              # R-squared value
                                          summary(gam1)$sp.criterion,      # GCV value for gam
                                          delta.GCV.gam.lm,                # GAM GCV score minus LM GCV score; #A negative value means the GAM with a smoother is a better model than the linear model  
                                          NA,                              # placeholder for p.ac value for gamm
                                          delta.AIC.gam.lm,                # delta AICc between gam and linear model; a negaive value means the GAM with a smoother is a better model than the linear model
                                          dev.diff.gam.lm                  # Difference in deviance explained between the gam and the linear model
      ))
      
      colnames(summary.gam1)<- c("MODEL", "AICc", "logLik","dev.expl", "edf", "Pvalue","R-squared","GCV","delta.GCV","p.ac","delta.AIC","diff.dev.expl")
      
      #FOR LINEAR MODEL
      summary.linear <- as.data.frame(cbind("Linear",                     # Model name
                                            #resp.name,                          # Response variable 
                                            #dri.name,                           # Pressure variable
                                            AICcmodavg::AICc(linear),                       # AICc
                                            logLik(linear),                     # Log likelihood
                                            summary(linear)$dev.expl,           # deviance explained by linear model
                                            summary(linear)$residual.df,        # residual degrees of freedom
                                            summary(linear)$p.pv[[2]],          # p-value of the null hypothesis
                                            summary(linear)$r.sq,               # R-squared value
                                            summary(linear)$sp.criterion,       # GCV value for linear model
                                            NA,                                 # placeholder for delta.GCV data used as selection criteria for gams
                                            NA,                                 # placeholder for p.ac for gamm
                                            NA,                                 # placeholder for delta AIC                                      
                                            NA                                  # placeholder for diff in deviance explained
      ))                          
      
      colnames(summary.linear)<- c("MODEL",  "AICc", "logLik","dev.expl", "edf", "Pvalue","R-squared","GCV","delta.GCV","p.ac","delta.AIC","diff.dev.expl")
      
      
      
      
      ### Identify "best model"#####
      ### 1a) Is p.ac <= 0.05? If yes, keep and evaluate selection criteria between GAMM and LMAC as best model.If no, move to step 2.
      ### 1b) Is GAMM edf > 2.0 for GAMM? If yes, keep. If no, LMAC is best.
      ### 1c) Is delta.AIC > 2.0 between GAMM and LMAC. If yes, GAMM is best. If no, LMAC is most parsimonious.
      ### 2a) If p.ac > 0.05, then revert to GAM model and ask if edf of GAM > 2.0? If yes, keep GAM. If no, linear model is best model.
      ### 2b) Is GCV minimized in GAM compared to Linear model? If delta.GCV.gam.lm is negative then keep GAM. If delta.GCV.gam.lm is positive then linear model is best.
      ### 2c) Is deltaAIC > 2.0 for GAM? If yes, then GAM is best model. If no, then linear model is best model.
      
      summary.gamm$best.model = ifelse(as.numeric(as.character(summary.gamm$p.ac))<=0.05,
                                       ifelse(as.numeric(as.character(summary.gamm$edf))>=1.99,
                                              ifelse(as.numeric(as.character(summary.gamm$delta.AIC))>=2.0,"yes","no"),"no"),"no")
      
      summary.lmac$best.model = ifelse(as.numeric(as.character(summary.gamm$p.ac))<=0.05,
                                       ifelse(as.numeric(as.character(summary.gamm$edf))>=1.99,
                                              ifelse(as.numeric(as.character(summary.gamm$delta.AIC))>=2.0,"no","yes"),"yes"),"no")
      
      summary.gam1$best.model = ifelse(as.numeric(as.character(summary.gamm$p.ac))>0.05,
                                       ifelse(as.numeric(as.character(summary.gam1$edf))>1.99,
                                              ifelse(as.numeric(as.character(summary.gam1$delta.GCV))<0,
                                                     ifelse(as.numeric(as.character(summary.gam1$delta.AIC))<=-2.0,"yes","no"),"no"),"no"),"no")
      
      summary.linear$best.model = ifelse(as.numeric(as.character(summary.gamm$p.ac))>0.05,
                                         ifelse(as.numeric(as.character(summary.gam1$edf))<1.99,"yes",
                                                ifelse(as.numeric(as.character(summary.gam1$delta.GCV))>0,"yes",
                                                       ifelse(as.numeric(as.character(summary.gam1$delta.AIC))<=-2.0,"no","yes"))),"no")
      
      
      
      allSummary<- data.frame()
      allSummary <- rbind(allSummary, summary.gamm, summary.lmac, summary.gam1,
                          summary.linear) 
      choseMod<- allSummary %>% dplyr::filter(allSummary$best.model == "yes")
      #print(choseMod)
      new.dat<-data.frame(Time = ind$Time, # newdata
                          Value = ind$Value) 
      
      dat<- if(choseMod$MODEL == "GAM"){
        dat<- data.frame(pred = mgcv::predict.gam(gam1, new.dat, se.fit = TRUE)) %>% # calc predicted values
          dplyr::mutate(Time = ind$Time) %>% 
          left_join(ind) %>% # join with orig data set
          dplyr::mutate(upper = pred.fit + pred.se.fit, # calc upper and lower ci 
                        lower = pred.fit - pred.se.fit, 
                        choseMod = c("GAM"))
        fm1 <- gratia::derivatives(gam1)
        trend <- fm1 %>%
          mutate(upper_bound = ifelse(upper < 0,
                                      "upper", "NA"),
                 lower_bound = ifelse(lower > 0,
                                      "lower", "NA")) %>%
          mutate(Time = round(data)) %>%
          group_by(Time) %>%
          slice(1) %>%
          ungroup() %>%
          select(Time, upper_bound, lower_bound)
        dat<- dat %>% left_join(trend) %>% 
          mutate(cat2 = case_when(upper_bound == "upper" & lower_bound == "NA" ~ 1,
                                  upper_bound == "NA" & lower_bound == "lower" ~ 0,
                                  upper_bound == "NA" & lower_bound == "NA" ~ -1))
      } else if(choseMod$MODEL == "Linear"){
        dat<- data.frame(pred = predict(linear, new.dat, se.fit = TRUE)) %>% # calc predicted values
          dplyr::mutate(Time = ind$Time) %>% 
          left_join(ind) %>% # join with orig data set
          dplyr::mutate(upper = pred.fit + pred.se.fit, # calc upper and lower ci 
                        lower = pred.fit - pred.se.fit, 
                        choseMod = c("Linear"),
                        upper_bound = ifelse(linear$coefficients[2] > 0, "upper", "NA"),
                        lower_bound = ifelse(linear$coefficients[2] < 0, "lower", "NA")) %>% 
          dplyr::mutate(cat2 = case_when(upper_bound == "upper" ~ 0,
                                         lower_bound == "lower" ~ 1,
                                         upper_bound == "NA" & lower_bound == "NA" ~ -1))
      } else if(choseMod$MODEL == "GAMM"){
        dat <- data.frame(pred = mgcv::predict.gam(gamm$gam, new.dat, se.fit = TRUE)) %>% # calc predicted values
          dplyr::mutate(Time = ind$Time) %>% 
          left_join(ind) %>% # join with orig data set
          dplyr::mutate(upper = pred.fit + pred.se.fit, # calc upper and lower ci 
                        lower = pred.fit - pred.se.fit, 
                        choseMod = c("GAMM"))
        fm1 <- gratia::derivatives(gamm$gam)
        trend <- fm1 %>%
          mutate(upper_bound = ifelse(upper < 0,"upper", "NA"),
                 lower_bound = ifelse(lower > 0,"lower", "NA")) %>%
          mutate(Time = round(data)) %>%
          group_by(Time) %>%
          slice(1) %>%
          ungroup() %>%
          select(Time, upper_bound, lower_bound)
        dat<- dat %>% left_join(trend) %>% 
          mutate(cat2 = case_when(upper_bound == "upper" & lower_bound == "NA" ~ 1,
                                  upper_bound == "NA" & lower_bound == "lower" ~ 0,
                                  upper_bound == "NA" & lower_bound == "NA" ~ -1))
      } else if(choseMod$MODEL == "LMAC"){
        dat<- data.frame(pred = mgcv::predict.gam(lmac$gam, new.dat, se.fit = TRUE)) %>% # calc predicted values
          dplyr::mutate(Time = ind$Time) %>% 
          left_join(ind) %>% # join with orig data set
          dplyr::mutate(upper = pred.fit + pred.se.fit, # calc upper and lower ci 
                        lower = pred.fit - pred.se.fit, 
                        choseMod = c("LMAC"), 
                        upper_bound = ifelse(lmac$lme$coefficients$fixed[2] > 0, "upper", "NA"),
                        lower_bound = ifelse(lmac$lme$coefficients$fixed[2] < 0, "lower", "NA")) %>% 
          dplyr::mutate(cat2 = case_when(upper_bound == "upper" ~ 0,
                                         lower_bound == "lower" ~ 1,
                                         upper_bound == "NA" & lower_bound == "NA" ~ -1))
      } else(print("No Model"))
      
      ###### Andy's loop-
      catlabel <- 1
      df<- dat %>% select(Time, cat2) %>%
        mutate(change = cat2,
               cat = NA)
      for (irow in 1:nrow(df)) {
        #print(irow)
        if (irow == 1) {
          df$cat[1] <- catlabel
          next
        }
        
        if ((df$change[irow]-df$change[irow-1]) == 0) {
        } else {
          catlabel=catlabel + 1
        }
        df$cat[irow] <- catlabel
        #
      }
      
      dat<- dat%>% left_join(df) %>%
        mutate(cat = as.character(cat),
               cat2 = as.character(cat2))
      #write.csv(dat, "lsst.csv")
    })
    
    
    
    
    ############## TIMESERIES PLOT ###################
    output$timeseries<- renderPlot({ 
      
      dat <- dat()
      
      shade.alpha <- 0.3
      shade.fill <- "lightgrey"
      lwd <- 1
      pcex <- 2
      trend.alpha <- 0.5
      trend.size <- 2
      hline.size <- 1
      hline.alpha <- 0.35
      hline.lty <- "dashed"
      label.size <- 5
      hjust.label <- 1.5
      letter_size <- 4
      feeding.guilds1<- c("Piscivore","Planktivore","Benthivore","Benthos")
      feeding.guilds <- c("Apex Predator","Piscivore","Planktivore","Benthivore","Benthos")
      x.shade.min <- 2011
      x.shade.max <- 2021
      
      
      
      ### New plot
      p3<- dat %>% 
        ggplot2::ggplot(aes(x = Time, y = Value))+
        ggplot2::geom_line( size = lwd) +
        ggplot2::geom_point( size = pcex) +
        ecodata::geom_gls(size = lwd+1, alpha = 0.5)+
        #ggplot2::geom_line(aes(x = Time, y = pred.fit), size = lwd+0.3, linetype = "dashed")+
        ggplot2::geom_line(aes(x = Time, y = pred.fit, color = cat2, group = cat), size = lwd+0.3, linetype = "dashed")+
        scale_color_manual(values = c("1" = "purple", "0" = "orange", "-1" = "gray"))+#, "NA" = NA))+
        ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper, x = Time, y = Value), fill = "gray", alpha = 0.3)+
        ggplot2::ylab(("Value")) +
        ggplot2::xlab(paste("Model = ", dat$choseMod))+
        ggplot2::ggtitle(paste(input$Indicator,"-",input$epu_abbr))+
        ggplot2::theme(axis.title.y = element_text(size = 10), 
                       axis.title.x = element_text(size = 15), 
                       legend.position = "none")+
        ecodata::theme_ts()+
        ecodata::theme_title()
      
      p3 
     
     
    })

   
      output$descriptionmarkdown <- renderUI({
        dat<- dat()
        selectmodel <- unique(dat$choseMod)
        includeHTML(
          rmarkdown::render(input = "descriptionmarkdown.Rmd", 
                            params = list(model = selectmodel)
        ))
        
        
      #HTML(markdown::markdownToHTML(knit('descriptionmarkdown.rmd', quiet = TRUE)))
      })
    #})
      
    output$tableout <- DT::renderDataTable(server = FALSE,{
      dat<- dat()
      
      dat<- dat %>% dplyr::select("Time","Var","EPU","Units","Value",
                                  "pred.fit", "pred.se.fit","choseMod", 
                                  "upper", "lower")
      
      DT::datatable(dat, extensions=c("Buttons",'Scroller'),
                    options = list(dom = 'Bfrtip',
                                   buttons = c( 'csv',
                                                'excel')))#,
      
    })
    
    
    output$markdown <- renderUI({
      HTML(markdown::markdownToHTML(knit('documentation.rmd', quiet = TRUE)))
    })
    
  })








