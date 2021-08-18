shinyServer(

function(input, output){
  
  library(tidyverse)
  library(ecodata)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(ecodata)
  library(here)
  library(kableExtra)
  library(ggrepel)
  library(stringr)
  library(knitr)
  library(DT)
  
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
  x.shade.min <- 2010
  x.shade.max <- 2020
  
  
  # Managed Revenue ###################
  ind<- reactive({ 
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
    ind <- message("No Data")
  }
  })
  
  
  output$timeseries<- renderPlot({
    
   ind<- ind()
      
        gam_norm<- mgcv::gam(Value ~ s(Time, k=input$knots), data = ind, na.action = na.omit, gamma = input$gamma) # calc gam
        
        new.dat<-data.frame(Time = ind$Time, # newdata
                            Value = ind$Value) 

        # calc deriv
        fm1 <- gratia::derivatives(gam_norm)

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
        
          # Add second deriv
        #inflection <- rle(as.vector(fm1$derivative))
        
        ind2<-  data.frame(pred = mgcv::predict.gam(gam_norm, new.dat, se.fit = TRUE)) %>% # calc predicted values
          dplyr::mutate(Time = ind$Time) %>% 
          left_join(ind) %>% # join with orig data set
          dplyr::mutate(upper = pred.fit + pred.se.fit, # calc upper and lower ci 
                        lower = pred.fit - pred.se.fit)
        
        
        ind3<- ind2 %>% left_join(trend) %>%
          mutate(cat2 = case_when(upper_bound == "upper" & lower_bound == "NA" ~ 1,
                                 upper_bound == "NA" & lower_bound == "lower" ~ 0,
                                 upper_bound == "NA" & lower_bound == "NA" ~ -1))
        
        ## Andy's loop-
        catlabel <- 1
        df<- ind3 %>% select(Time, cat2) %>% 
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

        ind3<- ind3 %>% left_join(df) %>% 
          mutate(cat = as.character(cat), 
                 cat2 = as.character(cat2))
        ### Plot
        p2 <- ind3 %>% 
          ggplot2::ggplot()+
          ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
          ggplot2::geom_point(aes(x = Time, y = Value), size = pcex) +
          ecodata::geom_gls(data = ind2, aes(x = Time, y = Value), size = lwd+1, alpha = 0.5)+
          ggplot2::geom_line(aes(x = Time, y = pred.fit, color = cat2, group = cat), size = lwd+0.3, linetype = "dashed")+
          scale_color_manual(values = c("1" = "purple", "0" = "orange", "-1" = "gray"))+#, "NA" = NA))+
          ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper, x = Time, y = Value), fill = "gray", alpha = 0.3)+
          ggplot2::ylab(("Value")) +
          ggplot2::xlab(paste("AIC = ", AICcmodavg::AICc(gam_norm)))+
          ggplot2::ggtitle(paste(input$Indicator,"-",input$epu_abbr))+
          ggplot2::theme(axis.title.y = element_text(size = 10), 
                         axis.title.x = element_text(size = 15), 
                         legend.position = "none")+
          ecodata::theme_ts()+
          ecodata::theme_title()
        p2

  })
  


  output$tableout <- DT::renderDataTable(server = FALSE,{
    ind<- ind()

    gam_norm<- mgcv::gam(Value ~ s(Time, k=input$knots), data = ind, na.action = na.omit, gamma = input$gamma) # calc gam

    new.dat<-data.frame(Time = ind$Time, # newdata
                        Value = ind$Value)

    ind2<-  data.frame(pred = mgcv::predict.gam(gam_norm, new.dat, se.fit = TRUE )) %>% # calc predicted values
      dplyr::mutate(Time = ind$Time) %>%
      right_join(ind) %>% # join with orig data set
      dplyr::mutate(upper = pred.fit + pred.se.fit, # calc upper and lower ci
                    lower = pred.fit - pred.se.fit)

    DT::datatable(ind2, extensions=c("Buttons",'Scroller'),
                  options = list(dom = 'Bfrtip',
                                 buttons = c( 'csv', 
                                             'excel')))#,

    })


  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit('documentation.rmd', quiet = TRUE)))
  })

  #output$markdown <- renderUI({
  #  HTML(markdown::markdownToHTML(knit('stat_summary.rmd', quiet = TRUE)))
  #})
})