shinyServer(

function(input, output){
  output$timeseries<- renderPlot({
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
        dplyr::filter(stringr::str_detect(Var, paste0(council_abbr," managed species - Landings weight|JOINT managed species - Landings weight")),
                      !stringr::str_detect(Var, "Other"),
                      Time >= 1986,
                      EPU == input$epu_abbr)
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
   
      
        gam_norm<- mgcv::gam(Value ~ s(Time, k=input$knots), data = ind, na.action = na.omit, gamma = input$gamma) # calc gam
        
        new.dat<-data.frame(Time = ind$Time, # newdata
                          Value = ind$Value) 
        
        ind2<-  data.frame(pred = mgcv::predict.gam(gam_norm, new.dat, se.fit = TRUE )) %>% # calc predicted values
          dplyr::mutate(Time = ind$Time) %>% 
          left_join(ind) %>% # join with orig data set
          dplyr::mutate(upper = pred.fit + pred.se.fit, # calc upper and lower ci 
                        lower = pred.fit - pred.se.fit)
        
        ### Add derivative calculation!!!!!!!!!!!!!!!!!!! and plotting prep 
        
        p2 <- ind2 %>% 
          ggplot2::ggplot()+
          ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
          ggplot2::geom_point(aes(x = Time, y = Value), size = pcex) +
          ecodata::geom_gls(aes(x = Time, y = Value), size = lwd+1, alpha = 0.5)+
          ggplot2::geom_line(aes(x = Time, y = pred.fit), size = lwd+1, color = "gray")+
          ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper, x = Time, y = Value), fill = "gray", alpha = 0.3)+
          ggplot2::ylab(("Value")) +
          ggplot2::xlab(element_blank())+
          ggplot2::ggtitle(paste(input$Indicator,"-",input$epu_abbr))+
          ggplot2::theme(axis.title.y = element_text(size = 7))+
          ecodata::theme_ts()+
          ecodata::theme_title()
        p2

  })
  
  output$modelSummary<-renderPrint({
      gam_norm2<- mgcv::gam(Value ~ s(Time, k=input$knots), data = ind, na.action = na.omit, gamma = input$gamma) 
      AICcmodavg::AICc(gam_norm2)
  })
  output$table <- renderDataTable({ind2})
  
  output$download <- downloadHandler(
    filename = function(){paste0(input$Indicator, "-", 
                                 input$epu_abbr, "-knots", 
                                 input$knots,".csv")}, 
    content = function(fname){
      write.csv(ind2, fname)
    }
  )

  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit('documentation.rmd', quiet = TRUE)))
  })
}
  
  
)