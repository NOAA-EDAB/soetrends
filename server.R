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
    #library(patchwork)
    library(grid)
    library(cowplot)
    
    #GIS libraries
    #library(sf)
    #library(rgdal)
    #library(raster)
    #library(ggspatial)
    #library(marmap)
    
    
    #General inline text input for report
    
    #Council
    council <- "Mid-Atlantic Fishery Management Council"
    council_abbr <- "MAFMC"
    
    #Region identifiers
    epu <- "Mid-Atlantic Bight"
    epu_abbr <- "MAB"
    region <- "Mid-Atlantic"
    region_abbr <- "MA" #Some commercial data organized by "MA" or "NE" regions, not by EPU 
    
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
    #ind<- input$Indicator
    #analysis<- input$`Trend analysis Type`
    #input$Indicator == Indicator
    if (input$Indicator == "Landings_Managed") {
      ### Commercial Landings
      managed_landings <- ecodata::comdat  %>%
        dplyr::filter(stringr::str_detect(Var, paste0(council_abbr," managed species - Landings weight|JOINT managed species - Landings weight")),
                      !stringr::str_detect(Var, "Other"),
                      Time >= 1986,
                      EPU == epu_abbr)
      
      # HMS Landings
      apex<-ecodata::hms_landings %>% 
        dplyr::filter(stringr::str_detect(Var, "Landings")) %>% 
        separate(Var, c("Var", "trash"), sep = "_") %>% 
        group_by(YEAR) %>% 
        summarise(Value = sum(Value)) %>% 
        rename( Time = YEAR) %>% 
        mutate(Var = c("HMS Landings"), 
               Units = c("metric tons"), 
               EPU = c("MAB"))
      
      #Total landings
      managed_landings <- ecodata::comdat  %>%
        dplyr::filter(!stringr::str_detect(Var, "managed species"),
                      !stringr::str_detect(Var, "Other"),
                      !stringr::str_detect(Var, "Apex"),
                      stringr::str_detect(Var, "Landings"),
                      Time >= 1986,
                      EPU == epu_abbr) %>% 
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
      } else if (input$Indicator == "LTSST") {
      ind <- ecodata::long_term_sst 
      } else {
      ind <- message("No Data")
      }

    if (input$Model == "SOE_Standard") {
    p1<- ind %>% 

      ggplot2::ggplot()+
        
        #Highlight last ten years
        #ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
        #                  xmin = x.shade.min , xmax = x.shade.max,
        #                  ymin = -Inf, ymax = Inf) +
        ecodata::geom_gls(aes(x = Time, y = Value))+#,
                              #group = Var),
                          #alpha = trend.alpha, size = trend.size) +
        ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
        ggplot2::geom_point(aes(x = Time, y = Value), size = pcex) +
        #ggplot2::scale_y_continuous(labels = function(l){trans = l / 1000})+
        #ggplot2::scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
        #ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
        #ggplot2::guides(color = FALSE) +
      ggplot2::ylab(("Value")) +
      ggplot2::xlab(element_blank())+
        #ggplot2::geom_hline(aes(yintercept = hline,
        #                        color = Var),
        #                    size = hline.size,
        #                    alpha = hline.alpha,
        #                    linetype = hline.lty) +
        ggplot2::ggtitle("Indicator")+
        ggplot2::theme(axis.title.y = element_text(size = 7))+
        ecodata::theme_ts()+
        ecodata::theme_title()
    
    p1
    } else if (input$Model == "GAM"){
      
        gam_norm<- mgcv::gam(Value ~ s(Time), data = ind, na.action = na.omit)#, knots = input$knots, gamma = input$gamma)
        ind2<- data.frame(pred = predict(gam_norm)) %>% 
          mutate(Time = c(min(ind$Time):max(ind$Time))) %>% 
          left_join(ind)
        
        p2 <- ind2 %>% 
          ggplot2::ggplot()+
          ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
          ggplot2::geom_point(aes(x = Time, y = Value), size = pcex) +
          ggplot2::geom_line(aes(x = Time, y = pred))+
          ggplot2::ylab(("Value")) +
          ggplot2::xlab(element_blank())+
          ggplot2::ggtitle("Indicator")+
          ggplot2::theme(axis.title.y = element_text(size = 7))+
          ecodata::theme_ts()+
          ecodata::theme_title()
        p2
    } 
  })
}
  
  
)