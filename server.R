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
    
    # Plotting nonsense
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
    
     
   
     observe({ ## Observe?? 
       ind_df<- read.csv(file.path(here::here("indicator_df.csv")))
       ind <- ind_df %>% 
         dplyr::filter(ind == input$Indicator, 
                       epu_abbr == input$epu_abbr)%>% 
         dplyr::mutate(Time = as.numeric(Time), 
                       cat2 = as.character(cat2))
    ############## TIMESERIES PLOT ###################
       output$timeseries<- renderPlot({ 
         dat<- ind
         p1<- dat %>% 
           ggplot2::ggplot(aes(x = Time, y = Value))+
           ggplot2::geom_line( size = lwd) +
           ggplot2::geom_point( size = pcex) +
           ecodata::geom_gls(size = lwd+1, alpha = 0.5)+
           #ggplot2::geom_line(aes(x = Time, y = pred.fit), size = lwd+0.3, linetype = "dashed")+
           ggplot2::geom_line(aes(x = Time, y = pred.fit, color = cat2, group = cat), 
                           size = lwd+0.3, linetype = "dashed")+
           ggplot2::scale_color_manual(values = c("1" = "purple", "0" = "orange", "-1" = "gray"))+
           ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper, x = Time, y = Value), 
                             fill = "gray", alpha = 0.3)+
           ggplot2::ylab(("Value")) +
           ggplot2::xlab(paste("Model = ", dat$choseMod))+
           ggplot2::ggtitle(paste(input$Indicator,"-",input$epu_abbr))+
           ggplot2::theme(axis.title.y = element_text(size = 10), 
                       axis.title.x = element_text(size = 15), 
                       legend.position = "none")+
           ecodata::theme_ts()+
           ecodata::theme_title()
         p1
         })
       })
     
     # observe({ ## Observe??
     # 
     #   ind_df<- read.csv(file.path(here::here("indicator_df.csv")))
     #   ind <- ind_df %>%
     #     dplyr::filter(ind == input$Indicator,
     #                   epu_abbr == input$epu_abbr)%>%
     #     dplyr::mutate(Time = as.numeric(Time),
     #                   cat2 = as.character(cat2))
       # output$descriptionmarkdown <- renderUI({
       #   dat<- ind
       #   selectmodel <- unique(dat$choseMod)
       #   includeHTML(rmarkdown::render(input = "descriptionmarkdown.Rmd",
       #                    params = list(model = selectmodel)
       #  ))
       #   })
      # })
    #  
     observe({ ## Observe??

       ind_df<- read.csv(file.path(here::here("indicator_df.csv")))
       ind <- ind_df %>%
         dplyr::filter(ind == input$Indicator,
                       epu_abbr == input$epu_abbr)%>%
         dplyr::mutate(Time = as.numeric(Time),
                       cat2 = as.character(cat2))
    output$tableout <- DT::renderDataTable(server = FALSE,{
      dat<- ind

      dat<- dat %>% dplyr::select("Time","ind","epu_abbr","Value",
                                  "pred.fit", "pred.se.fit","choseMod",
                                  "upper", "lower")

      DT::datatable(dat, extensions=c("Buttons",'Scroller'),
                    options = list(dom = 'Bfrtip',
                                   buttons = c( 'csv',
                                                'excel')))#,

    })
     })
    
    output$markdown <- renderUI({
      HTML(markdown::markdownToHTML(knit('documentation.rmd', quiet = TRUE)))
    })
  })
    
    
    