### Summary Stats for GAM outputs

# For each of the indicators run through multiple knot and smoother combiations to visualise where GAMs show relatively fixed outputs. 


# input<- list()
# input$epu_abbr <- "MAB"
# 
# ## Managed Landings
# managed_landings <- ecodata::comdat  %>%
#   dplyr::filter(EPU == input$epu_abbr,
#                 #dplyr::mutate(Var = gsub("MAFMC", "MABFMC", .$Var)) %>% 
#                 stringr::str_detect(Var, paste0("FMC managed species - Landings weight|JOINT managed species - Landings weight")),
#                 !stringr::str_detect(Var, "Other"),
#                 Time >= 1986)
# apex<-ecodata::hms_landings %>% 
#   dplyr::filter(stringr::str_detect(Var, "Landings")) %>% 
#   separate(Var, c("Var", "trash"), sep = "_") %>% 
#   group_by(YEAR) %>% 
#   summarise(Value = sum(Value)) %>% 
#   rename( Time = YEAR) %>% 
#   mutate(Var = c("HMS Landings"), 
#          Units = c("metric tons"), 
#          EPU = c(input$epu_abbr))
# total_landings <- ecodata::comdat  %>%
#   dplyr::filter(!stringr::str_detect(Var, "managed species"),
#                 !stringr::str_detect(Var, "Other"),
#                 !stringr::str_detect(Var, "Apex"),
#                 stringr::str_detect(Var, "Landings"),
#                 Time >= 1986,
#                 EPU == input$epu_abbr) %>% 
#   rbind(apex)
# total_landings_agg <- total_landings %>%
#   dplyr::group_by(Time) %>%
#   dplyr::summarise(Value = sum(Value)) %>% 
#   dplyr::mutate(Var = "Total",hline = mean(Value))
# managed_landings_agg <- managed_landings %>%
#   dplyr::group_by(Time) %>%
#   dplyr::summarise(Value = sum(Value)) %>% 
#   dplyr::mutate(Var = "Managed",hline = mean(Value))
# ind <- managed_landings_agg 
# 
# 
# 
# df<- data.frame()
# for (i in indicator){
#   for (k in knots){
#     for (g in gamma){
#     gam_norm<- mgcv::gam(Value ~ s(Time, k=k), data = ind, na.action = na.omit, gamma = g) # calc gam
#     aic<-AICcmodavg::AICc(gam_norm)
#     
#     df1<- data.frame(knots = k, 
#                     gamma = g,
#                     AIC = aic, 
#                     ind = c("landings_managed")) %>% 
#       rbind(df1) %>% 
#       mutate(AIC = round(AIC))
#     
#     length(unique(df1$AIC))
#     
#     range(df1$AIC)
#     
#     df1 %>% ggplot2::ggplot()+
#       ggplot2::geom_point(aes(x = knots, y = gamma, color = AIC))
#     
#     }
#   }
# }
# 
# # Plots showing number of local AIC minimums
# 
# df1 %>% ggplot2::ggplot()+
#   ggplot2::geom_point(aes(x = knots, y = gamma, color = AIC))+
#   ggtitle("landings_managed")
# 

# COmpiled list of Data
df1<- data.frame()
get_summary<- function(indicator, epu_abbr){
  
  ind<-
    if (indicator == "Revenue_Managed") { 
      apex<-ecodata::hms_landings %>% 
        dplyr::filter(stringr::str_detect(Var, "Revenue")) %>% 
        separate(Var, c("Var", "trash"), sep = "_") %>% 
        group_by(YEAR) %>% 
        summarise(Value = sum(Value)) %>% 
        rename( Time = YEAR) %>% 
        mutate(Var = c("HMS Revenue"), 
               Units = c("metric tons"), 
               EPU = c(epu_abbr))
      rev_agg <- ecodata::comdat %>% 
        dplyr::filter(stringr::str_detect(Var, "Revenue"),
                      !stringr::str_detect(Var, "Apex|prop|Other|NEFMC"), 
                      EPU == epu_abbr,
                      Time >= 1986) %>% 
        rbind(apex) %>% 
        dplyr::mutate(Status = ifelse(str_detect(Var, "managed"), 
                                      "Managed","Total")) %>% 
        dplyr::group_by(Status, Time) %>% 
        dplyr::summarise(Total = sum(Value)) 
      ind <- rev_agg %>% dplyr::filter(Status == "Managed") %>% 
        dplyr::rename(Value = Total)
      
      # Managed Landings #############################
    } else if (indicator == "Landings_Managed") {               
      managed_landings <- ecodata::comdat  %>%
        dplyr::filter(EPU == epu_abbr,
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
               EPU = c(epu_abbr))
      total_landings <- ecodata::comdat  %>%
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
      
      # Recreational Seafood Harvest ########################################
    } else if (indicator == "Recreational_Seafood_Harvest") {  
      ind <- ecodata::recdat %>% 
        dplyr::mutate(EPU = dplyr::recode(EPU, "MA"="MAB") ) %>% 
        dplyr::filter(EPU == epu_abbr,
                      Var == "Recreational Seafood")
      
      # Recreational seafood effort  #######################################
    } else if (indicator == "Recreational_Effort") { 
      ind <- ecodata::recdat %>% 
        dplyr::mutate(EPU = dplyr::recode(EPU, "MA"="MAB") ) %>% 
        dplyr::filter(EPU == epu_abbr, 
                      Var == "Recreational Effort")
      
      # Recreational effort diversity #########################################
    } else if (indicator == "Recreational_Catch_Diversity") { 
      ind <- ecodata::recdat %>% 
        dplyr::mutate(EPU = dplyr::recode(EPU, "MA"="MAB") ) %>% 
        dplyr::filter(EPU == epu_abbr, 
                      Var == "Recreational Diversity of Catch")
      
      # Zooplankton Diversity ##################################################
    } else if (indicator == "Zooplankton_Diversity"){
      ind <- ecodata::zoo_diversity %>% 
        dplyr::filter(EPU == epu_abbr)
      
      # Bottom Temperature
    } else if (indicator == "Bottom_Temp"){
      ind<- ecodata::bottom_temp %>% 
        dplyr::filter(EPU == epu_abbr) %>% 
        tidyr::complete(Time = tidyr::full_seq(min(Time):max(Time),1),
                        tidyr::nesting(Var)) %>%
        dplyr::filter(Var == "bottom temp anomaly in situ", 
                      !Value == "NA")
      
      # Heatwave Cumulative Intensity #####################################  
    } else if (indicator == "Heatwave_Cumulative_Intensity"){
      ind<- ecodata::heatwave %>% 
        dplyr::filter(Var == "cumulative intensity", 
                      EPU == epu_abbr) %>% 
        dplyr::group_by(Time, EPU, Var, Units) %>% 
        dplyr::summarise(Value = max(Value)) %>% 
        dplyr::ungroup() 
      
      # Heatwave Maximum Intensity #####################################  
    } else if (indicator == "Heatwave_Maximum_Intensity"){
      ind<- ecodata::heatwave %>% 
        dplyr::filter(Var == "maximum intensity", 
                      EPU == epu_abbr)  %>% 
        dplyr::group_by(Time, EPU, Var, Units) %>% 
        dplyr::summarise(Value = max(Value)) %>% 
        dplyr::ungroup() 
      
      # Long term sst #####################################################
    } else if (indicator == "LTSST") {                       
      ind <- ecodata::long_term_sst %>% 
        dplyr::filter(EPU == epu_abbr)
    } else {
      ind <- message("No Data")
    }

  knots<- c(1:20)
  gamma <- c(1:9)
  for (k in knots){
    for (g in gamma){
      gam_norm<- mgcv::gam(Value ~ s(Time, k=k), data = ind, na.action = na.omit, gamma = g) # calc gam
      aic<-AICcmodavg::AICc(gam_norm) # Calc AIC
      
      df1<- data.frame(knots = k, 
                       gamma = g,
                       AIC = aic, 
                       ind = c(indicator)) %>% 
        rbind(df1) %>% 
        mutate(AIC = round(AIC)) 
      



        }
  }
  
  plot<- df1 %>% ggplot2::ggplot(aes(x = knots, y = gamma, fill = AIC))+
    ggplot2::geom_tile()+
    ggplot2::scale_fill_gradientn(colours = terrain.colors(10))+
    ggtitle(paste0(indicator, "_", epu_abbr))
  
  dat<- data.frame(ind = indicator, 
                   N_AIC = length(unique(df1$AIC)), 
                   min_AIC = min(df1$AIC), 
                   max_AIC = max(df1$AIC))
  print(dat)
  print(plot)
}

get_summary("Bottom_Temp", "MAB")
get_summary("Recreational_Seafood_Harvest", "MAB")
get_summary("Recreational_Effort", "MAB")
get_summary("Landings_Managed", "MAB")
get_summary("Zooplankton_Diversity", "MAB")
