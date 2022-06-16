## indicator df functions

get_rev_mananged<- function(epu_abbr) {
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
    dplyr::summarise(Total = sum(Value)) %>% 
    dplyr::ungroup()
  ind <- rev_agg %>% dplyr::filter(Status == "Managed") %>% 
    dplyr::rename(Value = Total) %>% 
    dplyr::mutate(ind = c("Revenue_Managed"), 
                  epu_abbr = c(epu_abbr))%>% 
    dplyr::select(Time, Value, ind, epu_abbr)
}

get_landings_managed<- function(epu_abbr){
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
  ind <- managed_landings_agg %>% 
    dplyr::mutate(ind = c("Landings_Managed"), 
                  epu_abbr = c(epu_abbr))%>% 
    dplyr::select(Time, Value, ind, epu_abbr)
}

get_rec_harvest<- function(epu_abbr){
  ind <- ecodata::recdat %>% 
    dplyr::mutate(EPU = dplyr::recode(EPU, "MA"="MAB") ) %>% 
    dplyr::filter(EPU == epu_abbr,
                  Var == "Recreational Seafood") %>% 
    dplyr::mutate(ind = c("Recreational_Seafood_Harvest"), 
                  epu_abbr = c(epu_abbr))%>% 
    dplyr::select(Time, Value, ind, epu_abbr)
}

get_rec_effort<- function(epu_abbr){
  ind <- ecodata::recdat %>% 
    dplyr::mutate(EPU = dplyr::recode(EPU, "MA"="MAB") ) %>% 
    dplyr::filter(EPU == epu_abbr, 
                  Var == "Recreational Effort")%>% 
    dplyr::mutate(ind = c("Recreational_Effort"), 
                  epu_abbr = c(epu_abbr))%>% 
    dplyr::select(Time, Value, ind, epu_abbr)
}

get_rec_catch_div<- function(epu_abbr){
  ind <- ecodata::recdat %>% 
    dplyr::mutate(EPU = dplyr::recode(EPU, "MA"="MAB") ) %>% 
    dplyr::filter(EPU == epu_abbr, 
                  Var == "Recreational Diversity of Catch")%>% 
    dplyr::mutate(ind = c("Recreational_Catch_Diversity"), 
                  epu_abbr = c(epu_abbr))%>% 
    dplyr::select(Time, Value, ind, epu_abbr)
}

get_zoo_div<-function(epu_abbr){
  ind <- ecodata::zoo_diversity %>% 
    dplyr::filter(EPU == epu_abbr)%>% 
    dplyr::mutate(ind = c("Zooplankton_Diversity"), 
                  epu_abbr = c(epu_abbr))%>% 
    dplyr::select(Time, Value, ind, epu_abbr)
}       

get_bottom_temp <- function(epu_abbr){
  ind<- ecodata::bottom_temp %>% 
    dplyr::filter(EPU == epu_abbr) %>% 
    tidyr::complete(Time = tidyr::full_seq(min(ecodata::bottom_temp$Time):max(ecodata::bottom_temp$Time),1),
                    tidyr::nesting(Var)) %>%
    dplyr::filter(Var == "bottom temp anomaly in situ", 
                  !Value == "NA")%>% 
    dplyr::mutate(ind = c("Bottom_Temp"), 
                  epu_abbr = c(epu_abbr)) %>% 
    dplyr::select(Time, Value, ind, epu_abbr)
}

get_hw_cumu <- function(epu_abbr){
  ind<- ecodata::heatwave %>% 
    dplyr::filter(Var == "cumulative intensity", 
                  EPU == epu_abbr) %>% 
    dplyr::group_by(Time, EPU, Var, Units) %>% 
    dplyr::summarise(Value = max(Value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(ind = c("Heatwave_Cumulative_Intensity"), 
                  epu_abbr = c(epu_abbr))%>% 
    dplyr::select(Time, Value, ind, epu_abbr)
}

get_hw_max <- function(epu_abbr){
  ind<- ecodata::heatwave %>% 
    dplyr::filter(Var == "maximum intensity", 
                  EPU == epu_abbr) %>% 
    dplyr::group_by(Time, EPU, Var, Units) %>% 
    dplyr::summarise(Value = max(Value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(ind = c("Heatwave_Maximum_Intensity"), 
                  epu_abbr = c(epu_abbr))%>% 
    dplyr::select(Time, Value, ind, epu_abbr)
}


rev_man_MAB <- get_rev_mananged("MAB")
rev_man_GOM <- get_rev_mananged("GOM")
rev_man_GB <- get_rev_mananged("GB")
landings_managed_MAB<- get_landings_managed("MAB")
landings_managed_GOM<- get_landings_managed("GOM")
landings_managed_GB<- get_landings_managed("GB")
rec_harvest_MAB <- get_rec_harvest("MAB")
rec_harvest_NE <- get_rec_harvest("NE")
rec_effort_MAB<- get_rec_effort("MAB")
rec_effort_NE<- get_rec_effort("NE")
rec_catch_div_MAB<- get_rec_catch_div("MAB")
rec_catch_div_NE<- get_rec_catch_div("NE")
zoo_div_MAB <- get_zoo_div("MAB")
zoo_div_GOM <- get_zoo_div("GOM")
zoo_div_GB <- get_zoo_div("GB")
bottom_temp_MAB <- get_bottom_temp("MAB")
bottom_temp_GOM <- get_bottom_temp("GOM")
bottom_temp_GB <- get_bottom_temp("GB")
hw_cumu_MAB <- get_hw_cumu("MAB")
hw_cumu_GOM <- get_hw_cumu("GOM")
hw_cumu_GB <- get_hw_cumu("GB")
hw_max_MAB <- get_hw_max("MAB")
hw_max_GOM <- get_hw_max("GOM")
hw_max_GB <- get_hw_max("GB")
ltsst<- ecodata::long_term_sst %>% 
  dplyr::mutate(ind = c("LTSST"), 
                epu_abbr = c("ALL"))%>% 
  dplyr::select(Time, Value, ind, epu_abbr)

indicator_list<- list(rev_man_MAB, rev_man_GOM, rev_man_GB, 
                      landings_managed_MAB, landings_managed_GOM, landings_managed_GB, 
                      rec_harvest_MAB, rec_harvest_NE, 
                      rec_effort_MAB, rec_effort_NE, 
                      rec_catch_div_MAB, rec_catch_div_NE, 
                      zoo_div_MAB, zoo_div_GOM, zoo_div_GB,
                      bottom_temp_MAB, bottom_temp_GOM, bottom_temp_GB, 
                      hw_cumu_MAB, hw_cumu_GOM, hw_cumu_GB, 
                      hw_max_MAB, hw_max_GOM, hw_max_GB, ltsst)

df<- data.frame()
for (i in indicator_list){
  df<- df %>% rbind(mod_select(i))
}

write.csv(df, file = "indicator_df.csv")
