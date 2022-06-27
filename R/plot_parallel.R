plot_parallel = function(pa){
  tibble(`FA_Actual Data` = pa$fa.values,
         `FA_Simulated Data` = pa$fa.sim,
         `FA_Resampled Data` = pa$fa.simr,
         `PC_Actual Data` = pa$pc.values,
         `PC_Simulated Data` = pa$pc.sim,
         `PC_Resampled Data` = pa$pc.simr) %>%
    mutate(`Factor/Component Number` = 1:n()) %>%
    pivot_longer(names_to = "key",values_to = "value",cols = 1:6) %>%
    separate(key,c("Method","Data"),"_") %>%
    rename(Eigenvalue = value) %>%
    ggplot(aes(`Factor/Component Number`,Eigenvalue,col = Method,shape = Data))+
    geom_line(aes(group = paste(Method,Data),lty = Data != "Actual Data"))+
    geom_point(size = 3)+
    geom_hline(yintercept = 1,size = .2)+
    theme(legend.position = c(.875,.75))+
    guides(lty = "none")+
    scale_color_brewer(palette = "Set1")
}
