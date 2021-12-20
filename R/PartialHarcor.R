data = mtcars
vars = colnames(mtcars[1:5])
control.vars = colnames(mtcars[6:7])
partialHARcor = function(data,vars,control.vars){
  zf = paste(control.vars,collapse = "+")
  data = data %>% 
    dplyr::select(vars,control.vars) %>%
    tibble::rownames_to_column()
  rn = data$rowname
  
  partcor = data %>% 
    tidyr::gather(key,value,-control.vars,-rowname,factor_key = T) %>% 
    dplyr::group_by(key) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(lm = map(data,function(d){lm(
      formula = paste("value ~ ",zf),
      data= d)})) %>% 
    dplyr::mutate(resid = map(lm,"residuals"),
           resid = map(resid,enframe)) %>% 
    dplyr::select(key,resid) %>% 
    tidyr::unnest(cols = c(resid)) %>% 
    tidyr::spread(key,value) %>% 
    dplyr::select(-name) %>% 
    Ben::HARcor()
  
  cors = data %>% 
    select(vars) %>% 
    Ben::HARcor()
  
  c = cors[1:length(vars),2:ncol(cors)]
  p = t(partcor[1:length(vars),2:ncol(partcor)])
  c[upper.tri(c)] = p[upper.tri(p)]
  cors[1:length(vars),2:ncol(cors)] = c
  return(cors)
}
