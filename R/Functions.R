# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
require(tidyverse)
require(kableExtra)
require(magrittr)

theme_ang = function(){
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = .25))
}

r2tof2 = function(r2){f2 = r2/(1-r2);return(f2)}
write.clip = function(data){clip <- pipe("pbcopy", "w");write.table(data, file=clip, sep = '\t', row.names = FALSE,quote = F);close(clip)}

new.describe <- function(df){


  descr.output <- df %>%
    psych::describe() %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    select(var,mean,sd,n, min, max)

  descr.output %<>% mutate(


    mean.chr = ifelse(min==0 & max==1, paste0(round(mean*100, 2), "%"),
                      ifelse(mean > 999, formatC(mean, format="d", big.mark=","),
                             round(mean, 2))),
    sd.chr = ifelse(min==0 & max==1, " ",
                    ifelse(mean > 999, formatC(sd, format="d", big.mark=","), round(sd, 2))),
    n.chr = formatC(n, format="d", big.mark=",")

  )


  descr.output2 <- descr.output %>% select(var, "M" = mean.chr, "SD" = sd.chr, "n" = n.chr) %>% t() %>% data.frame() %>% tibble::rownames_to_column("var")

  header.true <- function(df) {

    new.col <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)

    names(df) <- new.col[1, ]
    df[-1,]
  }


  descr.output2 <- header.true(descr.output2)

  descr.output2 <- data.frame(lapply(descr.output2, as.character), stringsAsFactors=FALSE)

  descr.output2

}
corstars <- function(x){
  require(Hmisc)

  col.end <- ncol(x)
  col.start <- 1
  x <- as.matrix(x)
  R <- rcorr(x)$r
  p <- rcorr(x)$P

  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***",
                    ifelse(p < .01, "**",
                           ifelse(p < .05, "*", # significant
                                  ifelse(p < 0.1, "†", " ")))) #marginal

  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]


  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")

  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)


  return(Rnew)
}
HARcor = function (df, describe = TRUE, numbers = TRUE, headers = NULL, spots = NULL, copy = TRUE, names = NULL, full.labels = FALSE) {
  require(tidyverse)
  require(kableExtra)
  require(magrittr)
  require(clnR2)

    if (is.null(names)) {
    corrtab <- df %>% corstars() %>%
      rownames_to_column(., var = "var") %>% slice(-1)
  }
  else {
    corrtab <- df %>% corstars() %>%
      cbind.data.frame(var = names, .) %>% slice(-1)
  }
  corrtab %<>% tibble::add_row(var = names(corrtab)[2], .before = 1)
  corrtab %<>% dplyr::mutate(ID = row_number(), dot = ". ",
                             num.var = paste0(ID, dot, var))
  corrtab %<>% select(-c(ID, dot, var))
  corrtab %<>% relocate(num.var) %>% rename(var = num.var)
  corrtab <- data.frame(lapply(corrtab, as.character), stringsAsFactors = FALSE)
  corrtab[1, -c(1)] <- " "
  for (i in 1:nrow(corrtab)) {
    corrtab[i, -c(1)] %<>% str_replace("0.", ".")
  }
  desc.output <- df %>% new.describe()
  if (numbers == TRUE) {
    new.col.names <- c("var", 1:(ncol(corrtab) - 1))
    colnames(corrtab) <- new.col.names
    colnames(desc.output) <- new.col.names
  }
  else {
  }
  if (numbers == FALSE & full.labels == TRUE) {
    new.col.names <- c("var", names)
    colnames(corrtab) <- new.col.names
    colnames(desc.output) <- new.col.names
  }
  else {
  }
  if (describe == TRUE) {
    corrtab <- dplyr::bind_rows(corrtab, desc.output)
  }
  else {
  }
  if (length(headers) == 1) {
    corrtab %<>% tibble::add_row(var = headers[1], .before = spots[1])
    corrtab[spots[1], -c(1)] <- " "
  }
  else if (length(headers) == 2) {
    corrtab %<>% tibble::add_row(var = headers[1], .before = spots[1])
    corrtab %<>% tibble::add_row(var = headers[2], .before = spots[2] +
                                   1)
    corrtab[spots[1], -c(1)] <- " "
    corrtab[spots[2] + 1, -c(1)] <- " "
  }
  else if (length(headers) == 3) {
    corrtab %<>% tibble::add_row(var = headers[1], .before = spots[1])
    corrtab %<>% tibble::add_row(var = headers[2], .before = spots[2] +
                                   1)
    corrtab %<>% tibble::add_row(var = headers[3], .before = spots[3] +
                                   2)
    corrtab[spots[1], -c(1)] <- " "
    corrtab[spots[2] + 1, -c(1)] <- " "
    corrtab[spots[3] + 2, -c(1)] <- " "
  }
  else if (length(headers) == 4) {
    corrtab %<>% tibble::add_row(var = headers[1], .before = spots[1])
    corrtab %<>% tibble::add_row(var = headers[2], .before = spots[2] +
                                   1)
    corrtab %<>% tibble::add_row(var = headers[3], .before = spots[3] +
                                   2)
    corrtab %<>% tibble::add_row(var = headers[4], .before = spots[4] +
                                   3)
    corrtab[spots[1], -c(1)] <- " "
    corrtab[spots[2] + 1, -c(1)] <- " "
    corrtab[spots[3] + 2, -c(1)] <- " "
    corrtab[spots[4] + 3, -c(1)] <- " "
  }
  else {
  }
  if (copy == FALSE) {
    corrtab %>% kable(format = "html") %>% kable_styling(bootstrap_options = c("striped",
                                                                               "hover", "responsive"), font_size = 12, full_width = F)
  }
  else {
    corrtab
  }
}
alpha_ordinal = function(data,check.keys=T){
  data = lapply(data,as.ordered) %>% as.data.frame()
  cor = polycor::hetcor(data)
  cor = cor$correlations
  alpha = psych::alpha(cor,check.keys = check.keys)
  return(alpha)
}
No_Out = function(DataCompleta,Data,alpha = .999){
  mahal = mahalanobis(Data,colMeans(Data, na.rm=T),cov(Data,use="pairwise.complete.obs"))
  cutoff = qchisq(alpha,ncol(Data))
  remain = mahal<cutoff
  p = ggplot(Data %>% mutate(ID = 1:nrow(Data),Distance = mahal,Outlier = remain),aes(ID,Distance,color=Outlier))+geom_point()+theme(legend.position= "none")
  print(p)
  n = nrow(Data)-sum(remain)
  print(paste(n,"cases were multivariate outliers"))
  return(DataCompleta %>% filter(remain))
}
DiscVal = function(Reliability,LavMod){
  #AVE = reliability(LavMod)
  AVE = Reliability
  AVE = AVE %>% t   %>%  as.data.frame()%>% rownames_to_column("Variable")  %>% select(Variable,avevar)
  AVE = AVE %>% filter(Variable!="total")
  cor = lavInspect(LavMod,"cor.lv")
  Result = cbind(AVE,cor %>% as_tibble()) %>%
    gather(var,cor,-c(Variable,avevar)) %>%
    mutate(cor2 = cor^2,
           Dif = avevar-cor2,
           Valid = ifelse(Dif>0,T,F)) %>%
    select(Variable, var,avevar,cor,cor2,Dif,Valid) %>%
    filter(Variable != var) %>%
    mutate(Dif = as.numeric(Dif))

  p = Result %>%
    select(Variable, var, Dif,Valid) %>%
    as_tibble() %>%
    mutate(Dif = as.numeric(Dif)) %>%
    ggplot(aes(Variable, var, fill = Dif)) +
    geom_tile() +
    geom_point(label = "X",shape=4,size = 5,data=. %>% filter(Valid)) +
    scale_fill_gradient2(low = "dark red",
                         mid = "white",
                         high = "dark green") +
    theme_minimal()+
    theme(legend.position = "bottom")+
    labs(y = "Variable",
         fill = expression(AVE - r^2))
  print(p)
  return(list("Result" = Result, "Plot" = p))
}
codeps = function(p,figure = F) {
  if (figure == F){
    p = case_when(p < .001 ~ "***",
                  p < .01 ~ "**",
                  p < .05 ~ "*",
                  p < .10 ~ "†",
                  p >= .10 ~ "")
    return(p)
  }

  if (figure == T){
    p = case_when(p < .05 ~ "Significant",
                  p < .10 ~ "Marginal",
                  p >= .10 ~ "Not Significant")
    return(p)
  }
}
numformat = function(val, n = 2) {
  sub("^(-?)0.", "\\1.", sprintf(paste0("%.", n, "f"), val))
}
formatps = function(p,n=3) {
  stars = codeps(p)
  if (p >= .001) {
    pf = paste0(numformat(p, n), stars)
    return(pf)
  }
  return("<.001***")
}
formatest = function(est,p,digits = 2){
  stars = codeps(p)
  est = numformat(est,digits)
  return(paste0(est,stars))
}
cv_bake = function (data, new, ingred.list) {
  new <- enquo(new)
  data %>% rowwise() %>% mutate(`:=`(!!quo_name(new), mean(c(!!!ingred.list),
                                                           na.rm = TRUE))) %>% ungroup()
}
generate_data = function(cors,n.obs,M=0,SD=1,names=NA){
  #Desired correlation matrix
  if(is.matrix(cors)){R = cors}
  if(is.data.frame(cors)){R = as.matrix(cors)}
  if(!is.matrix(cors)&!is.data.frame(cors)){(warning("cors must be a correlation matrix or data frame"))}

  #Choleski decomposition
  U = t(chol(R))
  # Parameters for the fake dataset
  nvars = dim(U)[1] # number of variables extracted from correlation matrix
  #Generating random normal variables
  random.normal = matrix(rnorm(nvars*n.obs,M,SD), nrow=nvars, ncol=n.obs);
  #Matrix multiplication with the choleski decomposed correlation matrix yields correlated data
  X = U %*% random.normal
  #Needs transposing and cleaning
  newX = t(X)
  raw = as.data.frame(newX)
  if(!is.na(names)){names(raw) = names}
  return(raw)
}
likertify = function(array,min=1,max=7,nitems,SD = .4,prefix = "i"){
  items <- matrix(nrow = length(array), ncol = nitems)
  for (x in 1:nrow(items)){
    items[x, ] <- rnorm(n = nitems, mean = array[x], sd = SD)
  }
  items = round(items)

  outranged = (items[items<min|items>max])
  length(outranged)
  percentout = (length(outranged))/(length(array)*nitems)

  message(paste0(percentout*100),"% of the items were out of range")

  items[items<min] = min
  items[items>max] = max

  items = as.data.frame(items)
  names(items) = paste0(prefix,1:ncol(items))
  return(items)
}



