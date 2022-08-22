require(tidyverse)
require(kableExtra)
require(magrittr)

#Makes figure and table captions
captionR = function (prefix = "Figure", auto_space = TRUE, levels = 1, type = NULL,
                     infix = ".",out = "html")
{
  check_class(prefix, "character")
  check_class(auto_space, "logical")
  check_class(levels, "numeric")
  check_class(infix, "character")
  if (is.null(type)) {
    type <- c(rep("n", times = levels))
  }
  else if (length(type) < levels) {
    type[(length(type) + 1):levels] <- "n"
  }
  else if (length(type) > levels) {
    type <- type[1:levels]
  }
  if (!all(type %in% c("n", "c", "C"))) {
    stop("Invalid 'type' value used.  Expecting 'n', 'c', or 'C'.")
  }
  if (auto_space) {
    prefix <- paste(prefix, " ")
  }
  force(levels)
  force(prefix)
  force(infix)
  OBJECTS <- list(name = NULL, caption = NULL, number = list(list()))
  OBJECTS$number[[1]][which(type == "n")] <- 1
  OBJECTS$number[[1]][which(type == "c")] <- "a"
  OBJECTS$number[[1]][which(type == "C")] <- "A"
  function(name, caption = "", display = "full", level = FALSE,
           cite = FALSE, num = FALSE, out = out) {
    if (level > levels) {
      stop("Level too large.")
    }
    objects <- OBJECTS
    if (any(objects$name == name)) {
      obj_ind <- match(name, objects$name)
      if (objects$caption[obj_ind] == "") {
        objects$caption[obj_ind] <- caption
      }
      else {
        caption <- objects$caption[obj_ind]
      }
    }
    else {
      obj_ind <- length(objects$name) + 1
      if (length(objects$number) == length(objects$name)) {
        if (level) {
          objects$number[[obj_ind]] <- increment(objects$number[[obj_ind -
                                                                   1]], level)
        }
        else {
          objects$number[[obj_ind]] <- increment(objects$number[[obj_ind -
                                                                   1]], levels)
        }
      }
      objects$name[obj_ind] <- name
      objects$caption[obj_ind] <- caption
    }
    assign("OBJECTS", objects, envir = parent.env(environment()))
    obj_num <- paste(objects$number[[obj_ind]], collapse = infix)
    if (cite) {
      .Deprecated(new = "display", old = "cite")
      return(paste0(prefix, obj_num))
    }
    if (num) {
      .Deprecated(new = "display", old = "num")
      return(obj_num)
    }
    if (display == FALSE) {
      return(invisible())
    }
    else if (out == "pdf" && display == "full" || display == "f") {
      return(paste0("**",prefix, obj_num,"**", "\\newline", "\\textit{",caption,"}"))
    }
    else if (out == "html" && display == "full" || display == "f") {
      return(paste0("<p class = 'caption'>**",prefix, obj_num,"**", "<br>", "*",caption,"*</p>"))
    }
    else if (display == "cite" || display == "c") {
      return(paste0("**",prefix, obj_num,"**"))
    }
    else if (display == "num" || display == "n") {
      return(obj_num)
    }
    else {
      warning("Invalid display mode used.  Caption was still saved.")
      return(invisible())
    }
  }
}


#Makes formated text with numbers into 2 decimals
decimal.two = function (x)
{
  p1 = (x %>% str_split("\\."))[[1]][1]
  p2 = (x %>% str_split("\\."))[[1]][2]
  num = as.numeric(str_extract_all(x, ".[0-9]+")[[1]])
  str = str_extract_all(x, "[*†]") %>% unlist() %>% paste(collapse = "")
  return(as.numeric(paste0(p1, Ben::numformat(num))))
}

gt_apa = function(x){x %>%
    gt::tab_options(table_body.hlines.width = 0,
                                  stub.border.width = "1px",
                                  stub.border.color = "black",
                                  column_labels.border.bottom.color = "black",
                                  column_labels.border.top.color = "black",
                                  column_labels.border.top.width = "1px",
                                  column_labels.border.bottom.width = "1px",
                                  table_body.border.bottom.color = "black",
                                  table_body.border.bottom.width = "1px",
                                  table_body.border.top.width = 0,
                data_row.padding = 0,
                table.font.names = "Times New Roman",
                row_group.border.bottom.width = 0,
                row_group.border.top.width = 0,
                row_group.padding = 4,
                table.border.top.width = 0,
                                  table.border.bottom.width =  0) %>%
    gt::tab_options(table.font.color = "black",
                table.font.size = 12) %>%
    gt::tab_style(style = cell_text(style = "italic"),
              locations = cells_row_groups())
    }

#Factor analysis
fa_tibble = function(fa,sort=T){
  if(sort){fa = psych::fa.sort(fa)}
  loadings = fa$loadings %>% as.numeric() %>%
    matrix(ncol = fa$factors) %>% as_tibble %>%
    mutate(Var = fa$model %>% colnames()) %>%
    select(Item = Var,everything())
  return(loadings)


}
gt_fatable = function(x,sort = T,cut = .3,apa = T,eigenvalues = T,cor = F) {
  if (is_tibble(x)) {
    items = nrow(x)
    gt = x %>%
      gt::gt() %>%
      gt::fmt_number(columns = 2:ncol(x)) %>%
      gt::data_color(
        columns = 2:ncol(x),
        apply_to = "text",
        colors = scales::col_bin(
          bins = c(-1, cut * -1, cut, 1),
          palette = c("black", "gray", "black"),
          domain = NULL
        )
      )

  } else {
    fatib = x %>% Ben::fa_tibble(sort)
    items = nrow(fatib)
    ve = x$Vaccounted
    ve= ve[2,]
    ve = c("Variance Explained",ve,"Summary statistics")
    eigen = x$e.values[1:ncol(fatib)-1]
    eigen = c("Eigenvalues",eigen,"Summary statistics")

    if(eigenvalues) {
      fatib = fatib %>%
        mutate(g = " ") %>%
        rbind(eigen,ve) %>%
        mutate_at(2:(x$factors+1),as.numeric) %>%
        group_by(g)

      gt = fatib %>%
        gt::gt() %>%
        gt::fmt_number(columns = 2:ncol(fatib),rows = 1:items) %>%
        gt::data_color(
          columns = 2:ncol(fatib),
          apply_to = "text",
          colors = scales::col_bin(
            bins = c(-1, cut * -1, cut, 1),
            palette = c("black", "gray", "black"),
            domain = NULL
          )) %>%
        tab_style(style = cell_text(color = "black"),locations = cells_body(rows = (nrow(fatib)-1):nrow(fatib))) %>%
        fmt_number(columns = 2:ncol(fatib),rows = items+1) %>%
        fmt_percent(columns = 2:ncol(fatib),rows = items + 2,decimals = 1)
    } else {gt  = gt(fatib)}

    if(cor) {
      cors = x$Phi
      cors[upper.tri(cors,diag = T)] = NA
      cors = cors %>% as.data.frame()
      colnames(cors) = colnames(fatib)[2:(x$factors+1)]
      cors = cors %>% rownames_to_column("Item") %>% mutate(g = "Correlations")
      cors$Item = colnames(fatib)[2:(x$factors+1)]
      fatib$g[1:items] = " "
      fatib = rbind(fatib,cors) %>% group_by(g)

      gt = fatib %>%
        gt::gt() %>%
        gt::fmt_number(columns = 2:(1+x$factors),rows = 1:items) %>%
        gt::data_color(
          columns = 2:ncol(fatib),
          apply_to = "text",
          colors = scales::col_bin(
            bins = c(-1, cut * -1, cut, 1),
            palette = c("black", "gray", "black"),
            domain = NULL
          )) %>%
        tab_style(style = cell_text(color = "black"),locations = cells_body(rows = (items+1):nrow(fatib))) %>%
        fmt_missing(columns = 1:ncol(fatib),missing_text = " ")
      if(eigenvalues){
        gt =  gt %>%
          fmt_number(columns = 2:ncol(fatib),rows = items+1) %>%
          fmt_percent(columns = 2:ncol(fatib),rows = items + 2,decimals = 1)
      } else {
        gt = gt %>%
          fmt_number(columns = 2:ncol(fatib),rows = (nrow(fatib)+1-x$factors):nrow(fatib))
      }



    } else { gt  = gt::gt(fatib)}



  }




  if (apa) {
    gt = gt %>% Ben::gt_apa() %>% gt::fmt(
      columns = 2:ncol(fatib),
      rows = 1:items,
      fns = Ben::numformat
    )
    if (cor) {
      gt = gt %>%
        gt::fmt(
          columns = 2:ncol(fatib),
          rows = (nrow(fatib) - x$factors + 1):nrow(fatib),
          fns = Ben::numformat
        ) %>%
        gt::fmt_missing(columns = 1:ncol(fatib),missing_text = "")
    }
    return(gt)
  }
  else{
    return(gt)
  }
}

theme_ang = theme(legend.position = "bottom",
                       panel.grid = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank(),
                       axis.line = element_line(size = .25))



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
plotmed = function(Model,med=NULL){
  data=Model %>% parameterestimates() %>% select(lhs,op,rhs,label,p = pvalue)%>% left_join(
    Model %>% standardizedSolution() %>% select(lhs,op,rhs,est.std)
  ) %>%
    filter(label != "")
  data = data %>% mutate(est.std = Ben::formatest(est.std,p))

  if(is.null(med)){
    med = data %>% filter(label == "a") %>% pull(lhs)
  }


  a = data %>% filter(label == "a") %>% pull(est.std)
  b = data %>% filter(label == "b") %>% pull(est.std)
  c = data %>% filter(label == "c") %>% pull(est.std)
  ie = data %>% filter(label == "ie") %>% pull(est.std)
  te = data %>% filter(label == "te") %>% pull(est.std)
  cf = paste0(te," (",c,")")

  DiagrammeR::grViz(
    paste0("digraph {
    // Base Styling
    nodesep='1';

  'Preview' [shape = 'rectangle',fontname = 'Helvetica',width = 1.5]
  '",med,"' [shape = 'rectangle',fontname = 'Helvetica'width = 1.5]
  'Post-Test' [shape = 'rectangle',fontname = 'Helvetica'width = 1.5]


  'Indirect Effect = ",ie,"'[shape = 'plaintext',fontname = 'Helvetica',fontsize  = 12]

  'Preview'->'",med,"' [style = 'solid', label = '",a,"',fontname = 'Helvetica',fontsize  = 12]
  '",med,"'->'Post-Test' [style = 'solid', label = '",b,"',fontname = 'Helvetica',fontsize  = 12]
  'Preview'->'Post-Test' [style = 'solid', label = '",cf,"',fontname = 'Helvetica',fontsize  = 12]

  Preview ->  'Indirect Effect = ",ie,"' [style = 'invis']

           { rank = max; 'Indirect Effect = ",ie,"' }
           { rank = min; '",med,"' }
           { rank = same; 'Preview'; 'Post-Test' }
   }
"))
}

