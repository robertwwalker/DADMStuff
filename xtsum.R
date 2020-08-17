library(tidyverse)
library(plm)
xtsum <- function(formula, data) {
  pform <- terms(formula, data=data)
  unit <- pform[[2]]
  vars <- attr(pform, "term.labels")
  cls <- sapply(data, class)
  data <- data %>% select(which(cls%in%c("numeric","integer")))
  varnames <- intersect(names(data),vars)
  sumfunc <- function(data=data, varname, unit) {
    loc.unit <- enquo(unit)
    varname <- ensym(varname)
    ores <- data %>% filter(!is.na(!! varname)==TRUE) %>% summarise(
      O.mean=round(mean(`$`(data, !! varname), na.rm=TRUE), digits=3),
      O.sd=round(sd(`$`(data, !! varname), na.rm=TRUE), digits=3), 
      O.min = min(`$`(data, !! varname), na.rm=TRUE), 
      O.max=max(`$`(data, !! varname), na.rm=TRUE), 
      O.SumSQ=round(sum(scale(`$`(data, !! varname), center=TRUE, scale=FALSE)^2, na.rm=TRUE), digits=3), 
      O.N=sum(as.numeric((!is.na(`$`(data, !! varname))))))
    bmeans <- data %>% filter(!is.na(!! varname)==TRUE) %>% group_by(!! loc.unit) %>% summarise(
      meanx=mean(`$`(.data, !! varname), na.rm=T), 
      t.count=sum(as.numeric(!is.na(`$`(.data, !! varname)))))
    bres <- bmeans %>% ungroup() %>% summarise(
      B.mean = round(mean(meanx, na.rm=TRUE), digits=3),
      B.sd = round(sd(meanx, na.rm=TRUE), digits=3),
      B.min = min(meanx, na.rm=TRUE), 
      B.max=max(meanx, na.rm=TRUE), 
      B.Units=sum(as.numeric(!is.na(t.count))), 
      B.t.bar=round(mean(t.count, na.rm=TRUE), digits=3))
    wdat <- data %>% filter(!is.na(!! varname)==TRUE) %>% group_by(!! loc.unit) %>% mutate(
      W.x = scale(`$`(.data,!! varname), scale=FALSE))
    wres <- wdat %>% ungroup() %>% summarise(
      W.sd=round(sd(W.x, na.rm=TRUE), digits=3), 
      W.min=min(W.x, na.rm=TRUE), 
      W.max=max(W.x, na.rm=TRUE), 
      W.SumSQ=round(sum(W.x^2, na.rm=TRUE), digits=3))
    W.Ratio <- round(wres$W.SumSQ/ores$O.SumSQ, digits=3)
    return(c(ores,bres,wres,Within.Ovr.Ratio=W.Ratio))
  }
  res1 <- sapply(varnames, function(x) {sumfunc(data, !!x, !!unit)})
  return(data.frame(t(res1)))
}
