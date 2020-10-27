# R Code for resid.plotter that plots the residuals from a model object.
library(tidyverse); library(patchwork)
resid.plotter <- function(model) {
  r1 <- residuals(model)
  fv = fitted.values(model)
  sv <- paste0("Shapiro p-value: ",round(shapiro.test(r1)$p.value, 5))
  rcall <- deparse(model$call)
  data <- data.frame(Residuals = r1, Fitted.Values = fv) %>% mutate(y = Residuals + Fitted.Values)
  plot1 <-  data %>% ggplot() + aes(x=Residuals) + geom_density()
  plot2 <-  data %>% ggplot() + aes(x=Fitted.Values, y=y) + geom_point() + geom_abline(intercept=0, slope=1, color="blue")
  plot3 <-  data %>% ggplot() + aes(x=Fitted.Values, y=Residuals) + geom_point()
  plot4 <-  data %>% ggplot() + aes(sample=Residuals) + stat_qq() + stat_qq_line() + labs(title = sv)
  retplot <- (plot1 + plot2) / (plot4 + plot3)
  retplot <- retplot + plot_annotation(title=rcall)
 return(retplot)
}
# End of R Code
