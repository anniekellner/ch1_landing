
coefTable.flexsurvreg <- function(model, ...) {
  x <- as.data.frame(model$res[,c("est","se")])
  colnames(x) <- c("Estimate", "Std. Error")
  x
}
