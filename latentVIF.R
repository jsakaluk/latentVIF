latentVIF <- function(fit, x1name, x2name){
  corr <- lavaan::parameterestimates(fit) %>%
    dplyr::filter(op == "~~") %>%
    dplyr::filter(lhs == x1name | lhs == x2name) %>%
    dplyr::filter(rhs == x1name | rhs == x2name) %>%
    dplyr::filter(rhs != lhs) %>%
    dplyr::select(est)

  vars <-  lavaan::parameterestimates(fit) %>%
    dplyr::filter(op == "~~") %>%
    dplyr::filter(lhs == x1name | lhs == x2name) %>%
    dplyr::filter(rhs == x1name | rhs == x2name) %>%
    dplyr::filter(rhs == lhs) %>%
    dplyr::select(est)

  vif <- list("vif" = (1/(1-corr$est)))

  #Is sum of two latent variances 2 (i.e., 1 + 1)?
  #If not, variables are not standardized and corr is a cov
  #Note: this quick/dirty check will be vulnerable to cases
  #where variances add to 2 but are not both one (e.g., 1.5, .5)
  varsum <- sum(vars$est)
  if(varsum!= 2){
    warning("Your latent variables do not appear to be standardized; VIF calculation cannot be trusted because it has been estimated with a latent covariance, not a latent correlation")
  }
  return(vif)
}
