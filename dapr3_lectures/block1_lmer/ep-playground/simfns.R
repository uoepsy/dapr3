#'  Quickly sim data for a basic model y~1+x+(1|g)
#' @param seed random seed to use
#' @param b0 fixed intercept
#' @param b1 fixed slope
#' @param sigma2_u0 random intercept variance
#' @param sigma residual sd
#' @param N how many obs
#' @param n_groups how many groups
#' @param eq equality of x between groups (if true, categ x1, else zscore)
sim_int_1grp_1pred <- function(
    seed=NULL,
    b0=0, b1=1,
    sigma2_u0=1,
    sigma=1,
    N=200, n_groups=20,
    eq=TRUE
){
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  g = rep(1:n_groups, e = N/n_groups)
  
  if(eq){
    x1 = rep(1:(N/n_groups), n_groups)
  } else {
    x1 = rnorm(N)
  }
  
  # intercept adjustments
  u0 = rnorm(n_groups, mean = 0, sd = sqrt(sigma2_u0))[g]
  
  lp = (b0 + u0) + (b1 * x1)
  
  y = rnorm(N, mean = lp, sd = sigma)             # continuous target
  y_bin = rbinom(N, size = 1, prob = plogis(lp))  # binary target
  
  data.frame(x1, g=factor(g), y, y_bin)
}



sim_intslp_1grp_1pred <- function(
    seed=NULL,
    b0=0, b1=1,
    sigma2_u0=1, sigma2_u1=1,
    rho=0,
    sigma=1,
    N=200, n_groups=20,
    eq=TRUE
){
  #'  Quickly sim data for a basic model y~1+x+(1+x|g)
  #' @param seed random seed to use
  #' @param b0 fixed intercept
  #' @param b1 fixed slope
  #' @param sigma2_u0 random intercept variance
  #' @param sigma2_u1 random slope var
  #' @param rho int-slope correl
  #' @param sigma residual SD
  #' @param eq equality of x between groups (if true, categ x1, else zscore)
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  g = rep(1:n_groups, e = N/n_groups)
  
  if(eq){
    x1 = rep(1:(N/n_groups), n_groups)
  } else {
    x1 = rnorm(N)
  }
  
  Gcov = Matrix::nearPD(matrix(c(
    sigma2_u0,
    rho*sigma2_u0*sigma2_u1,
    rho*sigma2_u0*sigma2_u1,
    sigma2_u1
  ),nrow=2))$mat
  
  res = MASS::mvrnorm(n_groups, mu=c(0,0), Sigma=Gcov)
  u0  = res[,1][g]  # random intercepts
  u1  = res[,2][g]  # random slopes
  
  lp = (b0 + u0) + (b1 + u1) * x1
  
  y = rnorm(N, mean = lp, sd = sigma)             # continuous target
  y_bin = rbinom(N, size = 1, prob = plogis(lp))  # binary target
  
  data.frame(x1, g=factor(g), y, y_bin)
}



simcross <- function(seed=NULL,b0=0,b1=1,b2=1,b3=2,n_ppts=6,n_itms=4,n_obs_per=4){
  #' Quickly sim data for a big crossed ranef model 
  #' y ~ 1 + a + b + c + (1 + a + b + c | ppt) + (1 + a + c | itm)
  #' @param seed random seed to use
  #' @param b0 fixed intercept
  #' @param b1 fixed slope, binary within ppts, within items
  #' @param b2 fixed slope, binary within ppts, btwn items
  #' @param b3 fixed slope, num (or bin) within ppts, within items
  #' @param n_ppts how many ppts (or groups of one kind); must div by 2
  #' @param n_itms how many itms (or groups of a diff kind); must div by 2
  #' @param n_obs_per how many obs for each ppt/itm combo; must div by 2
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  N = n_ppts*n_itms
  
  # init design matrix with all combinations of ppts and items
  ds_mtx = expand.grid(ppts = 1:n_ppts, itms = 1:n_itms)
  
  # set up binary predictor that varies within ppts, btwn items
  # (eg, experimental condition?)
  ds_mtx$wi_ppts_btw_itms = 0
  ds_mtx$wi_ppts_btw_itms[1:(N/2)] = 1
  
  # set up binary predictor that varies within ppts AND within items
  # (eg, sentence version?)
  ds_mtx$wi_itms_wi_ppts = c(
    rep(rep(0:1,e=(n_ppts)/2), n_itms/2),
    rep(rep(1:0,e=(n_ppts/2)), n_itms/2)
  )
  
  # if >1 obs per ppt/item combo, set up num predictor 
  # which varies within ppts AND within items
  ds_mtx$x = list(1:n_obs_per)
  ds_mtx = tidyr::unnest(ds_mtx,x)
  # this new pred can map to another wi/wi binary categ pred if desired
  ds_mtx$xcat = ifelse(ds_mtx$x > (n_obs_per/2), "b", "a")
  
  # define by-ppt int + slope vcov mtx (assume no correls)
  # slopes that need adjusted:
  # - wi_itms_wi_ppts – b1
  # - wi_ppts_btw_itms – b2
  # - x or xcat – b3
  n_by_ppt_params <- 4
  ppts_vcov <- matrix(0, nrow = n_by_ppt_params, ncol = n_by_ppt_params)
  diag(ppts_vcov) <- 1
  ppts_vcov <- Matrix::nearPD(ppts_vcov)$mat
  
  # sample by-ppt adjustments and store as vectors, ready to add on
  ppts_res <- MASS::mvrnorm(n_ppts, mu=rep(0, n_by_ppt_params), Sigma=ppts_vcov)
  ppts_int <- ppts_res[,1][ds_mtx$ppts]
  ppts_slp_b1 <- ppts_res[,2][ds_mtx$ppts]
  ppts_slp_b2 <- ppts_res[,3][ds_mtx$ppts]
  ppts_slp_b3 <- ppts_res[,4][ds_mtx$ppts]
  
  
  # define by-item int + slope vcov mtx (assume no correls)
  # slopes that need adjusted:
  # - wi_itms_wi_ppts – b1
  # - x or xcat – b3
  n_by_itm_params <- 3
  itms_vcov <- matrix(0, nrow = n_by_itm_params, ncol = n_by_itm_params)
  diag(itms_vcov) <- 1
  itms_vcov <- Matrix::nearPD(itms_vcov)$mat
  
  # sample by-item adjustments and store as vectors, ready to add on
  itms_res = MASS::mvrnorm(n_itms, mu=rep(0, n_by_itm_params), Sigma=itms_vcov)
  itms_int <- itms_res[,1][ds_mtx$itms]
  itms_slp_b1 <- itms_res[,2][ds_mtx$itms]
  itms_slp_b3 <- itms_res[,3][ds_mtx$itms]
  
  lp = (b0 + ppts_int + itms_int) + 
    (b1 + ppts_slp_b1 + itms_slp_b1) * ds_mtx$wi_itms_wi_ppts +
    (b2 + ppts_slp_b2) * ds_mtx$wi_ppts_btw_itms + 
    (b3 + ppts_slp_b3 + itms_slp_b3) * ds_mtx$x
  
  y = rnorm(
    N*n_obs_per, 
    mean = lp, 
    sd = 1
  ) 
  
  data.frame(ds_mtx, y)
}

# simcross(n_obs_per = 2)
# # ^ if want other btwn effects, then can just filter some combinations out!
  
  
# # code from Jo for nested data
#   
# n_highergroup <- 10
# N <- 200
# 
# df <- do.call(rbind,
#               lapply(rnorm(n_highergroup), \(x) sim_intslp_1grp_1pred(b0=x, N = N))
# )
# df$highergroup = rep(1:n_highergroup,e=N)
# 
# lme4::lmer(y~1+x1+(1|highergroup)+(1+x1|highergroup:g),df)


# # code from Jo for sim between data design matrix:
#   
# # how many participants am i getting, and how do they vary?
# ppts = 1:6
# # observed btween ppt predictor
# btw_ppt = rbinom(6,1,.5)
# 
# # assuming everybody does the same expt, this can be seen as 
# # "for a given ppt, what do they see?":  
# items = 1:4
# btw_items = rep(0:1,e=max(items)/2)
# 
# # all my ppts: 
# allp = data.frame(ppts,btw_ppt)
# # nested df for each ppts data: 
# allp$pdat = list(data.frame(items,btw_items))
# 
# # unnest: 
# tidyr::unnest(allp, pdat)