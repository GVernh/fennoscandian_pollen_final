custom_commonality_analysis <- function(LCC, SPD, clim, p) {
  #commonality analysis
  varnames <- c(deparse(substitute(LCCadapt)), deparse(substitute(SPD)), deparse(substitute(clim)))
  lagnames <- paste0(varnames, "_lag", rep(1:p, each = 3))
  # Then we created the lagged variables / data for models
  VAR_data <- stats::embed(as.matrix(cbind(LCC, SPD, clim)), p + 1)
  colnames(VAR_data) <- c(varnames, lagnames)
  VAR_data <- VAR_data[, -c(2,3)]
  # the full model with LCC, SPD and clim
  model_formula <- paste(colnames(VAR_data)[-1], collapse = " + ")
  model_formula <- stats::formula(paste0(varnames[1], " ~ ", model_formula))
  full_mod <- stats::lm(model_formula, data = as.data.frame(VAR_data))
  # LCC and clim
  VAR_data_c <- dplyr::select(as.data.frame(VAR_data),contains(c("LCCadapt", "clim")))
  model_formula <- paste(colnames(VAR_data_c)[-1], collapse = " + ")
  model_formula <- stats::formula(paste0(varnames[1], " ~ ", model_formula))
  clim_mod <- stats::lm(model_formula, data = as.data.frame(VAR_data_c))
  # LCC and SPD
  VAR_data_s <- dplyr::select(as.data.frame(VAR_data),contains(c("LCCadapt", "SPD")))
  model_formula <- paste(colnames(VAR_data_s)[-1], collapse = " + ")
  model_formula <- stats::formula(paste0(varnames[1], " ~ ", model_formula))
  SPD_mod <- lm(model_formula, data = as.data.frame(VAR_data_s))
  # only LCC
  VAR_data_l <- dplyr::select(as.data.frame(VAR_data),contains("LCCadapt"))
  model_formula <- paste(colnames(VAR_data_l)[-1], collapse = " + ")
  model_formula <- stats::formula(paste0(varnames[1], " ~ ", model_formula))
  LCC_mod <- stats::lm(model_formula, data = as.data.frame(VAR_data_l))
  # Granger test
  granger_test_f <- stats::anova(LCC_mod, full_mod)
  granger_test_s <- stats::anova(LCC_mod, SPD_mod)
  granger_test_c <- stats::anova(LCC_mod, clim_mod)
  # getting R squared values
  R2_lsc = (granger_test_f$RSS[1] - granger_test_f$RSS[2]) / granger_test_f$RSS[1]
  R2_ls = (granger_test_s$RSS[1] - granger_test_s$RSS[2]) / granger_test_s$RSS[1]
  R2_lc = (granger_test_c$RSS[1] - granger_test_c$RSS[2]) / granger_test_c$RSS[1]
  # commonality analysis
  Us <- R2_lsc - R2_lc
  Uc <- R2_lsc - R2_ls
  Csc <- R2_lsc - Us - Uc
  # Return the results
  return(list(R2_lsc =R2_lsc,
              R2_ls =R2_ls,
              R2_lc =R2_lc,
              Us=Us,
              Uc=Uc,
              Csc=Csc))
}
