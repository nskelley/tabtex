#' Format regression estimates in a data frame
#'
#' @param ... Regression models (the results of `lm()`) to be made into a data frame.
#' @param main A character string indicating the main estimate to be shown in the data frame. By default, the point estimate. See `aux` for alternative estimates (`"ci"` cannot be used as the main estimate).
#' @param aux A character string indicating the secondary estimate to be shown in the row beneath the main estimate. By default, the standard error of the coefficient (`"se"`). Use `"t"` for the t-statistics, `"p"` for the p-value, or `"ci"` for the confidence interval instead of `"se"` if desired.
#' @param add_stats A character vector indicating model-level statistics to add to the end (last rows) of the table. Possible values are `"n"` (number of observations), `"df"` (degrees of freedom), `"rsq"` (R-squared), `"arsq"` (adjusted R-squared), `"rss"` (residual sum of squares), `"mse"` (mean squared error), and `"f"` (F-statistic). Name the values (e.g., `c("Degrees of freedom" = "df")`) to change the row's label (leftmost cell).
#' @param conf_level When `aux = "ci"`, a number in the range (0, 100) indicating the confidence level to be used in calculating the confidence interval. By default 95 (for a 95% confidence interval).
#'
#' @return A data frame with the specified regression summary estimates.
#' @export

modeltab <- function(...,
                     main = "b",
                     aux = "se",
                     add_stats = c("n", "df", "rsq"),
                     conf_level = 95) {
  all_models <- list(...)
  if (class(all_models[[1]]) == "list") {
    warning("List passed to function where models should be. Using first element of list.")
    all_models <- all_models[[1]]
  } else if (class(all_models[[1]]) %in% c("character", "numeric")) {
    warning("Vector passed to function where models should be. Using first element of vector.")
    all_models <- as.list(all_models[[1]])
  }
  
  # Identify all rows at the start/before getting to individual models
  vars <- c()
  for (m in all_models) {
    new_vars <- base::setdiff(names(m$coefficients), vars)
    vars <- c(vars, new_vars)
  }
  
  if (!is.null(aux) & !is.na(aux)) {
    rows <- lapply(vars, function(x) return(c(x, paste0(aux, "_", x)))) |>
      unlist()
  } else {
    rows <- vars
  }
  
  if (!is.null(names(add_stats))) {
    rows <- c(rows, names(add_stats))
  } else {
    rows <- c(rows, add_stats)
  }
  
  # Create a blank full table
  full_table <- data.frame(var = rows)
  
  # Add each model to the table
  for (model_num in 1:length(all_models)) {
    model <- all_models[[model_num]]
    
    # --------------------------------------------------------------------------
    # Model-level estimates (for add_stats)
    
    # Get number of observations
    nobs <- nrow(model$model)
    # Get degrees of freedom
    df <- model$df.residual
    # Get R-squared
    rsq <- summary(model)$r.squared
    # Get adjusted R-squared
    adj_rsq <- summary(model)$adj.r.squared
    # Get residual sum of squares (RSS)
    rss <- sum(model$residuals^2)
    # Get mean squared error (MSE)
    mse <- mean(model$residuals^2)
    # Get F-statistic
    f_stat <- summary(model)$fstatistic[["value"]]
    
    # Merge model-level estimates into a single named vector
    model_level_est <- c("n" = nobs,
                         "df" = df,
                         "rsq" = rsq,
                         "arsq" = adj_rsq,
                         "rss" = rss,
                         "mse" = mse,
                         "f" = f_stat)
    rm(nobs, df, rsq, adj_rsq, rss, mse, f_stat)
    
    
    # --------------------------------------------------------------------------
    # Covariate-level estimates (for main and aux)
    
    vars <- c("(Intercept)", "lifeExp", "gdpPercap") # tk delete this line
    
    # Point estimates
    b_est <- data.frame(var = names(model$coefficients),
                        b_est = unname(model$coefficients))
    # SE estimates
    se_raw <- summary(model)$coefficients[, "Std. Error"]
    se_est <- data.frame(var = names(se_raw), se_est = unname(se_raw))
    # t-statistic estimates
    t_raw <- summary(model)$coefficients[, "t value"]
    t_est <- data.frame(var = names(t_raw), t_est = unname(t_raw))
    # p-value estimates
    p_raw <- summary(model)$coefficients[, "Pr(>|t|)"]
    p_est <- data.frame(var = names(p_raw), p_est = unname(p_raw))
    # CI estimates
    if (aux == "ci" & conf_level >= 100 | conf_level <= 0) {
      stop("Invalid confidence level. Use a number in (0, 100).")
    }
    ci_mult <- qnorm((1 - conf_level / 100) / 2)
    ci_est <- merge(b_est, se_est, by = "var")
    ci_est$ci_lower <- ci_est$b_est - (ci_mult * ci_est$se_est)
    ci_est$ci_upper <- ci_est$b_est + (ci_mult * ci_est$se_est)
    ci_est[, c("b_est", "se_est")] <- NULL
    
    # Merge the covariate-level estimates into one data frame
    model_est <- Reduce(function(x, y) merge(x, y, by = "var"), 
                        list(b_est, se_est, t_est, p_est, ci_est))
    rm(b_est, se_est, t_est, p_est, ci_est)
    
    
    # --------------------------------------------------------------------------
    # Construct the model-specific data frame
    model_col <- data.frame(var = rows)
    
    # Isolate the main and aux estimates
    main_est <- model_est[c("var", paste0(main, "_est"))]
    names(main_est) <- replace(names(main_est), 
                               which(names(main_est) == paste0(main, "_est")), 
                               "est")
    
    if (aux == "ci") {
      # Covariate-level aux estimates
      main_est$est.1 <- NA
      aux_est <- model_est[c("var", "ci_lower", "ci_upper")]
      names(aux_est)[names(aux_est) == "ci_lower"] <- "est"
      names(aux_est)[names(aux_est) == "ci_upper"] <- "est.1"
      
      # Model-level statistics
      model_level_merge <- data.frame(est = model_level_est, est.1 = NA)
      model_level_merge$var <- names(model_level_est)
      rownames(model_level_merge) <- NULL
      
    } else {
      # Covariate-level aux estimate
      aux_est <- model_est[c("var", paste0(aux, "_est"))]
      names(aux_est) <- replace(names(aux_est), 
                                which(names(aux_est) == paste0(aux, "_est")), 
                                "est")
      
      # Model-level statistics
      model_level_merge <- data.frame(est = model_level_est)
      model_level_merge$var <- names(model_level_est)
      rownames(model_level_merge) <- NULL
    }
    
    # Combine main and aux estimates for merge
    aux_est$var <- paste0(aux, "_", aux_est$var)
    merge_est <- rbind(main_est, aux_est)
    
    
    model_col <- merge(model_col, rbind(merge_est, model_level_merge), 
                       by = "var", all.x = TRUE)
    
    names(model_col)[names(model_col) == "est"] <- paste0("est", model_num)
    names(model_col)[names(model_col) == "est.1"] <- 
      paste0("est", model_num, "_")
    
    full_table <- merge(full_table, model_col, by = "var", all.x = TRUE)
  }
  
  full_table$var <- factor(full_table$var,
                           levels = rows)
  full_table_ordered <- full_table[order(full_table$var), ]
  return(full_table_ordered)
}
