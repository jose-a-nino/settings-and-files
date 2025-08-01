# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper functions to create models and plots  ---------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print("helper-functions.R")

#' Create fixed effects linear model
#'
#' This function creates a simpler way to create fixed effects linear model
#' with the dependent variable as consumption by allowing variable
#' amount of inputs for covariates (via the vector \code{indep_vars}) and for the
#' coefficient of interest (fixed effects are chosen already.)
#'  \code{studentloan_upb_pc} (student loans per capita).
#' @param data Dataframe with student loan and consumption data and other relevant
#' relevant demographic variables.
#' @param dep_var The dependent variable to regress consumption on. Defaults to "trans_amt_total_pc_diff".
#' @param indep_vars A character vector of inputs you want to regress consumption
#' per capita against.
#' @param weights The column name in \code{data} to weight the regression by. Defaults to "total_adult".
#' @param fe_vars A character vector for user-defined fixed effects. If NULL, default is \code{zipcode + state:date}.
#' @param cluster_vars A character vector for user-defined clustered standard errors If NULL, default is \code{zipcode}.

#' @return Returns an object of class \code{felm}.
#' @export
create_felm_model <- function(data,
                              dep_var = NULL,
                              indep_vars, 
                              weights = NULL,
                              fe_vars = NULL,
							  cluster_vars = NULL) {

  # Default for `dep_var`
  if(is.null(dep_var)) dep_var <- "trans_amt_total_pc_diff" 
  
  # Create string of formula name from character vector input
  form_base <- glue("{dep_var} ~")
  form_indep_vars <- paste(indep_vars, collapse = " + ")
  
  # Set default for fixed effects
  form_fe_vars <- ifelse(is.null(fe_vars), 
                         paste(c("zipcode", "state:date"), collapse = " + "), 
                         paste(fe_vars, collapse = " + "))
  
  # Set default for clustered standard errors
  form_cluster_vars <- if_else(is.null(cluster_vars),
							 # Default to no clustering
							   "0",
							   paste(cluster_vars, collapse = " + "))

  formula <- as.formula(paste(form_base, 
  							  "0 +", 
							  form_indep_vars, 
							  "|", 
							  form_fe_vars, 
							  "| 0 |",
							  form_cluster_vars))
  print(formula)
  # Set default for weightss
  if (is.null(weights)) weights <- "total_adult"
  
  # Create regression model
  model <- felm(formula, data = data, weights = data[[weights]])
  model$call <- bquote(felm(.(formula), data = .(substitute(data)), weights = .(substitute(data[[weights]])), cmethod = "reghdfe"))  
  return(model)
}

#' Create centered coefficients for event-study analysis
#'
#' This function computes the centered coefficients for the event-study regression,
#' adjusting the coefficients to remove pre-treatment trends.
#' @param model The fitted felm model object.
#' @param covar The variable of interest, typically "studentloan_upb_pc".
#' @param start_date The start date of the event study, defaults to the first date in the sample.
#' @param treatment_date The date of the treatment shock.
#' @return A list containing the data of new estimates and the variance-covariance matrix.
#' @export
create_centered_coeffs <- function(model, 
                                   covar = NULL,
                                   start_date = NULL,
                                   treatment_date) {
  
  # Set default coefficient of interest
  if(is.null(covar)) covar <- "studentloan_upb_pc"
  # Create a data.table of information from regression model
  model_summary <- setDT(as.data.frame(summary(model)$coefficients), 
                         keep.rownames = "coeff_name")
  # Add date column
  model_summary[, date := ymd(gsub(".*date(\\d{4}-\\d{2}-\\d{2}).*", "\\1", coeff_name))]
  # Set default start date of re-centering to be starting date of sample
  start_date <- if(is.null(start_date)) min(model_summary$date, na.rm = T) else start_date
  # Only keep terms where there is exactly one colon — i.e., true 2-way interactions
  model_summary[, colon_count := str_count(coeff_name, ":")]

  # Define strict pattern for 2-way interactions with covar and date
  pattern <- glue("^{covar}:date\\d{{4}}-\\d{{2}}-\\d{{2}}$|^date\\d{{4}}-\\d{{2}}-\\d{{2}}:{covar}$")

  # Filter main interaction terms (not triples)
  main_effects <- model_summary[
    grepl(pattern, coeff_name) & colon_count == 1 & date >= as.Date(start_date)
  ]

  if (nrow(main_effects) == 0) {
    stop("No matching main interaction terms found. Check covar and data format.")
  }

  start <- which(model_summary$coeff_name == main_effects$coeff_name[1])
  end <- which(model_summary$coeff_name == main_effects$coeff_name[nrow(main_effects)])

  coeffs_data <- model_summary[start:end]
  betas <- coeffs_data$Estimate
  n_beta <- length(betas)
  
  if (identical(vcov(model, type = "cluster"), vcov(model))) {
    vcov_model  <-  vcov(model, type = "cluster")
	} else {
	vcov_model  <-  vcov(model, type = "robust")
  }
  omega <- matrix(stats::vcov(model)[start:end, start:end], nrow = n_beta)

  # Count pre-treatment periods
  n_pre_period <- coeffs_data[date < as.Date(treatment_date) & date >= as.Date(start_date), .N]

  # Transformation matrix
  L <- matrix(0, nrow = n_beta + 1, ncol = n_beta)
  
  # Subtract the mean from each coefficient
  for (i in 1:n_pre_period){
    L[, i] <- L[, i] - (1 / (n_pre_period + 1))
  }
  for (i in 1:n_beta){
    L[i + 1, i] <- L[i + 1, i] + 1
  }
  # Calculate the new beta values but centered at the mean of the coefficients
  betas_new <- L %*% betas
  # Calculate new covariance matrix and new standard errors
  omega_new <- L %*% omega %*% t(L)
  se_new <- 1.96*(diag(omega_new)^0.5)
  
  # Create a dataset of new coefficients and SEs by date
  new_estimate_data <- data.frame(
    date = c(min(model_summary$date, na.rm = T), coeffs_data$date),
    estimate = betas_new,
    se = se_new
  )
  
  setDT(new_estimate_data)
  
  return(list("data" = new_estimate_data, "vcv" = omega_new))
}

#' Create winsorized dataset by date
#' 
#' This function will winsorize a particular variable in a dataset to a specified 
#' level. Note that this is a two-sided, symmetric process so a 95% winsorize level 
#' will set anything above the 97.5% quantile to 97.5% and anything below the 2.5%
#' quantile to the 2.5% quantile. 
#'
#' @param data. Data frame object or object which can be modified into a data frame.
#' @param winsorize_var. Variable to winsorize. 
#' @param winsorize_val . Level to winsorize the parameter passed from 
#' \code{winsorize_var} with a default set to a 95% winsorization. 
#' 
#'
#' @return Returns a data frame or general data set object. 
#' @export
#'
#' @examples
create_winsorized_data <- function(data, winsorize_var, winsorize_val = 0.995) {

  winsorize_var <- rlang::parse_expr(winsorize_var)
  
  winsorized_data <- data %>% 
    group_by(date) %>% 
    mutate(!!winsorize_var := winsorize(!!winsorize_var)) %>%
    ungroup()
  
  
  return(winsorized_data)
}

plot_theme <- function(show_x_title = TRUE) {
    theme(
      # axis.title = element_text(face = "bold"),
      text = element_text(size = 16),
      axis.title.x = if (show_x_title) element_text(face = "bold") else element_blank(),
      axis.title.y = element_text(face = "bold"),
      axis.text = element_text(colour = "black", size = 20),
      panel.grid.major = element_line(colour = "white"),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      axis.ticks.length = unit(.25, "cm"),
      axis.minor.ticks.length = unit(.25 / 2, "cm"),
      plot.caption = element_text(hjust = 0),
      panel.background = element_rect(fill = "white")      
    ) 
}

#' Calculate Mean and Standard Error of Aggregated Coefficients
#'
#' This function computes the mean effect and its associated standard error
#' for a set of regression coefficients over a specified date range. It is 
#' typically used after extracting and centering event-study coefficients 
#' (e.g., via \code{\link{create_centered_coeffs}}).
#'
#' @param coeffs_data A data.table or data.frame containing coefficient estimates
#'   with a column named \code{date} and \code{estimate}.
#' @param vcv A variance-covariance matrix corresponding to the coefficients 
#'   in \code{coeffs_data}.
#' @param start_date A character string (YYYY-MM-DD) specifying the first 
#'   date to include in the aggregation.
#' @param end_date A character string (YYYY-MM-DD) specifying the last 
#'   date to include in the aggregation.
#'
#' @details 
#' The mean effect \eqn{\bar{\beta}} is calculated as:
#' \deqn{
#' \bar{\beta} = \frac{1}{n} \sum_{t=1}^{n} \hat{\beta}_t
#' }
#' 
#' The standard error of this aggregated mean is:
#' \deqn{
#' SE(\bar{\beta}) = \sqrt{\frac{1}{n^2} \mathbf{1}^\top \Omega \mathbf{1}}
#' }
#' where:
#' \itemize{
#'   \item \eqn{n} is the number of coefficients between 
#'         \code{start_date} and \code{end_date}.
#'   \item \eqn{\Omega} is the variance-covariance matrix for these coefficients.
#'   \item \eqn{\mathbf{1}} is a column vector of ones.
#' }
#'
#' This calculation accounts for potential correlation across
#' time-specific coefficients and is more accurate than
#' simply averaging individual standard errors.
#'
#' @return A named list with:
#' \describe{
#'   \item{mean}{The average of the coefficient estimates.}
#'   \item{se}{The standard error of the aggregated mean.}
#' }
#'
#' @examples
#' \dontrun{
#' model <- create_felm_model(mydata, dep_var, indep_vars)
#' centered <- create_centered_coeffs(model, covar = "studentloan_upb_pc",
#'                                    start_date = "2023-04-01",
#'                                    treatment_date = "2023-10-01")
#' stats <- create_sum_coeffs_stats(centered$data, centered$vcv, 
#'                                  start_date = "2023-10-01", 
#'                                  end_date = "2023-12-31")
#' print(stats)
#' }
#'
#' @export
create_sum_coeffs_stats <- function(coeffs_data, vcv, start_date, end_date) {

  # Find index for filtering by date
  start_idx <-  min(which(coeffs_data$date >= ymd(start_date)))
  end_idx <- max(which(coeffs_data$date <= ymd(end_date)))

  # Filter data to specified length from date indicies
  coeffs <- coeffs_data[start_idx:end_idx, ]$estimate 
  # Calculate the mean
  mean <- mean(coeffs)

  # Calculate the SE for sum of random variables
  n <- length(coeffs)
  vcv <- vcv[start_idx:end_idx, start_idx:end_idx]
  # Vector of ones to take a linear combinations of RV
  one <- matrix(1, nrow = n, ncol = 1)

  # Calculate variance of mean of coefficients
  var <- (1 / n^2) * t(one) %*% vcv %*% one  
  se <- sqrt(var)

  return(list("mean" = mean, "se" = se))
}

#' Create average effect bar chart by different time periods
#'
#' @param model. Model object from event study regression
#' @param covar_of_interest A character/string specifying which model covariate to center over the time period and eventually plot. 
#' @param time_period_starts A vector of start dates for the time periods (e.g., ("2023-01-01", "2023-10-01")).
#' @param time_period_ends A vector of end dates for the time periods (e.g., ("2024-01-01", "2024-01-01")).
#' @param winsorize_val Level to winsorize the parameter passed from \cod{winsorize_var}.
#'
#' @return Returns a bar chart of the average effect by time period.
#'
#' @examples
#' create_time_period_avg_effect_plot(my_model, c("2023-01-01", "22023-10-01"), c("2024-01-01", "2024-01-01"), 0.995)
create_time_period_avg_effect_plot <- function(model, 
                                               covar_of_interest = NULL,
                                               time_period_starts, 
                                               time_period_ends, 
                                               winsorize_val = NULL) {
  
  avg_effects <- data.frame(time_period = character(), 
                            average_effect = numeric(),
                            se = numeric()
                            )
  
  for (i in seq_along(time_period_starts)) {
    start_date <- ymd(time_period_starts[i])
    if (is.na(time_period_ends[i])) { 
      end_date <- max(ymd(coeffs_data$date)) 
    } else {
      end_date <- ymd(time_period_ends[i])
    } 
    
    # Extract and center coefficients
    centered_model <- create_centered_coeffs(model = model, 
                                             covar = covar_of_interest,
										     start_date = "2023-04-01",
                                             treatment_date = "2023-10-01")
    # Create mean and se object
    stats <- create_sum_coeffs_stats(centered_model$data, 
                                     centered_model$vcv, 
                                     start_date, 
                                     end_date)
    
    # Calculate average effect
    avg_effect <- 10000 * stats$mean
    avg_effect_se <- 10000 * stats$se 

    # Create label for time period
    time_period_label <- glue("{format(start_date, \"%b '%y\")} to {format(end_date, \"%b '%y\")}")
    
    avg_effects <- rbind(avg_effects, data.frame(time_period = time_period_label, 
                                                 average_effect = avg_effect,
                                                 se = avg_effect_se
                                                  ))
  }

  # Set output directory
  file_output <- "./output/figures/bar-charts"
  # Set filename directory
  filename <- glue("average_effect_by_time_periods")
  
  # Create bar chart
  plot <- ggplot(avg_effects, aes(x = factor(time_period), y = average_effect)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_errorbar(aes(ymin = average_effect - 1.96 * se, 
                      ymax = average_effect + 1.96 * se), 
                  width = 0.5,
                  size = 1.25,
                  color = "black") +
    labs(
      # title = "Average Effect by Time Periods",
      x = "Time Period",
      y = "Spending Response per \\$10,000 in Student Loans (\\$)"
    ) +
    geom_hline(yintercept = 0,
               linetype = "solid",
               color = "black") +
    plot_theme(show_x_title = T) + 
    guides(
      y = guide_axis(minor.ticks = TRUE)
    ) +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(colour = "black", size = 12),
      axis.text.y = element_text(colour = "black", size = 12)
    ) 

  # Print to viewer pane
  print(plot)

  # Output .tex file with TikZ conversion of plot
  tikz(file =  glue("{file_output}/tex/{filename}.tex"),
       width = 10,
       height = 7)

  print(plot)

  dev.off()
  
  # Save plot to file
  ggsave(
    plot = plot,
    filename = glue("{filename}.png"),
    device = "png",
    path = glue("{file_output}/png"),
    width = 12,
    height = 9
  )
}


create_binscatter_plot <- function(data, quantile_var = NULL, covar, num_bins = 10, 
                                   titles = list(), tikz_dimensions = c(7, 7)) {
  
  # Set default value for varible
  if(is.null(quantile_var)) {
    quantile_var <- "studentloan_upb_pc"
  }
  
  # Set quasi-quotation for dplyr functionality
  quantile_var <- rlang::parse_expr(quantile_var)
  covar <- rlang::parse_expr(covar)

  # Apply condition to divide "covar" by 1,000,000 if it equals "studentloan_upb"
  if (rlang::as_string(covar) == "studentloan_upb") {
    data <- data %>% mutate(!!covar := !!covar / 1e6)
  }
  
  # Create bins for the quantille variable 
  filtered_data <- data %>% 
    distinct(zipcode, .keep_all = T) %>% 
    mutate(quantile = ntile(!!quantile_var, num_bins)) 
  # Create data for each quantile
  binscatter_data <- filtered_data %>% 
    group_by(quantile) %>% 
    dplyr::summarize(n = n(),
                     quantile_var_mean = mean(!!quantile_var, 
                                              na.rm = T),
                     covar_mean = mean(!!covar, 
                                       na.rm = T),
                     covar_sum = sum(!!covar),
                     se_covar = sd(!!covar) / sqrt(n))
    print(binscatter_data)
  # Set output path
  file_output <- "./paper/figures/binscatters"
  if(!dir.exists(file_output)) dir.create(file_output, recursive = TRUE)
  if(!dir.exists(file_output)) dir.create(glue("{file_output}/png"), recursive = TRUE)
  if(!dir.exists(file_output)) dir.create(glue("{file_output}/tex"), recursive = TRUE)

  # Set filename
  filename <- glue("binscatter_{quantile_var}-{covar}_{num_bins}")

  # Set default tiles for plots
  default_titles <- list(
    main = "",
    x = "Student Loan Per Capita Decile",
    y = glue("Mean of {gsub('_', ' ', stringr::str_to_title(rlang::as_string(covar)))}")
  )
  # Use default titles if specific titles are not provided
  main_title <- ifelse(is.null(titles$main), default_titles$main, titles$main)
  x_title <- ifelse(is.null(titles$x), default_titles$x, titles$x)
  y_title <- ifelse(is.null(titles$y), default_titles$y, titles$y)
  
  # Create binscatter plot by quantile
  plot <- binscatter_data %>% 
    ggplot(aes(x = as.factor(quantile), 
               y = covar_mean),
               group = 1) +
    plot_theme() +
    guides(
      y = guide_axis(minor.ticks = TRUE)
    ) +
    labs(
      title = main_title,
      x = x_title,
      y = y_title
    ) + 
    geom_point(size = 3)
   # Conditionally add geom_line() if covar is "studentloan_upb"
  if (rlang::as_string(covar) == "studentloan_upb") {
        binscatter_data <- binscatter_data %>% 
            mutate(covar_sum = covar_sum / 1000,
                   se_covar = se_covar) 
        plot <- binscatter_data %>% 
            ggplot(aes(x = as.factor(quantile), 
                       y = covar_sum),
                       group = 1) +
            plot_theme() +
            guides(
              y = guide_axis(minor.ticks = TRUE)
            ) +
            labs(
              title = main_title,
              x = x_title,
              y = y_title
            ) + 
            scale_y_continuous(
                limits = c(0, 400)
            ) + 
            geom_point(size = 3) +
            geom_line(group = 1) 
            # geom_errorbar(aes(ymin = covar_sum - 1.96 * se_covar, 
            #           ymax = covar_sum + 1.96 * se_covar), 
            #       width = 0.5,
            #       size = 1.25,
            #       color = "black")  + 
  }

  print(plot)
  # Output .tex file with TikZ conversion of plot
  tikz(file =  glue("{file_output}/tex/{filename}.tex"),
      #  pointsize = 12,
       width = tikz_dimensions[1],
       height = tikz_dimensions[2])
  # Print to viewer pane
  print(plot)

  dev.off()
  
  ggsave(
    plot = plot,
    filename = glue("{filename}.png"),
    device = "png",
    path = glue("{file_output}/png"),
    width = 12,
    height = 9
  )
  
}

#' Create a LaTeX Table with kableExtra
#'
#' This function generates a LaTeX table using `kableExtra` with customizable options
#' such as column names, caption, label, footnotes, an optional header row above the table,
#' and grouped row headers for better sectioning.
#'
#' @param data A data frame or matrix to be rendered as a table.
#' @param col_names A character vector of column names to use for the table header.
#' @param filename A string specifying the file path and name to save the table (should end in `.tex`).
#' @param row_names Logical; whether to include row names (default: `FALSE`).
#' @param caption (Optional) A string to use as the table's caption.
#' @param label (Optional) A string to use as the LaTeX label for the table (for referencing in documents).
#' @param footnote (Optional) A string to add as a footnote in the table. It can include LaTeX commands.
#' @param header_above (Optional) A named vector specifying the header row above the table (like `add_header_above()`).
#' @param row_groups (Optional) A named list specifying row groupings. 
#' Each name in the list represents a group title, and its corresponding value should be a vector of row indices
#' that belong to that group. These group headers are bolded for clarity.
#'
#' @return This function saves the table to a `.tex` file at the specified location and does not return anything.
#'
#' @examples
#' # Example usage with row groups
#' create_latex_table(
#'   data = summary_stats_data,
#'   col_names = c("Quintile", "Balance", "MPC (Oct-Apr)", "MPC (Jan-Apr)", "ZIP Codes"),
#'   filename = "./output/tables/summary-stats.tex",
#'   caption = "Summary Statistics",
#'   label = "summary_stats",
#'   footnote = "\\footnotesize{This table provides summary statistics.}",
#'   row_groups = list(
#'     "Student Loan Balances" = 1:5,
#'     "Credit Data" = 6:10,
#'     "Credit & Mortgage Data" = 11:15
#'   )
#' )
#'
#' @importFrom kableExtra kbl kable_styling add_header_above footnote save_kable pack_rows
#' @importFrom dplyr %>%
#' @export
#' 
create_latex_table <- function(data, 
                               col_names, 
                               filename, 
                               row_names = FALSE,
                               caption = NULL, 
                               label = NULL, 
                               footnote = NULL, 
                               header_above = NULL,
                               row_groups = NULL) {  # <-- New parameter for row groupings
  
  # Create the base table
  table <- kbl(data, 
               format = "latex",
               booktabs = TRUE,
               col.names = col_names,
               align = "l",
               digits = 1,
               format.args = list(decimal.mark = '.', big.mark = ","),
               row.names = row_names,  
               caption = caption,
               label = label,
               position = "H",
               escape = FALSE
  )
  
  # Add header above if provided
  if (!is.null(header_above)) {
    table <- table %>%
      add_header_above(header_above, escape = FALSE)
  }
  
  # Apply row groupings if provided
  if (!is.null(row_groups)) {
    for (group in names(row_groups)) {
      indices <- row_groups[[group]]
      table <- table %>%
        pack_rows(group, indices[1], indices[length(indices)], 
				  bold = FALSE, 
				  escape = FALSE)
    }
  }
  
  # Add styling
  table <- table %>%
    kable_styling(
      position = "center",
      latex_options = c("scale_down", "repeat_header")
    )
  
  # Add footnote if provided
  if (!is.null(footnote)) {
    table <- table %>%
      footnote(
        general = footnote,
		general_title = "",
        threeparttable = TRUE,
        escape = FALSE,
        footnote_as_chunk = TRUE
      )
  }
  
  # Save the table
  table %>%
    save_kable(
      file = filename,
      float = FALSE
    )
}