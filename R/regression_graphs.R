##### FUNCTION TO MAKE THE GRAPHS OF THE LINEAR REGRESSIONS WITH CONFIDENCE INTERVALS #####
# THE INPUTs WILL BE A JAGS OUTPUT AND A VECTOR OF THE FACTORS AND EACH VARIABLE TO BE GRAPHED
# THE OUTPUTS WILL BE GRAPHS OF THE REGRESSION GRAPHS WITH CONFIDENCE INTERVALS

#' Create regression graphs with confidence intervals
#' @description
#' A function to create graphs of the raw data with the modeled line and confidence intervals
#' @param jags_output JAGS output file from the model
#' @param dataframe name of the dataframe containing the raw data input to the model
#' @param factor_column_name a string of the column name with the factors in dataframe
#' @param variable_columns a vector of strings of the column names that contain the variables in dataframe
#' @param factor_names a vector of strings with the names of the factors. This is for titling the graph and must be the same order as they are in dataframe
#' @param response_variable_column a string of the response variable column in dataframe
#' @param jags_intercept_names string of the name of the intercept variable in the JAGS file
#' @param jags_slope_names a vector of strings of the names of the slopes within the JAGS file
#' @return a group of graphs with the regressions and 95% confidence intervals of the JAGS model
#' @export
#'
#'
#'

regression_graphs = function(jags_output, # jags model output file
                             dataframe, # dataframe with the raw data put into model
                             factor_column_name, # column name with the factors within dataframe
                             variable_columns, # vector with the names of the columns with variables in dataframe
                             factor_names, # vector with the names of the factors that are graphed
                             response_variable_column, # name of column with the response variable
                             jags_intercept_names, # name of intercept in jags file
                             jags_slopes_names # vector of the names of the slopes in the jags output

){
  # 1. set mfrow to 1, length(factor_names)/3
  # 2. nested loop for 1:length(variable_columns)
    # create a separate group of graphs for each variable
  # 3. loop for 1:length(factor_names)
  # 4. create the intial graphs (code below)
  # 5. add in the confidence intervals, use code from the class
  # 6. reset the mfrow at the end
  for (i in 1:length(variable_columns)){
    # set the number of rows
    nrows = ceiling((length(factor_names))/3)
    par(mfrow=c(nrows,3))
    # pull out data from jags output
    pars = jags_output$BUGSoutput$mean
    pars_all = jags_output$BUGSoutput$sims.list
    # loop to create graphs
    for (n in 1:length(factor_names)){
      # make variable object
      variable = as.numeric(dataframe[[variable_columns[i]]])
      # set slope name for current factor
      jags_slope = jags_slopes_names[i]
      # make the slopes and intercepts a dataframe
      intercepts = data.frame(pars[jags_intercept_names])
      slopes = data.frame(pars[jags_slope])
      intercepts_all = data.frame(pars_all[jags_intercept_names])
      slopes_all = data.frame(pars_all[jags_slope])
      # things needed for confidence intervals
      n_sims = jags_output$BUGSoutput$n.sims
      x_grid = pretty(variable, n = 100)
      # select data for the current factor
      curr_dat = subset(dataframe, dataframe[[factor_column_name]] == n)
      # plot data for current factor
      plot(curr_dat[[variable_columns[i]]], curr_dat[[response_variable_column]], main = factor_names[n],
           xlab = var_col_names[i])
      # add in lines
      #lines(sort(variable), intercepts[n,1] + slopes[n,1]*sort(variable),col = n)
      # add in confidence intervals
      all_lines = matrix(NA, ncol = length(x_grid), nrow = n_sims)
      for (j in 1:n_sims){
        all_lines[j,] = intercepts_all[j,n] + slopes_all[j,n]*x_grid
      }
        all_lines_summary = apply(all_lines,2,'quantile',probs = c(0.05, 0.5, 0.95))
        lines(x_grid + mean(variable), all_lines_summary[2,], col = n)
        lines(x_grid + mean(variable), all_lines_summary[1,], col = n, lty = 'dotted')
        lines(x_grid + mean(variable), all_lines_summary[3,], col = n, lty = 'dotted')

    }
  }

}

# var_col_names = c("Lat", "Year", "JD")
# fac_names = c('Anchovy','Spot','Weakfish')
# jags_slopes = c("slopelat", "slopeyear", "slopejd")

# regression_graphs(jags_run, df, "CommonName", var_col_names, fac_names, "d15N", "intercept",jags_slopes)

# tested and works