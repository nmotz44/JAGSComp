##### FUNCTION TO MAKE THE LASSO GRAPHS FOR VARIABLE SELECTION #####
# THE INPUTs WILL BE A JAGS OUTPUT AND A VECTOR OF NAMES
# THE OUTPUTS WILL BE GRAPHS OF THE SLOPES AND SIGMA

#' Create variable selection graphs
#' @description
#' A function to make variable selection graphs after running a lassoo to select variables
#' @param jags_output a JAGS file of the model run
#' @param factor_names a vector of strings of the factor names being tested, must be the same order as the input data to the JAGS model. This is for titling purposes
#' @return a group of variable selection graphs
#' @export


variable_selection_graphs = function(jags_output, # JAGS output file
                                     factor_names) # vector of the names of the factors being tested, .
                                                   # must be the same order as input
  {
  # pull out the factors from the JAGS output file
  post = jags_output$BUGSoutput$sims.list
  x_range = range(post$slope)
  # set the shape of the graphs to be made
  nrows = ceiling((ncol(post$slope + 1))/2)
  par(mfrow=c(nrows,2))
  # make the slope graphs
  for(i in 1:ncol(post$slope)) {
    hist(post$slope[,i], breaks = 30, xlim = x_range,
         freq = FALSE, xlab = '', main = paste('slope', factor_names[i]))
    abline(v = 0, col = 'red')
  }
  # add in the sigma graph
  hist(post$sigma_b, breaks = 30,
       main = 'sigma_b')
  # reset the par for the rest of the document
  par(mfrow=c(1,1))
}

# factor_name = c("Lat", "Year", "JD")

# variable_selection_graphs(jags_run_lasso, factor_name)

# TESTED AND WORKS!