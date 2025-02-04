##### FUNCTION TO PULL OUT THE DIC AND PV FROM MANY DIFFERENT JAGS MODELS #####
# THE INPUTS WILL BE A LIST OF JAGS OUTPUTS
# THE OUTPUTS WILL BE A DATAFRAME WITH 1 COLUMN WITH DIC AND 1 WITH PV

#' Compare JAGS models
#' @description
#' A function to compare the DIC and pV of multiple models run through JAGS
#' @param jags_outputs a list of the jags outputs to compare
#'
#' @return a dataframe with the model names as rows and columns with the DIC and pV scores
#' @export

jags_comparisons = function(jags_outputs){

  # make a dataframe to put all the info in
  df = data.frame(
    Model = rep(NA, length(jags_outputs)),
    DIC = rep(NA, length(jags_outputs)),
    pV = rep(NA, length(jags_outputs))
  )

  for (i in 1:length(jags_outputs)){
    # pull out the model name
    model_desc = summary(jags_outputs[[i]]$BUGSoutput$model.file)
    model_name = model_desc$description

    # assign attributes to the dataframe
    df[i, "Model"] = model_name
    df[i, "DIC"] = jags_outputs[[i]]$BUGSoutput$DIC
    df[i, "pV"] = jags_outputs[[i]]$BUGSoutput$pV
  }

  return(df)
}

#test_list = list(jags_JDC_run, jags_JDN_run, jags_latC_run)

#test_func = jags_comparisons(test_list)

# TESTED AND WORKS!