getMeanSD <-
function(GSE_readcount_input, GSE_smoking_status_labels){
  
	current_smoker_indices <- c(1:length(which(GSE_smoking_status_labels[1,] == "Current_smoker")))
	never_smoker_indices <- c(length(current_smoker_indices):length(GSE_smoking_status_labels))
	never_smoker_indices <- never_smoker_indices[-1]
	
	# Make dataframe to contain the info
	GSE_CS_NS_mean_SD <- data.frame(
	  Genes = rownames(GSE_readcount_input),
	  Current_smoker_count = NA,
	  Current_smoker_mean = NA,
	  Current_smoker_SD = NA,
	  Never_smoker_count = NA,
	  Never_smoker_mean = NA,
	  Never_smoker_SD = NA
	)
	
	# Loop through each gene and calculate mean and standard deviation
	for (i in 1:nrow(GSE_readcount_input)) { #Looping through rows I hope?
	  Current_row <- GSE_readcount_input[i,] #Extract the row
	  current_smoker_values <- GSE_readcount_input[i, current_smoker_indices]
	  never_smoker_values <- GSE_readcount_input[i, never_smoker_indices]
	  
	  # Calculate mean and standard deviation for Current_smoker
	  current_mean <- mean(current_smoker_values)
	  current_sd <- sd(current_smoker_values)
	  
	  # Calculate mean and standard deviation for Never_smoker
	  never_mean <- mean(never_smoker_values)
	  never_sd <- sd(never_smoker_values)
	  
	  # Add results to the new dataframe
	  GSE_CS_NS_mean_SD[i, 2:4] <- c(length(current_smoker_values), current_mean, current_sd)
	  GSE_CS_NS_mean_SD[i, 5:7] <- c(length(never_smoker_values), never_mean, never_sd)
	}
	return(GSE_CS_NS_mean_SD)
}
