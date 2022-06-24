

# This is to convert our cleaned dataframe to a condensed form that would be easier to pass to a CNN


# First list all the necessary columns  

cnames <- c("adc_0", "adc_1", "adc_2", "adc_3", "adc_4",
            "adc_5", 
            "adc_6",
            "adc_7",
            "adc_8",
            "adc_9",
            "adc_10",
            "adc_11",
            "adc_12",
            "adc_13",
            "adc_14",
            "adc_15",
            "adc_16",
            "adc_17",
            "adc_18",
            "adc_19",
            "adc_20",
            "adc_21",
            "adc_22",
            "adc_23",
            "adc_24",
            "adc_25",
            "adc_26",
            "adc_27",
            "adc_28",
            "adc_29")




# Now create a function to grab batches of matrices and condense them in a nice
# Data frame 


#------------------------------------------------------------------------------

condense <- function(df) {
  
  # get rid of all the other columns and only retain cnames as above
  
  dfn <- df[, cnames]
  
  
  # Now lets get a sequence of starting ending points to loop over 
  # We want to grab batches of 4 rows at a time
  brks <- seq(1, nrow(dfn), 4)  # step in 4 
  
  
  # Now create empty vector to store matrix lists 
  vlm <- vector()
  # loop through the breaks to grab our batches an condense them
  
  for(i in brks) {
    batch <- dfn[seq(i, i+3), ] %>% as.matrix(dimnames=NULL)
    vlm <- append(vlm, list(batch))
    #bat <- rbind.data.frame(bat, batch)
  }
  
  # reshape into dimension of c(length(vlm), nrow*ncol of single matrix)
  vlm <- vlm %>% array_reshape(dim = c(length(vlm), 120)) %>% as.data.frame()
  
  # Return the condensed dataframe
  return(vlm )
  
  
}





#---------------------------------------------------------------------------
# Now we write a function to preprocess our data for our cnn 
# This takes in our dataframe from the condense function above 

cnn_preprocess <- function(df){
  
  # convert dataframe to matrix first
  dfmat <- df %>% as.matrix(dimnames=NULL)
  
  # Reshape into 4 x 30 matrices  
  # dim = c(nrow(df), 4, 30)
  dfmat <- dfmat %>% array_reshape(dim = c(nrow(df), 4, 30))
  
  return(dfmat)
  
}

