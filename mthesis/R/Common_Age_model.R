# Kleinow (2015) - Common Age Effect model #

# Common Age Effect (CAE) model #
f_CAE <- function(mx_list, lRegion,
                  vSub = c("Female", "Male", "Total")) {
  
  # Create a list for the outputs #
  lOut = list()
  
  # Loop over the regions #
  for (sRegion in names(lRegion)) {
    
    # Extract countries #
    vCountries = lRegion[[sRegion]]
    
    # Loop over subpopulations #
    for (sSub in vSub) {
      
    } # End loop over subpopulations #
    
  } # End loop over regions #
  
}

