# Function to get the data #


f_getHMD <- function(vCountries = "All",
                     vSubPop = c("Male", "Female", "Total"),
                     iAge_int = 1,
                     iYear_int = 1,
                     iMin_year = 1950,
                     iMax_year = 2016,
                     iMin_age = 50,
                     iMax_age = 95,
                     mx_path,
                     ex_path,
                     dx_path,
                     pop_path,
                     sUsername,
                     sPassword) {
  
  # Require the needed packages #
  require(MortalityLaws)
  require(HMDHFDplus)
  
  # Check if All countries is wanted #
  if (vCountries == "All") {
    vCountries = getHMDcountries()
  }
  
  sInterval <- paste0(iAge_int, "x", iYear_int)
  
  # Load mx_data for the countries specified #
  cat("Downloading mx data...")
  
  mx_data <- ReadHMD(what = "mx",
                     countries = vCountries,
                     interval  = sInterval,
                     username  = sUsername,
                     password  = sPassword, save = FALSE)
  
  # Change data-types #
  mx_data$data[, "country"] = as.character(mx_data$data[, "country"])
  mx_data$data[, "Year"] = as.integer(mx_data$data[, "Year"])
  mx_data$data[, "Age"] = as.integer(mx_data$data[, "Age"])
  mx_data$data[, "Female"] = as.numeric(mx_data$data[, "Female"])
  mx_data$data[, "Male"] = as.numeric(mx_data$data[, "Male"])
  mx_data$data[, "Total"] = as.numeric(mx_data$data[, "Total"])
  
  # Load ex_data for the countries specified #
  cat("Downloading ex data...")
  ex_data <- ReadHMD(what = "Ex",
                     countries = vCountries,
                     interval = sInterval,
                     username = sUsername,
                     password = sPassword, save = FALSE)
  
  # Change data types #
  ex_data$data[, "country"] = as.character(ex_data$data[, "country"])
  ex_data$data[, "Year"] = as.integer(ex_data$data[, "Year"])
  ex_data$data[, "Age"] = as.integer(ex_data$data[, "Age"])
  ex_data$data[, "Female"] = as.numeric(ex_data$data[, "Female"])
  ex_data$data[, "Male"] = as.numeric(ex_data$data[, "Male"])
  ex_data$data[, "Total"] = as.numeric(ex_data$data[, "Total"])
  
  # Load dx_data for the specified countries #
  cat("Downloading dx data...")
  dx_data <- ReadHMD(what = "Dx",
                     countries = vCountries,
                     interval = sInterval,
                     username = sUsername,
                     password = sPassword, save = FALSE)
  
  # Specify the data types #
  dx_data$data[, "country"] = as.character(dx_data$data[, "country"])
  dx_data$data[, "Year"] = as.integer(dx_data$data[, "Year"])
  dx_data$data[, "Age"] = as.integer(dx_data$data[, "Age"])
  dx_data$data[, "Female"] = as.numeric(dx_data$data[, "Female"])
  dx_data$data[, "Male"] = as.numeric(dx_data$data[, "Male"])
  dx_data$data[, "Total"] = as.numeric(dx_data$data[, "Total"])
  
  # Load the population data #
  cat("Downloading pop data...")
  pop_data <- ReadHMD(what = "population",
                      countries = vCountries,
                      interval = sInterval,
                      username = sUsername,
                      password = sPassword, save = FALSE)
  
  # Remove year values with - in them #
  vIdx_minus = grepl("-", pop_data$data$Year)
  pop_data$data = pop_data$data[!vIdx_minus, ]
  pop_data$data[, "Year"] = sub("\\+.*", "", pop_data$data[, "Year"])
  
  # Change the data types #
  pop_data$data[, "country"] = as.character(pop_data$data[, "country"])
  pop_data$data[, "Year"] = as.integer(pop_data$data[, "Year"])
  pop_data$data[, "Age"] = as.integer(pop_data$data[, "Age"])
  pop_data$data[, "Female"] = as.numeric(pop_data$data[, "Female"])
  pop_data$data[, "Male"] = as.numeric(pop_data$data[, "Male"])
  pop_data$data[, "Total"] = as.numeric(pop_data$data[, "Total"])
  
  # Find countries with the years and ages requirement fulfilled #
  vKeep_Country = character()
  for (sC in vCountries) {
    
    mxCountry_data = mx_data$data[mx_data$data$country == sC, c("Year", "Age", "Female", "Male",
                                                                "Total", "country")]
    

    exCountry_data = ex_data$data[ex_data$data$country == sC, c("Year", "Age", "Female", "Male",
                                                                 "Total", "country")]

    dxCountry_data = dx_data$data[dx_data$data$country == sC, c("Year", "Age", "Female", "Male",
                                                                "Total", "country")]

    # mxCountry_data = mx_data[mx_data$Country == sC, c("Year", "Age", "Female", "Male",
    #                                                             "Total", "Country")]
    # 
    # exCountry_data = ex_data[ex_data$Country == sC, c("Year", "Age", "Female", "Male",
    #                                                             "Total", "Country")]
    # 
    # dxCountry_data = dx_data[dx_data$Country == sC, c("Year", "Age", "Female", "Male",
    #                                                             "Total", "Country")]
    
    popCountry_data = pop_data$data[pop_data$data$country == sC, c("Year", "Age", "Female", "Male",
                                                                "Total", "country")]
    
    
    if ((min(mxCountry_data[, "Year"], na.rm = TRUE) <= iMin_year) &
        (max(mxCountry_data[, "Year"], na.rm = TRUE) >= iMax_year) &
        (min(mxCountry_data[, "Age"], na.rm = TRUE) <= iMin_age) &
        (max(mxCountry_data[, "Age"], na.rm = TRUE) >= iMax_age)) {
      
      # Append to vKeep_Country vector #
      vKeep_Country = c(vKeep_Country, sC)
      
      # Save paths #
      sMx_path = paste0(mx_path, "/", sC, "_mx.txt")
      sEx_path = paste0(ex_path, "/", sC, "_ex.txt")
      sDx_path = paste0(dx_path, "/", sC, "_dx.txt")
      sPop_path = paste0(pop_path, "/", sC, "_pop.txt")
      
      
      # Prepare mx data #
      mxCountry_data = mxCountry_data[, !(names(mxCountry_data) == "country")]
      mxCountry_data = mxCountry_data[mxCountry_data[, "Year"] >= iMin_year, ]
      mxCountry_data = mxCountry_data[mxCountry_data[, "Year"] <= iMax_year, ]
      mxCountry_data = mxCountry_data[mxCountry_data[, "Age"] >= iMin_age, ]
      mxCountry_data = mxCountry_data[mxCountry_data[, "Age"] <= iMax_age, ]
      
      # Prepare ex data #
      exCountry_data = exCountry_data[, !(names(exCountry_data) == "country")]
      exCountry_data = exCountry_data[exCountry_data[, "Year"] >= iMin_year, ]
      exCountry_data = exCountry_data[exCountry_data[, "Year"] <= iMax_year, ]
      exCountry_data = exCountry_data[exCountry_data[, "Age"] >= iMin_age, ]
      exCountry_data = exCountry_data[exCountry_data[, "Age"] <= iMax_age, ]
      
      # Prepare dx data #
      dxCountry_data = dxCountry_data[, !(names(dxCountry_data) == "country")]
      dxCountry_data = dxCountry_data[dxCountry_data[, "Year"] >= iMin_year, ]
      dxCountry_data = dxCountry_data[dxCountry_data[, "Year"] <= iMax_year, ]
      dxCountry_data = dxCountry_data[dxCountry_data[, "Age"] >= iMin_age, ]
      dxCountry_data = dxCountry_data[dxCountry_data[, "Age"] <= iMax_age, ]
      
      # Prepare pop data #
      popCountry_data = popCountry_data[, !(names(popCountry_data) == "country")]
      popCountry_data = popCountry_data[popCountry_data[, "Year"] >= iMin_year, ]
      popCountry_data = popCountry_data[popCountry_data[, "Year"] <= iMax_year, ]
      popCountry_data = popCountry_data[popCountry_data[, "Age"] >= iMin_age, ]
      popCountry_data = popCountry_data[popCountry_data[, "Age"] <= iMax_age, ]
      
      # Save data #
      write.table(mxCountry_data, sMx_path, sep="\t", row.names = FALSE)
      write.table(exCountry_data, sEx_path, sep="\t", row.names = FALSE)
      write.table(dxCountry_data, sDx_path, sep="\t", row.names = FALSE)
      write.table(popCountry_data, sPop_path, sep="\t", row.names = FALSE)
      
    }
    
  }
  
  
}


# Get mx saved data #
f_get_mx_list <- function(...) {
  
  require(miceadds)
  
  lPath = system.file("data/mx_list.Rdata", package = "mthesis")
  miceadds::load.Rdata(lPath, "mx_list")
  
}

# Get ex saved data #
f_get_ex_list <- function(...) {
  
  require(miceadds)
  
  lPath = system.file("data/ex_list.Rdata", package = "mthesis")
  miceadds::load.Rdata(lPath, "ex_list")
  
}





