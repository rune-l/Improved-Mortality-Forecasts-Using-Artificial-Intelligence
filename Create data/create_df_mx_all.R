# Create df_mx_all #

# Define the path to the mx files #
# Mx files will be created and save using the f_getHMD function from the package #
mx_path = "path_to_mx_files"

# Create empty data.frame #
df_mx_all = data.frame()

# Create the df_mx_all #
for (sFile in mx_files) {
  
  sCountry = sub("\\_mx.*", "", sFile)
  sFile_tmp = paste0(mx_path, "/", sFile)
  df_tmp = read.table(sFile_tmp, header = TRUE)
  df_tmp[, "Country"] = sCountry
  
  df_mx_all = rbind(df_mx_all, df_tmp)
  
}

# Save path #
pkg_path = "path_to_package/mthesis/data"

# Save data to package #
save(df_mx_all, file = paste0(pkg_path, "/", "df_mx_all_data.RData"))

