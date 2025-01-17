# Read and clean catch data from Sharepoint

# 1/16/25
# JGM


##############################################
# eventually, read directly from sharepoint?

# library (httr)
# library (readxl)
# library (tidyverse)
# 
# # https://stackoverflow.com/questions/28048979/accessing-excel-file-from-sharepoint-with-r
# # not sure this will work on both windows and Mac
# 
# # Sharepoint file "BSC Total Catch Calculation Example"
# file_url <- "https://edforg.sharepoint.com/:x:/r/sites/FisheriesOceans-FisherySolutionsCenter/_layouts/15/Doc.aspx?sourcedoc=%7BAB2655D6-08EC-44AC-85B1-8667BBE32157%7D&file=BSC%20Total%20Catch%20Calculation%20Example.xlsx&action=default&mobileredirect=true"
# 
# # save the excel file to disk
# GET(file_url, 
#     authenticate(active_directory_username, active_directory_password, "ntlm"),
#     write_disk("tempfile.xlsx", overwrite = TRUE))
