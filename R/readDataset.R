library(readxl)

datasetPath = system.file("extdata", "JS_DRSA_Trial_Key_and_Data_Table.xlsx", package = "DRSA", mustWork = TRUE)
decisionTable = readxl::read_excel(path = datasetPath, sheet = "Input_Data")
