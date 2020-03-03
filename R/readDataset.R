library(readxl)

datasetPath = system.file("extdata", "JS_DRSA_Trial_Key_and_Data_Table.xlsx", package = "DRSA", mustWork = TRUE)
decisionTable = readxl::read_excel(path = datasetPath, sheet = "Input_Data")

# Set type of variables:
attributes(decisionTable$OBJECT)$type = 'object'
attributes(decisionTable$QF1)$type = 'decision'

attributes(decisionTable$UNAME)$type = 'misc'
attributes(decisionTable$QIMG)$type = 'misc'
attributes(decisionTable$KIMG)$type = 'misc'
attributes(decisionTable$Q3OTH_RESP)$type = 'misc'

attributes(decisionTable$Q3OD)$type = 'dominance'
attributes(decisionTable$Q3PSO)$type = 'dominance'
attributes(decisionTable$Q3PSD)$type = 'dominance'
attributes(decisionTable$Q3OTHVAL)$type = 'dominance'

attributes(decisionTable$QF2)$type = 'indiscernibility'
attributes(decisionTable$QF3ST)$type = 'indiscernibility'
attributes(decisionTable$QF3PD)$type = 'indiscernibility'
attributes(decisionTable$QF3IL)$type = 'indiscernibility'
attributes(decisionTable$QF3ISP)$type = 'indiscernibility'

# Set alpha- and beta-parameter values for the similarity variables
attributes(decisionTable$Q1)$type = 'similarity'
attributes(decisionTable$Q1)$alpha = 0.2
attributes(decisionTable$Q1)$beta  = 0.0

attributes(decisionTable$Q2)$type = 'similarity'
attributes(decisionTable$Q2)$alpha = 0.0
attributes(decisionTable$Q2)$beta  = 1.0

attributes(decisionTable$QF4)$type = 'similarity'
attributes(decisionTable$QF4)$alpha = 0.0
attributes(decisionTable$QF4)$beta  = 1.0

save(decisionTable, file = "data/trial.RData")
