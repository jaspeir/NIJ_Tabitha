library(readxl)
library(tibble)

datasetPath = system.file("extdata", "JS_DRSA_Trial_Key_and_Data_Table.xlsx", package = "DRSA", mustWork = TRUE)

decisionTable = readxl::read_excel(path = datasetPath, sheet = "Input_Data")

metaData = tribble(
  ~name, ~type, ~alpha, ~beta,
  'OBJECT', 'object', NA, NA,
  'UNAME', 'misc', NA, NA,
  'QIMG', 'misc', NA, NA,
  'KIMG', 'misc', NA, NA,
  'Q1', 'similarity', 0.2, 0.0,
  'Q2', 'similarity', 0.0, 1.0,
  'Q3OD', 'dominance', NA, NA,
  'Q3PSO', 'dominance', NA, NA,
  'Q3PSD', 'dominance', NA, NA,
  'Q3OTHVAL', 'dominance', NA, NA,
  'Q3OTH_RESP', 'misc', NA, NA,
  'QF2', 'indiscernibility', NA, NA,
  'QF4', 'similarity', 0.0, 1.0,
  'QF3ST', 'indiscernibility', NA, NA,
  'QF3PD', 'indiscernibility', NA, NA,
  'QF3IL', 'indiscernibility', NA, NA,
  'QF3ISP', 'indiscernibility', NA, NA,
  'QF1',  'decision', NA, NA
)

informationTable = InformationTable$new(decisionTable, metaData)
save(decisionTable, file = "data/trial-informationTable.RData")
