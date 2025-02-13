library(ggplot2)
library(dplyr)
library(rlang)
library(stringr)

visualize <- function(df, attribute1, attribute2, columnName1, columnName2) {
  # Convert column names into symbols for tidy evaluation
  attr1 <- sym(attribute1)
  attr2 <- sym(attribute2)
  
  # Visualize two numeric attributes
  if (is.numeric(df[[attribute1]])) {
    if (is.numeric(df[[attribute2]])) {
      print(df |>
              ggplot(aes(x = !!attr1, y = !!attr2)) +
              geom_point() +
              labs(x = str_glue("{columnName1}"), y = str_glue("{columnName2}")))
    }
  }
  
  # Visualize two categorical attributes
  if (is.factor(df[[attribute1]])) {
    if (is.factor(df[[attribute2]])) {
      print(df |>
              ggplot(aes(x = !!attr1, fill = !!attr2)) +
              geom_bar(position = "dodge") +
              labs(x = columnName1, fill = columnName2))
      
      # Create a heatmap of counts
      print(df |>
              ggplot(aes(x = !!attr1, y = !!attr2)) +
              geom_bin2d() +
              labs(x = columnName1, y = columnName2))
    }
  }
  
  # Visualize one numeric and one categorical attribute
  if (is.numeric(df[[attribute1]])) {
    if (is.factor(df[[attribute2]])) {
      print(df |>
              ggplot(aes(x = !!attr1, y = !!attr2, color = !!attr2)) +
              geom_jitter(height = 0.5) +
              labs(x = str_glue("{columnName1}"), y = str_glue("{columnName2}")) +
              theme(legend.position = "none"))
      
      print(df |>
              ggplot(aes(x = !!attr1)) +
              geom_density(aes(color = !!attr2), lwd = 1) +
              labs(x = str_glue("{columnName1}"), y = "Density"))
      
      print(df |>
              ggplot(aes(x = !!attr2, y = !!attr1, fill = !!attr2)) +
              geom_violin(alpha = 0.7, color = "black") +
              labs(x = str_glue("{columnName2}"), y = str_glue("{columnName1}")))
    }
  }
}


visualize(
  df = data_no_na,
  attribute1 = "Physical.BMI",
  attribute2 = "PCIAT.PCIAT_Total",
  columnName1 = "BMI",
  columnName2 = "PCIAT.PCIAT_Total"
)

visualize(
  df = data_no_na,
  attribute1 = "Physical.Diastolic_BP",
  attribute2 = "PCIAT.PCIAT_Total",
  columnName1 = "Diastolic Blood Pressure",
  columnName2 = "PCIAT.PCIAT_Total"
)

visualize(
  df = data_no_na,
  attribute1 = "Fitness_Endurance.Max_Stage",
  attribute2 = "PCIAT.PCIAT_Total",
  columnName1 = "Fitness_Endurance.Max_Stage",
  columnName2 = "PCIAT.PCIAT_Total"
)

visualize(
  df = data_no_na,
  attribute1 = "FGC.FGC_PU",
  attribute2 = "PCIAT.PCIAT_Total",
  columnName1 = "FGC.FGC_PU",
  columnName2 = "PCIAT.PCIAT_Total"
)

visualize(
  df = data_no_na,
  attribute1 = "BIA.BIA_Activity_Level_num",
  attribute2 = "sii",
  columnName1 = "BIA.BIA_Activity_Level_num",
  columnName2 = "SII"
)

for (x in colnames(data_no_na)) {
  visualize(
    df = data_no_na,
    attribute1 = x,
    attribute2 = "PCIAT.PCIAT_Total",
    columnName1 = x,
    columnName2 = "PCIAT.PCIAT_Total"
  )
}
