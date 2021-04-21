context("data annotation")

dataLB <- data.frame(
    "USUBJID" = 1 : 5,
    "SAFFL" = "Y",
    "AVAL" = rnorm(5),
    "VISIT" = c("Day 1", "Day 1", "Day 2", "Day 2", "Day 1"),
    "PARAMCD" = c("ALS", "ALT", "ALT", "BILI", "BILI"),
    stringsAsFactors = FALSE
)
dataDM <- data.frame(
    "USUBJID" = 1 : 5,
    "SAFFL" = "Y",
    "SEX" = c("F", "F", "M", "M", "M"),
    "RACE" = c("WHITE", "HISPANIC", "WHITE", "NOT HISPANIC", "OTHER"),
    "COUNTRY" = c("US", "BE", "FR", "BE", "US"),
    "ACTARM" = c("Screen Failure", "TRT", "Screen Failure", "Placebo", "Screen Failure"),
    stringsAsFactors = FALSE
)

labelVars <- c(
    "USUBJID" = "Sub ID",
    "SAFFL" = "Safety Analysis Set",
    "AVAL" = "Values",
    "VISIT" = "Visit",
    "PARAMCD" = "Parameter code",
    "SEX" = "Sex",
    "RACE" = "Race",
    "COUNTRY" = "Country",
    "ACTARM" = "Actual Arm"
)

test_that("Process data identical to calling pre-processing function separately", {
      
      # each pre-processing function separately
      dataLBAnnot <- annotateData(
          data = dataLB, 
          annotations = list(data = dataDM, vars = c("SEX", "RACE", "COUNTRY", "ACTARM")), 
          labelVars = labelVars
      )
      labelVarsAnnot <- attr(dataLBAnnot, "labelVars")
      dataLBAnnotTreatment <- filterData(
          data = dataLBAnnot, 
          filters = list(var = "ACTARM", value = "Screen Failure", rev = TRUE), 
          labelVars = labelVarsAnnot
      )
      
      # all at once
      dataLBAnnotTreatment2 <- processData(
          data = dataLB,
          processing = list(
              list(annotate = list(data = dataDM, vars = c("SEX", "RACE", "COUNTRY", "ACTARM"))),
              list(filter = list(var = "ACTARM", value = "Screen Failure", rev = TRUE))
          ),
          labelVars = labelVars
      )
      
      expect_identical(dataLBAnnotTreatment, dataLBAnnotTreatment2)
      
    })

test_that("Process data fails is input is not a list", {
      
      # in case user forgets the '-' in the config file
      expect_error(
          res <- processData(
              data = dataLB,
              processing = list(
                  annotate = list(data = dataDM, vars = c("SEX", "RACE", "COUNTRY", "ACTARM")),
                  filter = list(var = "ACTARM", value = "Screen Failure", rev = TRUE)
              ),
              labelVars = labelVars
          )
      )
      
    })
