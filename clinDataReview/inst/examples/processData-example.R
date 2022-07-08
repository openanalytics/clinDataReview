library(clinUtils)

data(dataADaMCDISCP01)

dataLB <- dataADaMCDISCP01$ADLBC

# filter and annotate data
processData(
  data = dataLB,
  processing = list(
    list(filter = list(var = "ANL01FL", value = "Y")),
    list(annotate = list(vars = "ANRIND", varFct = 'factor(ANRIND, levels = c("L", "N", "H"))'))
  )
)

## multiple filtering steps:

# If these are specified in the same 'filter' step condition, these are considered independently,
# and the selected records combined with an 'AND' operator.
# Example: consider only records:
# - with analysis flag AND
# - from subject with high/low measurement (for all records) for each parameter
processData(
  data = dataLB,
  processing = list(
    list(filter = list(
      list(var = "ANL01FL", value = "Y"),
      list(var = "ANRIND", value = c("L", "H"), 
           postFct = any, varsBy = c("USUBJID", "PARAM"))
    )
    )
  )
)

# a custom operator to combine the selected records can be specified
# Example: consider only records:
# - with analysis flag OR
# - from subject with high/low measurement (for all records) for each parameter
processData(
  data = dataLB,
  processing = list(
    list(filter = list(
      list(var = "ANL01FL", value = "Y"),
      "|",
      list(var = "ANRIND", value = c("L", "H"), 
           postFct = any, varsBy = c("USUBJID", "PARAM"))
    )
    )
  )
)

# If the filtering conditions are specified in different filtering steps, these are
# considered sequentially.
# Example: 
# 1) consider only analysis records and 
# 2) from these records, consider only subject with high/low measurement for 
# each parameter
processData(
  data = dataLB,
  processing = list(
    list(filter = list(var = "ANL01FL", value = "Y")),
    list(filter = list(var = "ANRIND", value = c("L", "H"), 
      postFct = any, varsBy = c("USUBJID", "PARAM")))
  )
)
# Note for this particular 

