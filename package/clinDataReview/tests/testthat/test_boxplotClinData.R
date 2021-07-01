context("Boxplot for clinical data")


library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

## example of basic barplot:

data <- subset(dataADaMCDISCP01$ADVS, 
	PARAMCD == "DIABP" & ANL01FL == "Y" &
		AVISIT %in% c("Baseline", "Week 2", "Week 4", "Week 6", "Week 8")
)

## patient profiles report

# add path in data
data$patientProfilePath <- paste0(
	"patientProfiles/subjectProfile-", 
	sub("/", "-", data$USUBJID), ".pdf"
)
# add link in data (for attached table)
data$patientProfileLink <- with(data,
	paste0(
		'<a href="', patientProfilePath, 
		'" target="_blank">', USUBJID, '</a>'
	)
)

# Suppress the following warning as this is a bug in plotly: 
# See https://github.com/ropensci/plotly/issues/994
ignoreBoxmodeWarning <- function(expr){
	withCallingHandlers(
		expr,
		warning = function(w){
			if(grepl("^'layout' objects don't have these attributes: 'boxmode'",w$message)){
				invokeRestart("muffleWarning")
			}
		}
	)		
}

test_that("plotting function runs properly", {
		
		# create plot
		pl <- ignoreBoxmodeWarning({
				boxplotClinData(
					data = data,
					xVar = "AVISIT", 
					yVar = "AVAL",
					colorVar = "TRTA",
					facetVar = "ATPT",
					title = "Diastolic Blood Pressure distribution by actual visit and analysis timepoint",
					yLab = "Actual value of the Diastolic Blood Pressure parameter (mmHg)",
					labelVars = labelVars,
					pathVar = "patientProfileLink", pathLab = getLabelVar(var = "USUBJID", labelVars = labelVars)
				)})

		## check if input == output data
		
		# extract data from output object
		plData <- ignoreBoxmodeWarning(plotly_build(pl)$x$data)
	
		# only box aes
		plDataBox <- plData[sapply(plData, function(x) x$type == "box")]
		
		plDataBoxDf <- do.call(rbind,
			lapply(plDataBox, function(x) 
					data.frame(
						AVISIT = as.character(x[["x"]]), 
						AVAL = x$y,
						TRTA = x$legendgroup,
						# To get facetVar look at the key definition. This extraction relies on the
						# column facetVar position in the key-column definition. 
						# Ideally it should be made more general in the future. 
						ATPT = strsplit(as.character(x$key),"\\.")[[1]][[3]], 
						stringsAsFactors = FALSE
					)
			)
		)
		
		data <- data[c("AVISIT","AVAL","TRTA","ATPT")]
		
		# order data to compare with each other
		data <- data[with(data,order(ATPT, TRTA, AVISIT, AVAL)),]
		plDataBoxDf <- plDataBoxDf[with(plDataBoxDf,order(ATPT, TRTA, AVISIT, AVAL)),]
		expect_equivalent(object = data, expected = plDataBoxDf)
		
	})



test_that("interactive table is created", {
		
		# create plot
		res <- ignoreBoxmodeWarning({
				boxplotClinData(
					data = data,
					xVar = "AVISIT", yVar = "AVAL", colorVar = "TRTA", facetVar = "ATPT",
					table = TRUE,
					pathVar = "patientProfileLink", pathLab = getLabelVar(var = "USUBJID", labelVars = labelVars)
				)})
		
		expect_s3_class(res$table, "datatables")
		
	})

test_that("Boxplot with hoverVars without label", {
    
		# create plot
		res <- ignoreBoxmodeWarning({
				boxplotClinData(
					data = data,
					xVar = "AVISIT", yVar = "AVAL", colorVar = "TRTA", facetVar = "ATPT",
					hoverVars = c("AVISIT", "AVAL"),
					pathVar = "patientProfileLink", pathLab = getLabelVar(var = "USUBJID", labelVars = labelVars)
				)})
		
    expect_s3_class(res, "plotly")
    
  })

test_that("Boxplot without facet or color or both is created", {
		
		# create plot
		res <- ignoreBoxmodeWarning({
				boxplotClinData(
					data = data,
					xVar = "AVISIT", yVar = "AVAL", colorVar = "TRTA", facetVar = NULL, 
				)})
		
    expect_s3_class(res, "plotly")
		
		# create plot
		res <- ignoreBoxmodeWarning({
				boxplotClinData(
					data = data,
					xVar = "AVISIT", yVar = "AVAL", colorVar = NULL, facetVar = "ATPT", 
				)})
		
    expect_s3_class(res, "plotly")
		
		# create plot
		res <- ignoreBoxmodeWarning({
				boxplotClinData(
					data = data,
					xVar = "AVISIT", yVar = "AVAL", colorVar = NULL, facetVar = NULL, 
				)})
		
    expect_s3_class(res, "plotly")
  })



