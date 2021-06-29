context("Scatterplot for clinical data")

library(plotly)

## create input data
dataLB <- data.frame(
    "USUBJID" = as.character(1 : 5),
    "SAFFL" = "Y",
    "AVAL" = rnorm(5),
    "VISIT" = c("SCREENING 1", "SCREENING 1", "SCREENING 1", "SCREENING 1", "SCREENING 1"),
    "VISITNUM" = c(1, 2, 3, 1, 1),
    "PARAMCD" = c("ALT", "ALT", "ALT", "ALT", "ALT"),
    "LBDY" = c(21, -19, 1, 15, 29),
    "LBSTRESN" = c(39, 93, 10, 31, 13),
    "LBSTNRLO" = 0,
    "LBSTNRHI" = 50,
    "LBNRIND" = c("HIGH", "NORMAL", "LOW", "HIGH", "LOW"),
    stringsAsFactors = FALSE
)
labelVars <- c(
    "USUBJID" = "Unique Subject Identifier",
    "SAFFL" = "Safety Analysis Set",
    "AVAL" = "Actual values",
    "VISIT" = "Visit",
    "VISITNUM" = "Visit numeric",
    "PARAMCD" = "Parameter code",
    "LBDY" = "",
    "LBSTRESN" = "",
    "LBSTNRLO" = "Lower bound",
    "LBSTNRHI" = "Upper bound",
    "LBNRIND" = "Category"
)

# add baseline as extra column:
dataPlot <- dataLB
dataPlotBL <- subset(dataPlot, VISIT == "SCREENING 1")
dataPlotBL <- dataPlotBL[with(dataPlotBL, order(USUBJID, -LBDY)), ]
dataPlotBL <- dataPlotBL[!duplicated(dataPlotBL$USUBJID), ]
dataPlot$LBSTRESNBL <- dataPlot[
    match(dataPlot$USUBJID, dataPlotBL$USUBJID), "LBSTRESN"
]

# sort visits:
dataPlot$VISIT <- with(dataPlot, reorder(VISIT, VISITNUM))


test_that("plotting function aesthetic testing", {
      
      pl <- scatterplotClinData(
          data = dataPlot, 
          xVar = "LBSTRESNBL",
          xLab = paste(labelVars["LBSTRESN"], "for last screening visit"),
          yVar = "LBSTRESN",
          yLab = paste(labelVars["LBSTRESN"], "at visit X"),
          aesPointVar = list( 
              color = "USUBJID", fill = "USUBJID",
              group = "USUBJID", stroke = "USUBJID",
              shape = "USUBJID", 
              alpha = "LBSTRESNBL", size = "LBSTRESNBL"
          ),
          aesLineVar = list(
              group = "USUBJID", #color = "USUBJID", 
              alpha = "LBSTRESNBL",
              linetype = "USUBJID", 
              size = "LBSTRESNBL"
          ),
          idVar = "USUBJID"
      )
      
      ## check input == output data
      
      # extract input data
      cols <- c("LBSTRESNBL", "LBSTRESN", "USUBJID")
      dataPointLine <- dataPlot[, cols]
      dataPointLine <- dataPointLine[do.call(order, dataPointLine), ]
      dataPointLine <- subset(dataPointLine, ! is.na(LBSTRESNBL))
      
      # check data points
      plData <- plotly_build(pl)$x$data
      plDataPointsAll <- plData[sapply(plData, function(x) (x$mode == "markers" & !is.null(x$set)))]
      plDataPoints <- lapply(plDataPointsAll, function(x)
            data.frame(
                x = as.numeric(x$x), 
                y = as.numeric(x$y), 
                key = as.character(x$key),
                stringsAsFactors = FALSE
            )
      )
      plDataPoints <- setNames(do.call(rbind, plDataPoints), cols)
      plDataPoints <- plDataPoints[do.call(order, plDataPoints), ]
      
      expect_equivalent(
          object = plDataPoints, expected = dataPointLine
      )# all equal, no attributes
      
      # check data lines
      plDataLinesAll <- plData[sapply(plData, function(x) (x$mode == "lines" & !is.null(x$set)))]
      plDataLines <- lapply(plDataLinesAll,  function(x)
            data.frame(
                x = as.numeric(x$x), 
                y = as.numeric(x$y), 
                legend = sub("^\\(([^,]*).+", "\\1", x$legendgroup),
                stringsAsFactors = FALSE
            )
      )
      plDataLines <- setNames(do.call(rbind, plDataLines), cols)
      plDataLines <- plDataLines[do.call(order, plDataLines), ]
      
      expect_equivalent(object = plDataLines, expected = dataPointLine)# all equal, no attributes
            
    })

test_that("plotting function: reference lines", {
      
      xLine <- mean(dataPlot$LBSTRESNBL)
      yLine <- mean(dataPlot$LBSTRESN)
      pl <- scatterplotClinData(
          data = dataPlot, 
          xVar = "LBSTRESNBL", yVar = "LBSTRESN",
          aesPointVar = list(color = "USUBJID"),
          aesLineVar = list(group = "USUBJID", color = "USUBJID"),
          facetPars = list(facets = "VISIT"),
          refLinePars = list(
              # fixed values, no labels
              list(slope = 1, intercept = 0, linetype = 1, color = "black", label = FALSE),
              list(xintercept = xLine), 
              list(yintercept = yLine),
              list(slope = 1, intercept = 0, linetype = 1, color = "black", label = FALSE),
              # from aesthetic
              # default label
              list(xintercept = "LBSTNRLO", linetype = 2, color = "orange", alpha = 0.5), 
              # custom label:
              list(yintercept = "LBSTNRHI", linetype = 2, color = "orange", label = "Reference Range Upper Limit"),
              # combined
              list(slope = 1, intercept = "LBSTNRLO")
          )
      )
      
      ## check input == output data
      
      # extract input values for reference lines
      dataRefLines <- unique(dataPlot[, c("VISIT", "LBSTNRLO", "LBSTNRHI")])
      
      # extract reference lines from output object
      plData <- plotly_build(pl)$x$data
      plDataRefLines <- plData[sapply(plData, function(x) (x$mode == "lines" & is.null(x$set)))]
      
      # check if lines are in the plot
      # ideally we should also check if they are in the correct facet
      # but the mapping facet <-> line doesn't seem to be easily extracted from the plotly_build output
      isRefLineXInPlot <- all(c(dataRefLines$LBSTNRLO, xLine) %in% unlist(lapply(plDataRefLines, function(x) x$x)))
      expect_true(isRefLineXInPlot, info = "All specified horizontal lines are plotted.")
      
      isRefLineYInPlot <- all(c(dataRefLines$LBSTNRHI, yLine) %in% unlist(lapply(plDataRefLines, function(x) x$y)))
      expect_true(isRefLineYInPlot, info = "All specified vertical lines are plotted.")
            
    })

test_that("plotting function: labels", {
      
      xLab <- paste("Baseline", labelVars["LBSTRESN"])
      title <- "Actual value of lab parameter at each visit vs baseline"
      pl <- scatterplotClinData(
          data = dataPlot, 
          xVar = "LBSTRESNBL", yVar = "LBSTRESN",
          xLab = xLab, # custom label
          aesPointVar = list(color = "USUBJID", shape = "LBNRIND"),
          aesLineVar = list(group = "USUBJID", color = "USUBJID"),
          facetPars = list(facets = "VISIT"),
          labelVars = labelVars,
          title = title
      )
      
      ## check input == output data
      
      # extract labels from output object
      yLab <- labelVars["LBSTRESN"]
      plLayout <- plotly_build(pl)$x$layout
      plLayoutAnnot <- plLayout$annotations
      
      # title
      expect_identical(plLayout$title$text, title)
      
      # axes labels
      plAxes <- plLayoutAnnot[sapply(plLayoutAnnot, function(x) "annotationType" %in% names(x) &&x$annotationType == "axis")]
      plAxesLabels <- sapply(plAxes, function(x) x$text)
      expect_equivalent(plAxesLabels, xLab)
      
      # facet labels
      #plAnnotAll <- sapply(plLayoutAnnot, function(x)x $text)
      #expect_true(unique(dataPlot$VISIT) %in% plAnnotAll)
            
    })

test_that("interactive table is created", {
      
      res <- scatterplotClinData(
          data = dataPlot, 
          xVar = "LBSTRESNBL", yVar = "LBSTRESN",
          table = TRUE
      )
      expect_is(res$table, "datatables")
      
    })

test_that("facetting", {
      
      plNoFacet <- scatterplotClinData(
          data = dataPlot, 
          xVar = "LBSTRESNBL", yVar = "LBSTRESN",
          aesPointVar = list(color = "USUBJID"),
          aesLineVar = list(group = "USUBJID", color = "USUBJID")
      )
      
      # facet_wrap: character test high number of facets
      plWrapSt <- scatterplotClinData(
          data = dataPlot, 
          xVar = "LBSTRESNBL", yVar = "LBSTRESN",
          aesPointVar = list(color = "USUBJID"),
          aesLineVar = list(group = "USUBJID", color = "USUBJID"),
          facetPars = list(facets = "USUBJID"),
          themePars = list(legend.position = "right")
      )
      
      # facet_wrap: formula
      plWrapFm <- scatterplotClinData(
          data = dataPlot, 
          xVar = "LBSTRESNBL", yVar = "LBSTRESN",
          aesPointVar = list(color = "USUBJID"),
          aesLineVar = list(group = "USUBJID", color = "USUBJID"),
          facetPars = list(facets = as.formula(~VISIT + USUBJID))
      )
      
    })

test_that("Specified limits expanded with data limits", {
      
      xLim <- c(min(dataPlot$LBSTRESNBL) + diff(range(dataPlot$LBSTRESNBL))/2, max(dataPlot$LBSTRESNBL))
      yLim <- c(min(dataPlot$LBSTRESN) + diff(range(dataPlot$LBSTRESN))/2, max(dataPlot$LBSTRESN))
      
      plNoExpandLim <- scatterplotClinData(
          data = dataPlot, 
          xVar = "LBSTRESNBL", yVar = "LBSTRESN",
          aesPointVar = list(color = "USUBJID"),
          aesLineVar = list(group = "USUBJID", color = "USUBJID"),
          facetPars = list(facets = as.formula(~VISIT)),
          xLim = xLim, yLim = yLim,
          xLimExpandData = FALSE, yLimExpandData = FALSE
      )
      
      
      plExpandLim <- scatterplotClinData(
          data = dataPlot, 
          xVar = "LBSTRESNBL", yVar = "LBSTRESN",
          aesPointVar = list(color = "USUBJID"),
          aesLineVar = list(group = "USUBJID", color = "USUBJID"),
          facetPars = list(facets = as.formula(~VISIT)),
          xLim = xLim, yLim = yLim
      )		
      
      expect_gt(
          object = diff(plotly_build(plExpandLim)$x$layout$xaxis$range),
          expected = diff(plotly_build(plNoExpandLim)$x$layout$xaxis$range),
          label = "limits of x-axis expanded from data greater than specified limits"
      )
      expect_gt(
          object = diff(plotly_build(plExpandLim)$x$layout$yaxis$range),
          expected = diff(plotly_build(plNoExpandLim)$x$layout$yaxis$range),
          label = "limits of y-axis expanded from data greater than specified limits"
      )			
      
    })

test_that("Scatterplot with hoverVars without label", {
      
      plOutput <- scatterplotClinData(
          data = dataPlot, 
          xVar = "LBSTRESNBL", yVar = "LBSTRESN",
          hoverVars = c("LBSTRESNBL", "USUBJID")
      )
      expect_is(plOutput, "plotly")
      
    })

test_that("No legend", {
      
      plOutput <- scatterplotClinData(
          data = dataPlot, 
          xVar = "LBSTRESNBL", yVar = "LBSTRESN",
          themePars = list(legend.position = "none")
      )
      expect_is(plOutput, "plotly")
      
    })

test_that("custom color palette is specified", {
      
      data <- data.frame(
          DY = c(1, 2, 1, 2),
          AVAL = c(3, 4, 2, 6),
          NRIND = c("Normal", "High", "Low", "Normal"),
          USUBJID = c(1, 1, 2, 2),
          stringsAsFactors = FALSE
      )
      colorPalette <- c(
          Low = "purple", 
          Normal = "green", 
          High = "blue"
      )
      pl <- scatterplotClinData(
          data = data, 
          xVar = "DY", 
          yVar = "AVAL", 
          aesPointVar = list(color = "NRIND"),
          scalePars = list(
              list(aesthetic = "colour", values = colorPalette)	
          )
      )
      plData <- plotly_build(pl)$x$data
      plData <- lapply(plData, function(x){
            data.frame(
                x = as.numeric(x$x),
                y = as.numeric(x$y),
                NRIND = as.character(x$name),
                color = as.character(x$marker$line$color),
                stringsAsFactors = FALSE
            )
          })
      plData <- do.call(rbind, plData)
      plData <- plData[do.call(order, plData), ]
      
      # plotly specifies color in rgba
      colorPaletteRGB <- col2rgb(colorPalette)
      colorPaletteRGBA <- paste0(
          "rgba(", 
          apply(colorPaletteRGB, 2, paste, collapse = ","), 
          ",1)" # + alpha
      )
      names(colorPaletteRGBA) <- colorPalette
      
      # extract color for input data:
      data$color <- colorPaletteRGBA[colorPalette[data$NRIND]]
      dataPlot <- data[, c("DY", "AVAL", "NRIND", "color")] 
      dataPlot <- dataPlot[do.call(order, dataPlot), ]
      
      expect_equal(dataPlot, plData, check.attributes = FALSE)
      
    })
