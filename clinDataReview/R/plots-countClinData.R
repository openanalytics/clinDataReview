#' Interactive plot of 'count' data
#' 
#' Note: the table and plot are not (yet) linked.
#' @param typePlot String with plot type, 'treemap' or 'sunburst'.
#' @param colorVar (optional) String with coloring variable
#' (NULL by default).
#' By default, the treemap is colored based by section.
#' @param colorRange (optional) Numeric vector of length 2 with range 
#' for the color variable, in case it is a numeric variable.
#' @inheritParams clinDataReview-common-args-summaryStatsVis
#' @inheritParams clinDataReview-common-args
#' @inheritParams tableClinData
#' @inherit scatterplotClinData return
#' @family visualizations of summary statistics for clinical data
#' @import plotly
#' @importFrom stats as.formula
#' @importFrom utils head tail
#' @importFrom clinUtils getColorPalette
#' @author Laure Cougnaud
#' @export
plotCountClinData <- function(
  data, 
  # plot variables:
  vars, varsLab = getLabelVar(vars, labelVars = labelVars),
  valueVar, valueLab = getLabelVar(valueVar, labelVars = labelVars),
  colorVar = NULL, colorLab = getLabelVar(valueVar, labelVars = labelVars),
  colorPalette = getOption("clinDataReview.colors"),
  colorRange = NULL,
  # general plot:
  valueType = "total",
  title = paste(c(
    paste(valueLab, "by", paste(varsLab, collapse = " and ")), 
    titleExtra
  ), collapse = "<br>"),
  titleExtra = NULL,
	subtitle = NULL, caption = NULL,
  labelVars = NULL,
  # interactivity:
  width = NULL, height = NULL,
  pathVar = NULL, pathLab = getLabelVar(pathVar, labelVars = labelVars),
  hoverVars = c(vars, valueVar, colorVar), 
  hoverLab = getLabelVar(hoverVars, labelVars = labelVars),
  table = FALSE, 
  tableVars,
  tableLab,
  tableButton = TRUE, tablePars = list(),
  id = paste0("plotClinData", sample.int(n = 1000, size = 1)),
  verbose = FALSE,
  typePlot = c("sunburst", "treemap"),
  watermark = NULL){
  
  # store input parameter values for further use
  plotArgs <- c(as.list(environment()))
  
  typePlot <- match.arg(typePlot)
  
  # filter missing records for 'vars' (as input for table and plot)
  idxAllNa <- which(rowSums(is.na(data[, vars, drop = FALSE])) == length(vars))
  if(length(idxAllNa) > 0){
    warning(length(idxAllNa), " record(s) are filtered from the data",
        " because contain missing for all variable(s): ",
        toString(shQuote(vars)), "."
    )
    data <- data[-idxAllNa, ]
  }
  
  # for data to hierarchical format
  dataPlot <- formatToHierarchicalData(
	data = data, 
	vars = vars,
	valueVar = valueVar
  )
  varsPlot <- attr(dataPlot, "metadata")
  varID <- varsPlot$varID
  varLabel <- varsPlot$varLabel
  varParent <- varsPlot$varParent
  
  # check that sum (counts children) <= count(parent)
  valueType <- checkValueType(
      data = dataPlot,
      vars = c(varParent, varID), 
      valueVar = valueVar,
      valueType = valueType,
      labelVars = labelVars
  )
  
  # format data to: 'SharedData' object
  dataSharedData <- formatDataForPlotClinData(
      data = dataPlot,
      keyVar = varID, 
      id = id,
      labelVars = labelVars,
      hoverVars = hoverVars, hoverLab = hoverLab,
      hoverByVar = varID
  )
  
  # get plot dim
  dimPlot <- getSizePlot(
      width = width, height = height,
      includeLegend = FALSE,
	  title = title,
	  subtitle = subtitle,
	  caption = caption
  )
  width <- dimPlot[["width"]]
  height <- dimPlot[["height"]]
  
  # get color vector
  colorPaletteOpt <- colorPalette
  
  if(!is.null(colorVar)) {
    
    if(is.numeric(dataPlot[, colorVar])) {
      
      if(is.null(colorRange))	colorRange <- range(data[, colorVar], na.rm = TRUE)
      colorsX <- tapply(dataPlot[, colorVar], dataPlot[, varID], mean)
      colorGroups <- cut(
          colorsX,
          breaks = c(
              -Inf,
              seq(from = colorRange[1], to = colorRange[2], length.out = 8),
              +Inf
          )
      )
      colorPalette <- getColorPalette(
          n = nlevels(colorGroups),
          palette = colorPaletteOpt
      )
      names(colorPalette) <- levels(colorGroups)
      
    } else {
      
      colorGroups <- dataPlot[, colorVar][order(dataPlot[, varID])]
      if(!is.factor(colorGroups))	colorGroups <- factor(colorGroups)	
      colorPalette <- getColorPalette(x = colorGroups, palette = colorPaletteOpt)
    }
    
  } else {
    
    colorGroups <- dataPlot[, varID][order(dataPlot[, varID])]
    if(!is.factor(colorGroups))	colorGroups <- factor(colorGroups)
    colorPalette <- getColorPalette(x = colorGroups, palette = colorPaletteOpt)    
  }
  
  colors <- unname(colorPalette[as.character(colorGroups)])
  
  # create interactive plot:
  pl <- plot_ly(
      data = dataSharedData, 
      ids = varToFm(varID),
      parents = if(!is.null(varParent))	varToFm(varParent), 
      labels = varToFm(varLabel), 
      values = varToFm(valueVar), 
      marker = list(colors = colors), #if(!is.null(colorVar))	list(colors = colors),
      type = typePlot,
      branchvalues = valueType,
      hovertemplate = varToFm("hover"),
      width = width, height = height,
      textinfo = "label"
  )
  
	pl <- layoutClinData(
		p = pl,
		title = title,
		subtitle = subtitle,
		caption = caption,
		includeLegend = FALSE,
		# fix for partial matching of args (legend <-> legendPosition)
		legendPosition = "none",
		width = width,
		height = height,
		# extra params passed to plotly::layout
		# remove the axis labels, included when color is specified in the treemap:
		xaxis = list(showticklabels = FALSE),
		yaxis = list(showticklabels = FALSE),
		watermark = watermark,
		# extra params passed to plotly::layout
		legend = list(title = list(text = colorLab))
	)
  
  # current hovered element identified by d.points[0].label
  
  # specific formatting for clinical data
  pl <- formatPlotlyClinData(
      data = dataPlot, pl = pl,
      idVar = varID, pathVar = pathVar,
      # extract ID from 'id' column in the plot output object directly
      idFromDataPlot = FALSE, idVarPlot = "id",
      # patient prof filename based on the 'id' label
      labelVarPlot = "id",
      # click and double-click events already used to zoom/unzoom in sunburst
      highlightOn = "plotly_selected",
      highlightOff = "plotly_relayout",
      id = id, 
      verbose = verbose
  )
  
  # create associated table
  if(table) {
    
    tableVars <- getPlotTableVars(
        plotFunction = "plotCountClinData", 
        plotArgs = plotArgs
    )
    tableLab <- attr(tableVars, "tableLab")
    tablePars <- attr(tableVars, "tablePars")
    
    table <- tableClinData(
        data = dataPlot, 
        idVar = varID, keyVar = varID,
        pathVar = pathVar, pathLab = pathLab,
        pathExpand = TRUE,
        tableVars = tableVars,
        tableLab = tableLab,
        tableButton = tableButton, tablePars = tablePars,
        # link table <-> plot not functional for sunburst
        # so 'break' temporarily this link (until this functionality is fixed)
        id = paste0(id, "-table"), 
        labelVars = labelVars,
        verbose = verbose
    )
    res <- list(plot = pl, table = table)
    
    class(res) <- c("clinDataReview", class(res))
    
  }else res <- pl
  
  return(res)
  
}