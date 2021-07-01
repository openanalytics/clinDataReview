
#' Boxplot interactive plot.
#' @param colorVar (optional) String with color variable.
#' @param colorLab String with label for \code{colorVar}.
#' @param facetVar (optional) String with facet variable.
#' @param facetLab String with label for \code{facetVar}.
#' @param ncol single-length integer denoting the number of columns for the facetting.
#' @inheritParams clinDataReview-common-args-summaryStatsVis
#' @inheritParams clinDataReview-common-args
#' @inheritParams tableClinData
#' @inherit scatterplotClinData return
#' @example inst/examples/boxplotClinData-example.R
#' @family visualizations of summary statistics for clinical data
#' @import plotly
#' @importFrom clinUtils getColorPalette
#' @importFrom plyr ddply
#' @author Lennart Tuijnder
#' @export
boxplotClinData <- function(
	data, 
	# x/y variables:
	xVar, yVar, 
	xLab = getLabelVar(xVar, labelVars = labelVars),
	yLab = getLabelVar(yVar, labelVars = labelVars), 
	# aesthetic
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = NULL,
	facetVar = NULL, facetLab = getLabelVar(facetVar, labelVars = labelVars),
	ncol = 1L,
	# general plot:
	titleExtra = NULL,
	title = paste(paste(yLab, "vs", xLab, titleExtra), collapse = "<br>"),
	labelVars = NULL,
	# interactivity:
	width = NULL, height = NULL,
	hoverVars, hoverLab,
	pathVar = NULL, pathLab = getLabelVar(pathVar, labelVars = labelVars),
	idVar = "USUBJID", idLab = getLabelVar(idVar, labelVars = labelVars),
	table = FALSE, 
	tableVars, tableLab,
	tableButton = TRUE, tablePars = list(),
	id = paste0("plotClinData", sample.int(n = 1000, size = 1)),
	verbose = FALSE){
	
	
	# store input parameter values for further use
	plotArgs <- c(as.list(environment()))
	
	### Checks
	if(!is.data.frame(data)) stop("data is not a data.frame")
	
	colsToCheck <- c(xVar, yVar, colorVar, facetVar)
	validColsEntry <- sapply(colsToCheck, function(x) is.character(x) && length(x))
	
	if(any(!validColsEntry)) stop("Argument(s): ", colsToCheck[!validColsEntry], " are not a string.")
	if(any(!colsToCheck %in% colnames(data))){
		notFound <- colsToCheck[!colsToCheck %in% colnames(data)]
		stop("Column(s): ", toString(notFound), " not found in data.")
	}
	
	## Create combined factor column where each element corresponds to 
	## A single box plot.
	idBoxVars <- c(xVar, colorVar, facetVar)
	data$idEl <- do.call(paste, c(data[, idBoxVars, drop = FALSE], list(sep = ".")))
	
	idPointVars <- c(xVar, yVar, colorVar, facetVar)
	data$idPoint <- do.call(paste, c(data[, idPointVars, drop = FALSE], list(sep = ".")))
	
	if(is.null(colorPalette)){
		colorPaletteOpt <- getOption("clinDataReview.colors")
		if(!is.null(colorVar)){
			colorPalette <- getColorPalette(
				x = data[, colorVar], 
				palette = colorPaletteOpt
			)
		}else	colorPalette <- getColorPalette(n = 1, palette = colorPaletteOpt)
	}
	
	# Hover info:
	# format data to: 'SharedData' object
	if(missing(hoverVars)){
		hoverVars <- c(xVar, yVar, colorVar, facetVar)
		hoverLab <- setNames(c(xLab, yLab, colorLab, facetLab), hoverVars)
	}else	if(missing(hoverLab)){
		hoverLab <- getLabelVar(hoverVars, labelVars = labelVars)
	}
	hoverVars <- unique(hoverVars)
	
	
	
	######## Box-plots figure arguments ########
	
	# Prepare arguments for singleBoxPlot
	dataList <- if(!is.null(facetVar)) split(data,data[[facetVar]]) else list(data)	
	showLegend <- c(
		if(!is.null(colorVar)) TRUE else FALSE, 
		rep(FALSE, length(dataList)-1)) # Show only one legend else all the facets add a legend
	
	if(is.null(facetVar)) ncol <- 1
	nrow = ceiling(length(dataList)/ncol)	

	# get plot dim
	dimPlot <- getSizePlotClinData(
		width = width, height = height,
		legend = !is.null(colorVar),
		nrow = nrow, ncol = ncol,
		legendPosition = "bottom"
	)
	width <- unname(dimPlot["width"])
	height <- unname(dimPlot["height"])
	
	# Default size configurations:
	axisLabelFontsize <- 15
	titleFontSize <- 18
	panelWidth <- 20 
	panelSide <- "top"
	
	
	## Help function to create a single sub-plot.
	singleBoxPlot <- function(data, showLegend, row, col){
		
		dataSharedData <- formatDataForPlotClinData(
			data = data, 
			hoverVars = hoverVars, hoverLab = hoverLab,
			hoverByVar = "idPoint",
			keyVar = "idEl", id = id, # All the different data sets need to have same Id for linking.
			labelVars = labelVars
		)
		
		## Box plot
		pl <- plot_ly(
			dataSharedData, 
			x = varToFm(xVar), y = varToFm(yVar), 
			ids = varToFm(idVar),
			color = if(!is.null(colorVar)) varToFm(colorVar)	else	I(colorPalette), 
			colors = if(!is.null(colorVar))	colorPalette,
			type = "box",
			legendgroup = if(!is.null(facetVar) && !is.null(colorVar)) varToFm(colorVar),
			showlegend = showLegend,
			width = width, height = height,
			text = varToFm("hover"),
			hoveron = c("boxes", "points")
		)
		
		## Settings that need to be specified on indiviudal plot level:
		
		if(!is.null(colorVar)){
			pl <- layout(pl, boxmode = "group") # This throws warning but is not: https://github.com/ropensci/plotly/issues/994
		}
		
		if(!is.null(facetVar)){
			facetText <- unique(data[[facetVar]])
			pl <- addFacetPanel(
				pl = pl, 
				panelLab = facetText,
				panelWidth = panelWidth,
				fontSize = axisLabelFontsize,
				side = panelSide
			)
		}
		
		return(pl)
		
	}
	
	col_row <- expand.grid(col = 1:ncol, row = 1:nrow)[1:length(dataList),]
	plotList <- mapply(
		FUN = singleBoxPlot,
  	data = dataList,
		col = col_row$col,
		row = col_row$row,
  	showLegend = showLegend,
  	SIMPLIFY = FALSE)
	
	pl <- subplot(
		plotList,
  	nrows = nrow,
  	shareX = TRUE,
  	titleY = FALSE,
  	titleX = FALSE,
		margin = if(nrow > 1 &&  panelSide == "top"){
				# Add additional bottom margin to each sub figure. 
				# Else facet panel overlaps with fig above.
	  		# (note that this margin does not apply to the very last figure. (hence the nRows - 1)
	  		# Because the last row is uneffect, The height of the figures in the last row will be 
    		# slightly larger. This is how plotly has decided to define margin....
	  		# 
	  		# Units of margin = fraction of the combined figure (roughly).
	  		# Example: if height = 400 px and margin = 0.02 => 400*0.02 = 8px between the plots. 
				c( 0.02, 0.02, 0.02, ((nrow * panelWidth)/height)/(nrow-1))
			} else {
				0.02 # the default 
			}
	)
	
	## Additional settings:
	
	# x-Label y-Label and title
	pl <- pl %>% layout(
		annotations = list(
			list( #create y-label via annotation.
				x = 0, y=0.5, xshift = -60, # position y-label
				text = yLab,
				textangle = 270,
      	showarrow = F,
				font = list(size = axisLabelFontsize),
				xref='paper', yref='paper'
			),
			list(#create x-label via annotation.
				x = 0.5, xref='paper', # place in the middle under the figures. 
				y = -0.125/nrow,  # y_xlabel < 0 (else collision with ticks) and y_xlabel > y_legend
				yref='paper',# important that yref = the same as the legend yref. 
  			yanchor = "top", # else collision with the figures. 
				text = xLab,
      	showarrow = F,
				font = list(size = axisLabelFontsize)
	))
	)
	
	# Title
	if(!is.null(facetVar) && panelSide == "top"){
		pl <-	pl %>% layout(
			title = list(
				text = title, 
				font = list(size = titleFontSize),
				# Place the title at the very top of the plotting region. 
				yref = "container",
				y = 1
			),
			margin = list(t = panelWidth + titleFontSize*4/3) # Roughly conversion font point size to pixel .
		)
	}else{
		pl <- pl %>% layout(title = list(text = title, font = list(size = titleFontSize)))
	}
	
	# Legend
	pl <- pl %>% layout(
		legend = list(
			orientation = "h", 
			x = 0.5, 
			xanchor = "center",
			y = -0.2/nrow, # Imporant that y_legend < y_xlabel (such that legend is below xlabel)
			yanchor = "top", # Important that this is top such that it not collides with the x-label. (multi-row legend will expand downwards)
			title = list(text = colorLab)
		)
	)
	## Custom interactive behaviour 
  
	pl <- formatPlotlyClinData(
		data = data, pl = pl,
		idVar = idVar, pathVar = pathVar,
		# extract ID from 'id' column directly the plot output object
		idFromDataPlot = FALSE, idVarPlot = "id",
		id = id, 
		verbose = verbose
	)
	
	# create associated table
	if(table){
		
		tableVars <- getPlotTableVars(
			plotFunction = "boxplotClinData", 
			plotArgs = plotArgs
		)
		tableLab <- attr(tableVars, "tableLab")
		
		table <- tableClinData(
			data = data, 
			idVar = idVar, idLab = idLab,
			keyVar = "idEl", 
			pathVar = pathVar, pathLab = pathLab,
			pathExpand = FALSE,
			tableVars = tableVars,
			tableLab = tableLab,
			tableButton = tableButton, tablePars = tablePars,
			id = id, 
			labelVars = labelVars
		)
		
		res <- list(plot = pl, table = table)
		
		class(res) <- c("clinDataReview", class(res))
		
	}else{
		res <- pl
	}
	
	return(res)
}




#' Add facet-panel to single plotly plot.
#' 
#' @param pl a plotly object to which to add a single facet panel
#' @param panelLab text to be shown in the facet panel
#' @param panelWidth thickness of the panel in pixels. 
#' @param fontSize fontsize of \code{facetText}
#' @param side the side of the plot to show the panel 
#' 		(currently only right panels are implemented.)
#' 
#' @return plotly object with the facet panel added. 
#' @author lennart tuijnder
#' @import plotly
#' 
#' @details plot title clipping.
#' 
#' Incase case side = 'top', the plot title (eg. layout(title = "title"))
#' will clip with the top pannel. 
#' 
#' Resolve this with the following configutations: (once all the subplots have already been combined)
#' \code{
#' layout(
#' 		title = list(text = "title", yref = "container", y = 1)) # place the title at absolute top of the page
#' 		margin = list(t = panelWidth + heightTitleTextInPixels) # If font size = 15 roughly equal to 20 pixels. 
#' }
#' 
addFacetPanel <- function(pl, panelLab,
	panelWidth = 20, fontSize = 15, side = c("top","right")){
	
	side <- match.arg(side)
	
	if(side == "right"){
		pl %>% add_annotations(
    		text = panelLab,
    		x = 1,              # Actual position terms of xref
				xshift = 2 + panelWidth/2, # additional shift to the x-position in pixels. (center in panel)
    		y = 0.5,            # Actuall position in terms of yref
    		yref = "paper",     # Controlls the reference system in which to provide the x - units.
    		xref = "paper",     # Controlls the reference system in which to provide the y - units
    		xanchor = "center", # Controlls the x-anchor of the text
				yanchor = "middle", # Controlls the y-anchor of the text
    		showarrow = FALSE,  # Annotation come by default with an arrow. (disable it we just want text)
				textangle = 90,     # Read the text at an angle
    		font = list(size = fontSize) # font size. 
  		) %>%
  		layout(
    		shapes = list(
      		type = "rect",
      		x0 = 0,
      		x1 = panelWidth,
      		xref = "paper",
					xanchor = 1,
      		y0 = 0, 
      		y1 = 1,
					yanchor = "bottom",
      		yref = "paper",
					xsizemode = "pixel",
      		fillcolor = toRGB("gray80"),
      		line = list(color = "transparent")
    		),
				margin = list(r = panelWidth)
  		)
	}else if(side == "top"){
		pl %>% add_annotations(
    		text = panelLab,
    		x = 0.5,
    		y = 1,
				yshift = 2 + panelWidth/2,
    		yref = "paper",
    		xref = "paper",
    		yanchor = "middle",
				xanchor = "center",
    		showarrow = FALSE,
    		font = list(size = fontSize)
  		) %>%
  		layout(
    		shapes = list(
      		type = "rect",
      		x0 = 0,
      		x1 = 1,
      		xref = "paper",
      		y0 = 0, 
      		y1 = panelWidth,
      		yanchor = 1,
      		yref = "paper",
      		ysizemode = "pixel",
      		fillcolor = toRGB("gray80"),
      		line = list(color = "transparent")
    		),
				margin = list(t = panelWidth)# 100 is the default. 
			)
	}
}
