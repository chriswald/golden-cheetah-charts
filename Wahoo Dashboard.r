$$activity <- GC.activity()

# number of readings to bucket together
$$n = 10

$$alt <- $$activity$altitude
$$altgroup <- split($$alt, ceiling(seq_along($$alt)/$$n))
$$alt2 <- sapply($$altgroup, mean)

$$time <- $$activity$seconds[seq(1, length($$activity$seconds), $$n)]
$$time2 <- lapply($$time, $$displaytime)

$$displaytime <- function(timevalue) {
	$$totalMinutes <- timevalue %/% 60
	$$totalSeconds <- sprintf("%02d", timevalue %% 60)
 	paste($$totalMinutes, $$totalSeconds, sep=":")
}

$$makePlot <- function(data, col, title, ylab) {
	datagroup <- split(data, ceiling(seq_along(data)/$$n))
	data2 <- sapply(datagroup, mean)
	dataAvg <- mean(data)

	scaleAltToData <- function(altvalue) {
		maxData <- max(data2)
		return($$scaleAltToValue(altvalue, maxData))
	}

	altScaledToData <- sapply($$alt2, scaleAltToData)
	
	m <- length($$time)
	data.x.poly <- c($$time, $$time[m], $$time[1])
	data.y.poly <- c(data2, 0, 0)

	alt.x.poly <- c($$time, $$time[m], $$time[1])
	alt.y.poly <- c(altScaledToData, 0, 0)
	
	colorWithAlpha <- do.call("rgb", as.list(c(col2rgb(col), max=255, alpha=125)))

	plot($$time, data2,
		col=col,
		type="l",
		xlab="Time",
		xaxt="n"
	)
	polygon(alt.x.poly, alt.y.poly, col=gray(0.3), border=NA)
	polygon(data.x.poly, data.y.poly, col=colorWithAlpha, border=NA)
	lines($$time, data2, col=col, type="l")
	axis(side=1, at=$$time, 
		labels=$$time2
	)
	title(main=paste(title, "- Avg:", round(dataAvg, 1), ylab), ylab=ylab)
}

$$scaleAltToValue <- function(altvalue, maxvalue) {
	minAlt <- min($$alt2)
	maxAlt <- max($$alt2)
	ratio <- maxvalue / (maxAlt - minAlt)
	return((altvalue - minAlt) * ratio)
}

$$plotPowerCurve <- function() {
	power <- $$activity$power
	k <- density(power)
	maxPower <- max(power)
	maxDensity <- max(k$y)

	plot(k, ylim=c(0,maxDensity),
		xlim=c(0,maxPower),
		col="orange", main="", xlab="", yaxt="n")
	title(main="Power distribution",
		xlab="Watts")
}


$$numPlots=5
GC.page(width=dev.size("px")[1],height=$$numPlots*400)
par(mfrow=c($$numPlots,1))

# Speed plot
$$makePlot($$activity$speed, "cadetblue1", "Speed", "KPH")

# Power plot
$$makePlot($$activity$power, "yellow", "Power", "Watts")

# Power curve
$$plotPowerCurve()

# Cadence plot
$$makePlot($$activity$cadence, "purple", "Cadence", "RPM")

# Temperature plot
$$makePlot($$activity$temperature, "green", "Temperature", "C")