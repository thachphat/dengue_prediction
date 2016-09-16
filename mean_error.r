source("db.R")

for (diem in diemdo) {
	MULTI_REGRESSION_meanError <- getMeanError("MULTI_REGRESSION", diem)
	TIME_SERIES_meanError <- getMeanError("TIME_SERIES", diem)
	NEURAL_NETWORK_meanError <- getMeanError("NEURAL_NETWORK", diem)
	ANFIS_meanError <- getMeanError("ANFIS", diem)
	
	# ve cac duong len do thi
	colors = c('red', 'green', 'blue', 'purple')
	lineTypes = c(1:4)
	times <- paste(MULTI_REGRESSION_meanError$thang, MULTI_REGRESSION_meanError$nam, sep="/")
	plot(MULTI_REGRESSION_meanError$saiso, type='l', col=colors[1], xlab="Thoi gian", ylab="Sai so", lty=lineTypes[1], lwd=1.5)
	axis(1, at = 1:length(times),labels = times)
	lines(TIME_SERIES_meanError$saiso, col=colors[2], lty=lineTypes[2], lwd=1.5)
	lines(NEURAL_NETWORK_meanError$saiso, col=colors[3], lty=lineTypes[3], lwd=1.5)
	lines(ANFIS_meanError$saiso, col=colors[4], lty=lineTypes[4], lwd=1.5)
	title("Bieu do so sanh sai so giua cac giai thuat")
	legend("topleft",legend=c("Multi Regression", "Time Series", "Neural Network", "ANFIS"), lty=lineTypes, lwd=1.5, col=colors, title="Algorithms")
}