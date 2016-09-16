#model = POST$model
#if (model == 1) {
#	fileName = "linear_regression.r"
#} else if (model == 2) {
#	fileName = "linear_regression.r"
#} else if (model == 3) {
#	fileName = "time_series.r"
#} else if (model == 4) {
#	fileName = "neural_network_stable.r"
#} else {
#	fileName = "anfis.r"
#}

cmd = "Rscript -e 'source(\"linear_regression.r\")'; source(\"linear_regression.r\")'; source(\"time_series.r\")'; source(\"neural_network_stable.r\")'; source(\"anfis.r\")'; curl localhost/finish"
system(cmd, wait=FALSE)