require("neuralnet")
source("db.R")

model_type <- "NEURAL_NETWORK_3_TO_1"
number_of_rep <- 10
min_threshold <- 0.005
offset <- 3
selected_cols <- c("somac", "di", "bi", "him", "m", "thang", "nam", "hil", "ci", "ad", "nd")
first_layer_nodes = floor((length(selected_cols) - 2) * 3 / 2)
second_layer_nodes = floor(first_layer_nodes / 2)

saveOrUpdateNeuralNetwork <- function() {
	#create table if not exist
	dbGetQuery(con, "CREATE TABLE IF NOT EXISTS ytdp_neural_network (model_type text NOT NULL, tham_so text NOT NULL, diemdo integer NOT NULL, layer_nodes text, first_layer_weights text, second_layer_weights text)")
	
	# generate tham so
	tham_so = toString(names(df_predict))
	
	#generate node data
	weights = lapply(nn$weights[[min_rep]], round, 3)
	layer_nodes = toString(c(first_layer_nodes, second_layer_nodes))
	first_layer_weights = toString(weights[[1]])
	second_layer_weights = toString(weights[[2]])
	
	#save to db	
	existsSQL <- sprintf("SELECT EXISTS(SELECT 1 FROM ytdp_neural_network WHERE diemdo = %d AND model_type = '%s')", diem, model_type)
	insertSQL <- sprintf("INSERT INTO ytdp_neural_network (model_type, tham_so, diemdo, layer_nodes, first_layer_weights, second_layer_weights) VALUES ('%s', '%s', %d, '%s', '%s', '%s');", model_type, tham_so, diem, layer_nodes, first_layer_weights, second_layer_weights)
	updateSQL <- sprintf("UPDATE ytdp_neural_network SET tham_so = '%s', layer_nodes = '%s', first_layer_weights = '%s', second_layer_weights = '%s' WHERE diemdo = %d AND model_type = '%s';", tham_so, layer_nodes, first_layer_weights, second_layer_weights, diem, model_type)
	#save or update predicted values
	saveOrUpdate(existsSQL, insertSQL, updateSQL)
}

calculatePredictData <- function() {
	for(i in length(predict.nn_):1){
		#get month and year for prediction
		predict_month <- months[i]
		predict_year <- years[i] + 1

		predict <- floor(predict.nn_[i])
		if(is.na(predict)||predict < 0){
			predict <- 0
		}
		print(paste0("Predict value of ", predict_month, "/", predict_year, " is ", predict))

		existsSQL <- sprintf("SELECT EXISTS(SELECT 1 FROM ytdp_prediction WHERE thang = %d AND nam = %d AND diemdo = %d AND phuong_phap = '%s')", predict_month, predict_year, diem, model_type)
		insertSQL <- sprintf("INSERT INTO ytdp_prediction (thang, nam, diemdo, phuong_phap, sm_du_bao) VALUES (%d, %d, %d, '%s', %e);", predict_month, predict_year, diem, model_type, predict)
		updateSQL <- sprintf("UPDATE ytdp_prediction SET sm_du_bao = %e WHERE thang = %d AND nam = %d AND diemdo = %d AND phuong_phap = '%s';", predict, predict_month, predict_year, diem, model_type)
		#save or update predicted values
		saveOrUpdate(existsSQL, insertSQL, updateSQL)
	}
	saveOrUpdateNeuralNetwork()
}

createDataframe <- function(df) {
	months <- unique(df$thang)
	years <- unique(df$nam)

	result_df = data.frame()
	#loop from 2008 to 2010
  for (year in tail(years, -offset)) {
  	for (month in months) {
  		first_year = subset(df[df$nam == year & df$thang == month,], select = -c(thang,nam))
  		second_year = subset(df[df$nam == year + 1 & df$thang == month,], select = -c(thang,nam))
  		third_year = subset(df[df$nam == year + 2 & df$thang == month,], select = -c(thang,nam))
  		fourth_year = df[df$nam == year + 3 & df$thang == month,]
  		data_row = data.frame(c(fourth_year[1], first_year, second_year, third_year))
  		result_df = rbind(result_df, data_row)
  	}
  }

	return(result_df)
}

createTestDataframe <- function(df) {
	months <- unique(df$thang)
	years <- unique(df$nam)

	result_df = data.frame()
	#loop from 2008 to 2011
  for (year in tail(years, -offset + 1)) {
  	for (month in months) {
  		first_year = subset(df[df$nam == year & df$thang == month,], select = -c(thang,nam))
  		second_year = subset(df[df$nam == year + 1 & df$thang == month,], select = -c(thang,nam))
  		third_year = subset(df[df$nam == year + 2 & df$thang == month,], select = -c(thang,nam))
  		data_row = data.frame(first_year, second_year, third_year)
  		result_df = rbind(result_df, data_row)
  	}
  }

	return(result_df)
}

# loop through diemdo, subset diemdo, make correlation for each nam
for (diem in diemdo) {
	sub_df_diem <- subset(df_postgres, diemdo == diem, select = selected_cols)

	# get data for prediction
	months <- sub_df_diem$thang
	years <- sub_df_diem$nam
	sub_df_diem = createDataframe(sub_df_diem)

	#normalize data
	maxs <- apply(sub_df_diem, 2, max)
	mins <- apply(sub_df_diem, 2, min)
	scaled <- as.data.frame(scale(sub_df_diem, center = mins, scale = maxs - mins))

	#calculate model
	n <- names(scaled)
	f <- as.formula(paste("somac ~", paste(n[!n %in% "somac"], collapse = " + ")))
	nn <- neuralnet(f, data=scaled, hidden=c(first_layer_nodes, second_layer_nodes), rep=number_of_rep, threshold = min_threshold)

	#predict
	#tuong tu nhu calculate voi test, input la nam truoc do
	max_somac <- max(sub_df_diem$somac)
	min_somac <- min(sub_df_diem$somac)

	#tinh toan cac model va lua ra model co MSE nho nhat
	min_rep <- which.min(nn$result.matrix["error",])

	#calculate predict data from df_postgres
	df_predict = createTestDataframe(subset(df_postgres, diemdo == diem, select = selected_cols))
	maxs <- apply(df_predict, 2, max)
	mins <- apply(df_predict, 2, min)
	scaled <- as.data.frame(scale(df_predict, center = mins, scale = maxs - mins))
	predict.nn <- compute(nn, scaled, rep = min_rep)
	predict.nn_ <- predict.nn$net.result * (max_somac - min_somac) + min_somac

	#get data for predict and save to db
	print(paste0("Predict for diemdo: ", diem))
	calculatePredictData()
	plot_data <- predict.nn_[,1]

	print("======================================")

	# plot nn 12/2014 -> 1/2008
	plot_data <- c(rep(0, offset * 12), rev(plot_data))
	plot(plot_data, type='l', col='red')
	real_data <- rev(subset(df_postgres, diemdo==diem)$somac)
	lines(real_data, col='blue')
}
