require("neuralnet")
source("db.R")

model_type <- "NEURAL_NETWORK"
number_of_rep <- 10
min_threshold <- 0.005
selected_cols <- c("somac", "di", "bi", "him", "m", "thang", "nam", "hil", "ci", "ad", "nd")
first_layer_nodes = floor((length(selected_cols) - 3) / 2)
second_layer_nodes = floor(first_layer_nodes / 2)

calculatePredictData <- function() {
	for(i in length(predict.nn_):1){
		#get month and year for prediction
		predict_month <- months[i]
		predict_year <- years[i]
	  
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

saveOrUpdateNeuralNetwork <- function() {
	#create table if not exist
	dbGetQuery(con, "CREATE TABLE IF NOT EXISTS ytdp_neural_network (model_type text NOT NULL, tham_so text NOT NULL, diemdo integer NOT NULL, layer_nodes text, first_layer_weights text, second_layer_weights text)")
	
	# generate tham so
	tham_so = toString(n[!n %in% "somac"])
	
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

# loop through diemdo, subset diemdo, make correlation for each nam
for (diem in diemdo) {
	sub_df_diem <- subset(df_postgres, diemdo == diem, select = selected_cols)	
	
	# get data for prediction
	months <- sub_df_diem$thang
	years <- sub_df_diem$nam
	sub_df_diem <- subset(sub_df_diem, select = -c(nam,thang))
	
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
	predict.nn <- compute(nn, scaled[,2:length(sub_df_diem)], rep = min_rep)
	predict.nn_ <- predict.nn$net.result * (max_somac - min_somac) + min_somac
	
	#get data for predict and save to db
	print(paste0("Predict for diemdo: ", diem))
	calculatePredictData()
	plot_data <- predict.nn_[,1]
	
	#calculate predict data from df_predict
	#sub_df_diem <- subset(df_predict, diemdo == diem, select = selected_cols)
	#sub_df_diem <- sub_df_diem[nrow(sub_df_diem):1,]
	#months <- sub_df_diem$thang
	#years <- sub_df_diem$nam
	#sub_df_diem <- subset(sub_df_diem, select = -c(nam,thang))
	
	#scaled <- as.data.frame(scale(sub_df_diem, center = mins, scale = maxs - mins))
	#predict.nn <- compute(nn, scaled[,2:length(sub_df_diem)], rep = min_rep)
	#predict.nn_ <- predict.nn$net.result * (max_somac - min_somac) + min_somac
	#calculatePredictData()
	#plot_data <- append(predict.nn_[,1],plot_data)
	
	print("======================================")
	
	# plot nn 12/2013 -> 1/2008
	plot_data <- rev(plot_data)
	plot(plot_data, type='l', col='red')
	real_data <- rev(subset(df_postgres, diemdo==diem)$somac)
	lines(real_data, col='blue')
}
