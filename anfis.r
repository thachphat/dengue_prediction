require("e1071")
require("neuralnet")
source("D:\\db.R")
init()

STATIC_MF <- 0.5
STATIC_selected_cols <- c("somac", "di", "him", "bi", "hil", "ci", "ad", "m", "nd", "thang", "nam")
STATIC_assign_cols <- c("somac", "di", "him", "bi", "hil", "ci", "ad", "m", "nd")
STATIC_model_type <- "ANFIS"
STATIC_number_of_rep <- 30
STATIC_min_threshold <- 0.001

getLocalMF <- function(x) {
	if (x*STATIC_MF>1) {
		result <- 1
	} else if (x*STATIC_MF<0) {
		result <- 0
	} else {
		result <- x*STATIC_MF
	}
 
	return(result)
}

error_array <- matrix(data = 0.0, nrow = length(diemdo), ncol = 1, byrow = FALSE, dimnames = NULL)

for (diem in diemdo) {
	sub_df_diem <- subset(df_postgres, df_postgres[,12] == diem, select = STATIC_selected_cols)
	sub_df_predict <- subset(df_predict, df_predict[,12] == diem, select = STATIC_selected_cols)
	
	print(nrow(sub_df_diem))
 
	#
	predict_month <- sub_df_diem[,10]
	predict_year <- sub_df_diem[,11]
	
	#
	predict_month_append <- sub_df_predict[,10]
	predict_year_append <- sub_df_predict[,11]
	 
	#chi cluster DI -> ND
	df_training <- sub_df_diem[2:9]
	df_training_rows <- nrow(df_training)
	df_training_cols <- ncol(df_training)

	for (number_cluster in 5:10){
		#gom nhom cluster bang C-Means
		cmeans_result<-cmeans(df_training,number_cluster,200,verbose=TRUE,method="cmeans",m=2)
		#print(cmeans_result)
	   
		#init min/max vertex by each cluster center
		min_vertexes <- cmeans_result$centers
		max_vertexes <- cmeans_result$centers
	  
		#find min/max vertexes
		for (i in 1:df_training_rows){
			#print(cmeans_result$cluster[i])
			cluster_row <- cmeans_result$cluster[i]
			for (j in 1:df_training_cols){
				#print(min_vertexes[i,j])
				# re-assign if the record of training < min_vertex
				if(df_training[i,j] < min_vertexes[cluster_row,j]){
					min_vertexes[cluster_row,j] <- df_training[i,j]
				}
		
				# re-assign if the record of training > min_vertex
				if(df_training[i,j] > max_vertexes[cluster_row,j]){
					max_vertexes[cluster_row,j] <- df_training[i,j]
				}
			}
		}
	  
		#if(FALSE){
			#init distance matrix
			distance_matrix <- matrix(data = 0.0, nrow = df_training_rows, ncol = number_cluster+1, byrow = FALSE, dimnames = NULL)
		  
			#loop n rows
			for (i in 1:df_training_rows){
			#loop n cluster
				for (j in 1:number_cluster){
					#tinh khoang cach moi vector input den tung cluster bang cach SUM tung attribute
					#distance <- 0
					for (k in 1:df_training_cols){
						distance_matrix[i,j+1] <- distance_matrix[i,j+1] + 1 - getLocalMF(df_training[i,k]-max_vertexes[j,k]) - getLocalMF(min_vertexes[j,k] - df_training[i,k])
					}
			 
					#k phan ra pHB, thay vao do, moi vector se dc simplify ve value 0-1
					distance_matrix[i,j+1] <- distance_matrix[i,j+1]/df_training_cols
				}
			}
			
			predict_matrix <- sub_df_predict[2:9]
			predict_matrix_rows <- nrow(predict_matrix)
			predict_matrix_cols <- ncol(predict_matrix)
			
			#init distance matrix
			predict_distance_matrix <- matrix(data = 0.0, nrow = predict_matrix_rows, ncol = number_cluster+1, byrow = FALSE, dimnames = NULL)
		  
			#loop n rows
			for (i in 1:predict_matrix_rows){
			#loop n cluster
				for (j in 1:number_cluster){
					#tinh khoang cach moi vector input den tung cluster bang cach SUM tung attribute
					#distance <- 0
					for (k in 1:predict_matrix_cols){
						predict_distance_matrix[i,j+1] <- predict_distance_matrix[i,j+1] + 1 - getLocalMF(predict_matrix[i,k]-max_vertexes[j,k]) - getLocalMF(min_vertexes[j,k] - predict_matrix[i,k])
					}
			 
					#k phan ra pHB, thay vao do, moi vector se dc simplify ve value 0-1
					predict_distance_matrix[i,j+1] <- predict_distance_matrix[i,j+1]/predict_matrix_cols
				}
			}
		   
			#xac dinh max & min somac de scale
			max_somac <- max(sub_df_diem$somac)
			min_somac <- min(sub_df_diem$somac)
		   
			somac_col <- sub_df_diem[,1]
			somac_scaled <- scale(somac_col, center = min_somac, scale = max_somac - min_somac)
		   
			distance_matrix[,1] <- somac_scaled[,1]
			training_matrix <- as.data.frame(distance_matrix)
		   
			for (i in 1:ncol(training_matrix)){
				if(i == 1){
					colnames(training_matrix)[i] <- "somac"
				} else {
					colnames(training_matrix)[i] <- paste("dist", toString(i), sep="_")
				}
			}
			
			predict_distance_matrix <- as.data.frame(predict_distance_matrix)
		   
			for (i in 1:ncol(predict_distance_matrix)){
				if(i == 1){
					colnames(predict_distance_matrix)[i] <- "somac"
				} else {
					colnames(predict_distance_matrix)[i] <- paste("dist", toString(i), sep="_")
				}
			}
		   
			#print(head(training_matrix))
		   
			#calculate model
			n <- names(training_matrix)
			f <- as.formula(paste("somac ~", paste(n[!n %in% "somac"], collapse = " + ")))
			number_of_nodes <- floor(ncol(training_matrix) / 2)
			nn <- neuralnet(f, data=training_matrix, hidden=c(number_of_nodes), rep=STATIC_number_of_rep, threshold = STATIC_min_threshold)
			#nn <- neuralnet(f, data=training_matrix, hidden=c(number_of_nodes))
		  
			#tinh toan cac model va lua ra model co MSE nho nhat
			min_rep <- which.min(nn$result.matrix["error",])
		   
			#calculate predict data from df_postgres
			predict.nn <- compute(nn, training_matrix[,2:length(training_matrix)], rep = min_rep)
			predict.nn_ <- predict.nn$net.result * (max_somac - min_somac) + min_somac
			
			#calculate predict data from df_predict
			predict.predict_nn <- compute(nn, predict_distance_matrix[,2:length(predict_distance_matrix)], rep = min_rep)
			predict.predict_nn_ <- predict.predict_nn$net.result * (max_somac - min_somac) + min_somac
		   
			#print(head(predict.nn_))
		   
		   
			mean_error <- 0
			#tinh toan sai so
			for (i in 1:length(somac_col)){
				mean_error <- mean_error + abs(predict.nn_[i] - somac_col[i])
			}
			print(paste0("Sai so cua huyen ", diem," theo ", number_cluster," cluseter la ", mean_error))
		   
			#if (number_cluster == 2) {
				error_array[diem-10] <- mean_error
				best_predict <- predict.nn_
				best_cluster <- number_cluster
				best_min_rep <- min_rep
				best_nn <- nn
				best_training <- predict_distance_matrix
				best_predict_append <- predict.predict_nn_
			#} else if (error_array[diem-10] > mean_error) {
			#	error_array[diem-10] <- mean_error
			#	best_predict <- predict.nn_
			#	best_cluster <- number_cluster
			#	best_min_rep <- min_rep
			#	best_nn <- nn
			#	best_training <- predict_distance_matrix
			#	best_predict_append <- predict.predict_nn_
			#}
		#}
	}
	
	#save into DB the best predict of ANFIS theo tung huyen
	#con <<- getDB()
	for (i in 1:nrow(sub_df_diem)){
		if(best_predict[i] < 0){
			predict_rounded <- 0
		} else {
			predict_rounded <- round(best_predict[i])
		}
		saveOrUpdatePrediction(predict_month[i], predict_year[i], diem, STATIC_model_type, predict_rounded)
	}
	
	#save into DB the best predict of ANFIS theo tung huyen
	for (i in 1:nrow(best_predict_append)){
		if(best_predict_append[i] < 0){
			predict_rounded <- 0
		} else {
			predict_rounded <- round(best_predict_append[i])
		}
		
		saveOrUpdatePrediction(predict_month_append[i], predict_year_append[i], diem, STATIC_model_type, predict_rounded)
	}
}
