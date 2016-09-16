source("db.R")

saveOrUpdateSimpleRegression <- function(factor1_name, factor2_name, factor1_value, factor2_value, location, real_data_frame, predicted_data_frame, column_number){
	soMac <- real_data_frame[,1]
	factor <- real_data_frame[,column_number]
	
	lm_rs <- lm(soMac ~ factor)
	co <- coef(summary(lm_rs))

	weight <- round(co[2,1], digits = 2)
	intercept <- round(co[1,1], digits = 2)
	std_error <- round(co[2,2], digits = 2)
	print(paste0(factor1_name, " = ",factor2_name,"*", weight, " + ", intercept, " with standard error is ", std_error))

	model_type <- paste0("SIMPLE_REGRESSION_BASED_ON_", factor2_name)
	model <- paste0(factor1_name, " = ",factor2_name,"*", weight, " + ", intercept)

	saveOrUpdateRegressionModel(factor1_name, factor2_name, location, model, model_type)

	#predicting....
	if(location > 1) {
		#calculate the predict data with the real data frame

	 	df_input_predict <- subset(real_data_frame, select = c(column_number))
	 	df_output_predict <- predict(lm_rs, newdata = df_input_predict)
		saveOrUpdateLinearRegressionPrediction(real_data_frame, df_output_predict, location, model_type)
	
		months <- predicted_data_frame$thang
		years <- predicted_data_frame$nam

		sub_df_predict <- subset(predicted_data_frame, select = c(column_number))
		#df_output_predict <- predict(lm_rs, newdata = b)
		#saveOrUpdateLinearRegressionPrediction(predicted_data_frame, df_output_predict, location)
	
		
		df_input_predict <- as.matrix(sub_df_predict[1])
		
		for (i in 1:length(df_input_predict)) {
			predict_month <- months[i]
			predict_year <- years[i]
		
			model <- paste0(factor1_name, " = ",weight,"*", df_input_predict[i], " + ", intercept)
			#print(model)
			predict <- round(df_input_predict[i]*weight + intercept)
			
			if(is.na(predict)||predict < 0){
				predict <- 0
			}
			
			predict <- round(predict)

			print(paste0("Predict value of ", predict_month, "/", predict_year, " is ", round(predict), " with ", df_input_predict[i]))
			saveOrUpdatePrediction(predict_month, predict_year, location, model_type, predict)
		}
		
		if(FALSE){

		#for(u in df_predict) {
		for(u in 1:length(sub_df_predict)) {
			predict_month <- months[u]
			predict_year <- years[u]
		
			model <- paste0(factor1_name, " = ",weight,"*", sub_df_predict[u], " + ", intercept)
			#print(model)
			predict <- round(sub_df_predict[u]*weight + intercept)
			
			if(is.na(predict)||predict < 0){
				predict <- 0
			}
			
			predict <- round(predict)

			print(paste0("Predict value of ", predict_month, "/", predict_year, " is ", round(predict), "with ", sub_df_predict[u]))

			#sub_df_predict[u]
			for (i in as.matrix(sub_df_predict[u])) {
				str(i)
			}

			#saveOrUpdatePrediction(predict_month, predict_year, location, model_type, predict)
		}
		}

		#str(predicted_data_frame)
		#factor <- subset(predicted_data_frame, select = c(column_number))
		#factor <- predicted_data_frame[,column_number]
	 	#df_output_predict <- predict(lm_rs, newdata = factor)
	 	#str(df_output_predict)
		#saveOrUpdateLinearRegressionPrediction(predicted_data_frame, df_output_predict, location)
		
		if(FALSE) {
			df_input_predict <- subset(predicted_data_frame, select = c(column_number))
			df_output_predict <- predict(lm_rs, newdata = df_input_predict)
			saveOrUpdateLinearRegressionPrediction(predicted_data_frame, df_output_predict, location)
			
			df_predict <- predicted_data_frame[,column_number]
		
			months <- predicted_data_frame$thang
			years <- predicted_data_frame$nam
		
			#for(u in df_predict) {
			for(u in 1:length(df_predict)) {
				predict_month <- months[u]
				predict_year <- years[u]
			
				model <- paste0(factor1_name, " = ",weight,"*", df_predict[u], " + ", intercept)
				#print(model)
				predict <- round(df_predict[u]*weight + intercept)
				
				if(is.na(predict)||predict < 0){
					predict <- 0
				}
				
				predict <- round(predict)

				print(paste0("Predict value of ", predict_month, "/", predict_year, " is ", round(predict), "with ", df_predict[u]))

				#saveOrUpdatePrediction(predict_month, predict_year, location, model_type, predict)
			}
		}
	}
}

saveOrUpdatePolinomialRegression <- function(factor1_name, factor2_name, location, real_data_frame, predicted_data_frame, column_number){
	soMac <- real_data_frame[,1]
	thang <- real_data_frame[,10]
	nam <- real_data_frame[,11]

	var_factor <- real_data_frame[,column_number]

	lm_model <- lm(soMac ~ poly(var_factor, 2))
	lm_rs <- summary(lm_model)
	co <- coef(lm_rs)
	weight_values <- co[,1]

	model <- "SM = "
	for(i in length(weight_values):1) {
		if(i > 2) {
			if(i == length(weight_values)) {
				model <- paste0(model, factor2_name, "^", i-1, "*", round(weight_values[i], digits = 2))
			} else {
				model <- paste0(model, " + ", factor2_name, "^", i-1, "*", round(weight_values[i], digits = 2))
			}
		} else if (i > 1){
			model <- paste0(model, " + ", factor2_name, "*", round(weight_values[i], digits = 2))
		} else {
			model <- paste0(model, " + ", round(weight_values[i], digits = 2))
		}
	}
	
	r_squared <- round(lm_rs$r.squared, digits = 2)
	sigma <- round(lm_rs$sigma, digits = 2)

	model_type <- paste0("QUADRATIC_REGRESSION_BASED_ON_", factor2_name)
	string <- paste0(model, " R-squared ", r_squared, " and sigma ", sigma)
	
	#saveOrUpdateRegressionModel(factor1_name, factor2_name, location, model, model_type)
	#print(string)

	if(location > 1) {
	 	#calculate the predict data
	 	df_input_predict <- subset(real_data_frame, select = c(column_number))

	 	#get predict values
	 	df_output_predict <- predict(lm_model, newdata = df_input_predict)


	 	#df_input_predict <- subset(full_data_frame, select = -c(somac, thang, nam, diemdo, huyen))
		#df_output_predict <- predict(multi_model, newdata = df_input_predict)
		saveOrUpdateLinearRegressionPrediction(real_data_frame, df_output_predict, location, model_type)

	 	##########
	 	if(FALSE){
		 	for(i in length(sm_predict):1){
		 		predict_month <- thang[i]
				#predict_year <- nam[i] + number_of_gap_years
				predict_year <- nam[i]
		  
				predict <- round(sm_predict[i])
			  	if(is.na(predict)||predict < 0){
			  		predict <- 0
			  	}

				print(paste0("Predict value of ", predict_month, "/", predict_year, " of the model type ", model_type," is ", predict))

				#saveOrUpdatePrediction(predict_month, predict_year, location, model_type, predict)
		 	}
	 	}
	 	############
	}	
}

calculateSimpleRegression <- function(data_frame, predicted_data_frame, diemdo_id) {
	soMac <- data_frame[,1]
	di <- data_frame[,2]
	him <- data_frame[,3]
	bi <- data_frame[,4]
	hil <- data_frame[,5]
	ci <- data_frame[,6]
	ad <- data_frame[,7]
	m <- data_frame[,8]
	nd <- data_frame[,9]
	thang <- data_frame[,10]
	nam <- data_frame[,11]
	diemdo <- data_frame[,12]
	huyen <- data_frame[,13]

	#save simple linear regression
	saveOrUpdateSimpleRegression("SM", "DI", soMac, di, diemdo_id, data_frame, predicted_data_frame, 2)
	saveOrUpdateSimpleRegression("SM", "BI", soMac, bi, diemdo_id, data_frame, predicted_data_frame, 4)
	saveOrUpdateSimpleRegression("SM", "HIM", soMac, him, diemdo_id, data_frame, predicted_data_frame, 3)
	saveOrUpdateSimpleRegression("SM", "HIL", soMac, hil, diemdo_id, data_frame, predicted_data_frame, 5)
	saveOrUpdateSimpleRegression("SM", "CI", soMac, ci, diemdo_id, data_frame, predicted_data_frame, 6)
	saveOrUpdateSimpleRegression("SM", "AD", soMac, ad, diemdo_id, data_frame, predicted_data_frame, 7)
	saveOrUpdateSimpleRegression("SM", "M", soMac, m, diemdo_id, data_frame, predicted_data_frame, 8)
	saveOrUpdateSimpleRegression("SM", "ND", soMac, nd, diemdo_id, data_frame, predicted_data_frame, 9)
	
	#save polynomial regression
	if(FALSE){
		saveOrUpdatePolinomialRegression("SM", "DI", diemdo_id, data_frame, predicted_data_frame, 2)
		saveOrUpdatePolinomialRegression("SM", "BI", diemdo_id, data_frame, predicted_data_frame, 4)
		saveOrUpdatePolinomialRegression("SM", "HIM", diemdo_id, data_frame, predicted_data_frame, 3)
		saveOrUpdatePolinomialRegression("SM", "HIL", diemdo_id, data_frame, predicted_data_frame, 5)
		saveOrUpdatePolinomialRegression("SM", "CI", diemdo_id, data_frame, predicted_data_frame, 6)
		saveOrUpdatePolinomialRegression("SM", "AD", diemdo_id, data_frame, predicted_data_frame, 7)
		saveOrUpdatePolinomialRegression("SM", "M", diemdo_id, data_frame, predicted_data_frame, 8)
		saveOrUpdatePolinomialRegression("SM", "ND", diemdo_id, data_frame, predicted_data_frame, 9)
	}
}

calculateMultiRegression <- function(real_data_frame, predicted_data_frame, diemdo_id) {
	soMac <- real_data_frame[,1]
	di <- real_data_frame[,2]
	him <- real_data_frame[,3]
	bi <- real_data_frame[,4]
	hil <- real_data_frame[,5]
	ci <- real_data_frame[,6]
	ad <- real_data_frame[,7]
	m <- real_data_frame[,8]
	nd <- real_data_frame[,9]
	thang <- real_data_frame[,10]
	nam <- real_data_frame[,11]
	diemdo <- real_data_frame[,12]
	huyen <- real_data_frame[,13]
 
 	lm_rs <- lm(soMac ~ di + him + bi + hil + ci + ad + m + nd)
 	co <- coef(summary(lm_rs))
 
 	print(paste0("=============================MULTI REGRESSION MODEL OF HUYEN: ", diemdo_id))

 	weight_intercept <- round(co[1,1], digits = 2)
	weight_di <- round(co[2,1], digits = 2)
	weight_him <- round(co[3,1], digits = 2)
	weight_bi <- round(co[4,1], digits = 2)
	weight_hil <- round(co[5,1], digits = 2)
	weight_ci <- round(co[6,1], digits = 2)
	weight_ad <- round(co[7,1], digits = 2)
	weight_m <- round(co[8,1], digits = 2)
	weight_nd <- round(co[9,1], digits = 2)

 	model <- paste0("SM = ", weight_di, "*DI + ", weight_him, "*HIM +", weight_bi, "*BI + ", weight_hil, "*HIL + ", weight_ci, "*CI + ", weight_ad, "*AD + ", weight_m, "*M + ", weight_nd, "*ND + ", weight_intercept)
 	model_type <- "MULTI_REGRESSION"
 	print(model)

 	existsSQL <- sprintf("SELECT EXISTS(SELECT 1 FROM ytdp_multi_regression WHERE diemdo = %d)", diemdo_id)
	insertSQL <- sprintf("INSERT INTO ytdp_multi_regression (diemdo, mo_hinh, trong_so_di, trong_so_him, trong_so_bi, trong_so_hil, trong_so_ci, trong_so_ad, trong_so_m, trong_so_nd, trong_so_intercept) VALUES (%d, '%s', %e, %e, %e, %e, %e, %e, %e, %e, %e);", diemdo_id, model, weight_di, weight_him, weight_bi, weight_hil, weight_ci, weight_ad, weight_m, weight_nd, weight_intercept)
	updateSQL <- sprintf("UPDATE ytdp_multi_regression SET mo_hinh = '%s', trong_so_di = %e, trong_so_him = %e, trong_so_bi = %e, trong_so_hil = %e, trong_so_ci = %e, trong_so_ad = %e, trong_so_m = %e, trong_so_nd = %e, trong_so_intercept = %e WHERE diemdo = %d;", model, weight_di, weight_him, weight_bi, weight_hil, weight_ci, weight_ad, weight_m, weight_nd, weight_intercept, diemdo_id)
	#save or update prediction model
	saveOrUpdate(existsSQL, insertSQL, updateSQL)
 
 	if(diemdo_id > 1) {
 		#calculate the prediction with the real value
 		saveOrUpdateMultiRegressionPrediction(real_data_frame, lm_rs, diemdo_id, model_type)

 		#calculate the predict data with the predicted value
 		saveOrUpdateMultiRegressionPrediction(predicted_data_frame, lm_rs, diemdo_id, model_type) 	
  	}
}

saveOrUpdateMultiRegressionPrediction  <- function(full_data_frame, multi_model, diemdo_id, model_type) {
	df_input_predict <- subset(full_data_frame, select = -c(somac, thang, nam, diemdo, huyen))
	df_output_predict <- predict(multi_model, newdata = df_input_predict)
	saveOrUpdateLinearRegressionPrediction(full_data_frame, df_output_predict, diemdo_id, model_type)
}

saveOrUpdateLinearRegressionPrediction  <- function(full_data_frame, df_output_predict, diemdo_id, model_type) {
	months <- full_data_frame$thang
	years <- full_data_frame$nam

	for(i in 1:length(df_output_predict)){
	  	#get month and year for prediction
	  	predict_month <- months[i]
		#predict_year <- nam[i] + number_of_gap_years
		predict_year <- years[i]
	  
	  	predict <- round(df_output_predict[i])
	  	if(is.na(predict)||predict < 0){
	  		predict <- 0
	  	}
	  	print(paste0("Predict value of ", predict_month, "/", predict_year, " is ", predict, " ", model_type))

		saveOrUpdatePrediction(predict_month, predict_year, diemdo_id, model_type, predict)
  	}
}

#get unique diemdo from result set
unique_diemdo <- unique(diemdo)

#calculate simple regression for the whole province
calculateSimpleRegression(df_postgres, df_predict, 1)

#calculate multi regression for the whole province
calculateMultiRegression(df_postgres, df_predict, 1)

#calculate simple regression for each district of province
for(diemdo_id in unique_diemdo) {
	print(paste0("=============================MULTI REGRESSION MODEL OF HUYEN: ", diemdo_id))
	#calculate the subset with real data
	df_subset <- subset(df_postgres, df_postgres[,12]==diemdo_id)

	#calculate the subset with predicted data
	df_subset_predict <- subset(df_predict, df_predict[,12]==diemdo_id)
	
	#save the predtion with real data as input
	calculateSimpleRegression(df_subset, df_subset_predict, diemdo_id)
	calculateMultiRegression(df_subset, df_subset_predict, diemdo_id)
}
