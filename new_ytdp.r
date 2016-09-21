source("db.R")

model_type <- "SIMPLE_LINEAR_3_TO_1"
offset <- 3


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

saveOrUpdateMultiRegressionPrediction  <- function(full_data_frame, multi_model, diemdo_id, model_type) {
	df_input_predict <- subset(full_data_frame, select = -c(somac, thang, nam, diemdo, huyen))
	df_output_predict <- predict(multi_model, newdata = df_input_predict)
	saveOrUpdateLinearRegressionPrediction(full_data_frame, df_output_predict, diemdo_id, model_type)
}

saveOrUpdateLinearRegressionPrediction  <- function(full_data_frame, df_output_predict, diemdo_id, model_type) {
	#months <- full_data_frame$thang
	#years <- full_data_frame$nam

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

#calculate simple regression for each district of province
for(diemdo_id in unique_diemdo) {
	#diemdo_id <- 11
	print(paste0("=============================MULTI REGRESSION MODEL OF HUYEN: ", diemdo_id))
	
	
	
	#calculate the subset with real data
	df_subset <- subset(df_postgres, df_postgres[,12]==diemdo_id)
	
	# get data for prediction
	months <- df_subset$thang
	years <- df_subset$nam
	
	df_subset = createDataframe(df_subset)
	
	real_data_frame <- df_subset
	
	soMac <- real_data_frame[,1]
	soMac.1 <- real_data_frame[,2]
	di <- real_data_frame[,3]
	him <- real_data_frame[,4]
	bi <- real_data_frame[,5]
	hil <- real_data_frame[,6]
	ci <- real_data_frame[,7]
	ad <- real_data_frame[,8]
	m <- real_data_frame[,9]
	nd <- real_data_frame[,10]
	
	soMac.2 <- real_data_frame[,13]
	di.1 <- real_data_frame[,14]
	him.1 <- real_data_frame[,15]
	bi.1 <- real_data_frame[,16]
	hil.1 <- real_data_frame[,17]
	ci.1 <- real_data_frame[,18]
	ad.1 <- real_data_frame[,19]
	m.1 <- real_data_frame[,20]
	nd.1 <- real_data_frame[,21]
	
	soMac.3 <- real_data_frame[,24]
	di.2 <- real_data_frame[,25]
	him.2 <- real_data_frame[,26]
	bi.2 <- real_data_frame[,27]
	hil.2 <- real_data_frame[,28]
	ci.2 <- real_data_frame[,29]
	ad.2 <- real_data_frame[,30]
	m.2 <- real_data_frame[,31]
	nd.2 <- real_data_frame[,32]
	#thang <- real_data_frame[,10]
	#nam <- real_data_frame[,11]
	diemdo <- real_data_frame[,11]
	huyen <- real_data_frame[,13]
 
 	lm_rs <- lm(soMac ~ soMac.1 + di + him + bi + hil + ci + ad + m + nd + soMac.2 + di.1 + him.1 + bi.1 + hil.1 + ci.1 + ad.1 + m.1 + nd.1 + soMac.3 + di.2 + him.2 + bi.2 + hil.2 + ci.2 + ad.2 + m.2 + nd.2)
 	co <- coef(summary(lm_rs))
 
	print(co)
	
 	print(paste0("=============================MULTI REGRESSION MODEL OF HUYEN: ", diemdo_id))

 	weight_intercept <- round(co[1,1], digits = 2)
	
	weight_somac1 <- round(co[2,1], digits = 2)
	weight_di1 <- round(co[3,1], digits = 2)
	weight_him1 <- round(co[4,1], digits = 2)
	weight_bi1 <- round(co[5,1], digits = 2)
	weight_hil1 <- round(co[6,1], digits = 2)
	weight_ci1 <- round(co[7,1], digits = 2)
	weight_ad1 <- round(co[8,1], digits = 2)
	weight_m1 <- round(co[9,1], digits = 2)
	weight_nd1 <- round(co[10,1], digits = 2)
	
	weight_somac2 <- round(co[11,1], digits = 2)
	weight_di2 <- round(co[12,1], digits = 2)
	weight_him2<- round(co[13,1], digits = 2)
	weight_bi2 <- round(co[14,1], digits = 2)
	weight_hil2 <- round(co[15,1], digits = 2)
	weight_ci2 <- round(co[16,1], digits = 2)
	weight_ad2 <- round(co[17,1], digits = 2)
	weight_m2 <- round(co[18,1], digits = 2)
	weight_nd2 <- round(co[19,1], digits = 2)
	
	weight_somac3 <- round(co[20,1], digits = 2)
	weight_di3 <- round(co[21,1], digits = 2)
	weight_him3 <- round(co[22,1], digits = 2)
	weight_bi3 <- round(co[23,1], digits = 2)
	weight_hil3 <- round(co[24,1], digits = 2)
	weight_ci3 <- round(co[25,1], digits = 2)
	weight_ad3 <- round(co[26,1], digits = 2)
	weight_m3 <- round(co[27,1], digits = 2)
	weight_nd3 <- round(co[28,1], digits = 2)

 	model <- paste0("SM = ", weight_somac1, "*SM1 + ", weight_di1, "*DI1 + ", weight_him1, "*HIM1 +", weight_bi1, "*BI1 + ", weight_hil1, "*HIL1 + ", weight_ci1, "*CI1 + ", weight_ad1, "*AD1 + ", weight_m1, "*M1 + ", weight_nd1, "*ND1 + ")
	model <- paste0(model, weight_somac2, "*SM2 + ", weight_di2, "*DI2 + ", weight_him2, "*HIM2 +", weight_bi2, "*BI2 + ", weight_hil2, "*HIL2 + ", weight_ci2, "*CI2 + ", weight_ad2, "*AD2 + ", weight_m2, "*M2 + ", weight_nd2, "*ND2 + ")
	model <- paste0(model, weight_somac3, "*SM3 + ", weight_di3, "*DI3 + ", weight_him3, "*HIM3 +", weight_bi3, "*BI3 + ", weight_hil3, "*HIL3 + ", weight_ci3, "*CI3 + ", weight_ad3, "*AD3 + ", weight_m3, "*M3 + ", weight_nd3, "*ND3 + ", weight_intercept)
 	model_type <- "NEW_MULTI_REGRESSION"
 	print(model)

 	existsSQL <- sprintf("SELECT EXISTS(SELECT 1 FROM ytdp_new_multi_regression WHERE diemdo = %d)", diemdo_id)
	insertSQL <- sprintf("INSERT INTO ytdp_new_multi_regression (diemdo, mo_hinh) VALUES (%d, '%s');", diemdo_id, model)
	updateSQL <- sprintf("UPDATE ytdp_new_multi_regression SET mo_hinh = '%s' WHERE diemdo = %d;", model, diemdo_id)
	
	
	#save or update prediction model
	con <<- getDB()
	saveOrUpdate(existsSQL, insertSQL, updateSQL)
 
	predicted_data_frame <- createTestDataframe(subset(df_postgres, df_postgres[,12]==diemdo_id))
	
	df_input_predict <- subset(predicted_data_frame, select = -c(diemdo, huyen, diemdo.1, huyen.1, diemdo.2, huyen.2))
	
	colnames(df_input_predict)[1] <- "soMac.1"
	colnames(df_input_predict)[10] <- "soMac.2"
	colnames(df_input_predict)[19] <- "soMac.3"
	
	df_output_predict <- predict(lm_rs, newdata = df_input_predict)
 
	for(i in 1:length(df_output_predict)){
	  	#get month and year for prediction
	  	#get month and year for prediction
		predict_month <- months[i]
		predict_year <- years[i] + 1
	  
	  	predict <- round(df_output_predict[i])
	  	if(is.na(predict)||predict < 0){
	  		predict <- 0
	  	}
	  	print(paste0("Predict value of ", predict_month, "/", predict_year, " is ", predict, " ", model_type))

		saveOrUpdatePrediction(predict_month, predict_year, diemdo_id, model_type, predict)
  	}
 
}