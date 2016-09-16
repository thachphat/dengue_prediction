source("db.R")

model_type <- "TIME_SERIES"

for (diem in diemdo) {
	#subset theo diem do, start o thang dau tien nam dau tien
	sub_df_diem <- subset(df_postgres, diemdo == diem)
	sub_df_diem <- sub_df_diem[nrow(sub_df_diem):1,]
	first_row <- head(sub_df_diem, n=1)
	a <- ts(sub_df_diem$somac,frequency=12,start=c(first_row$nam,first_row$thang))
	
	#chua ro input dau vao
	fit <- arima(a, order=c(1,0,0), list(order=c(2,1,0), period=12))
	
	#predict
	fore <- predict(fit, n.ahead=12)
	print("=======================================================")
	print(paste0("Predict for diemdo: ", diem))
	
	# get data for prediction
	last_row <- tail(sub_df_diem, n=12)
	months <- last_row$thang
	years <- last_row$nam
	
	#print predict data and save to db
	for(i in 1:length(fore$pred)){
		#get month and year for prediction
		predict_month <- months[i]
		predict_year <- years[i] + 1
		predict <- floor(fore$pred[i])
		if(is.na(predict)||predict < 0){
			predict <- 0
		}
		
		standard_error <- floor(fore$se[i])
		if(is.na(standard_error)||standard_error < 0) {
			standard_error <- 0
		}
		print(paste0("Predict value of ", predict_month, "/", predict_year, " is ", predict, " - ", standard_error))
		
		existsSQL <- sprintf("SELECT EXISTS(SELECT 1 FROM ytdp_prediction WHERE thang = %d AND nam = %d AND diemdo = %d AND phuong_phap = '%s')", predict_month, predict_year, diem, model_type)
		insertSQL <- sprintf("INSERT INTO ytdp_prediction (thang, nam, diemdo, phuong_phap, sm_du_bao, sai_so) VALUES (%d, %d, %d, '%s', %e, %e);", predict_month, predict_year, diem, model_type, predict, standard_error)
		updateSQL <- sprintf("UPDATE ytdp_prediction SET sm_du_bao = %e WHERE thang = %d AND nam = %d AND diemdo = %d AND phuong_phap = '%s';", predict, predict_month, predict_year, diem, model_type)
		#save or update predicted values
		saveOrUpdate(existsSQL, insertSQL, updateSQL)
	}
	
	#code for plotting
	#U <- fore$pred + 2*fore$se
	#L <- fore$pred - 2*fore$se
	#ts.plot(a, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
	#legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"),col=c(1,2,4), lty=c(1,1,2))
}