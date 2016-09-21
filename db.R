#install.packages("RPostgreSQL")
require("RPostgreSQL")

getDB <- function() {
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = "yteduphong",
                     host = "localhost", port = 5432,
                     user = "postgres", pass = "PG@2014")
}

getData <- function() {
    con <<- getDB()
    df_postgres <- dbGetQuery(con, "select sm.giatri as SOMAC, mdmc.giatri as DI, ncmc.giatri as HIM, dclq.giatri as BI, nclq.giatri as HIL, tlnlq.giatri as CI, tt.giatri as AD, m.giatri as M, nd.giatri as ND, sm.thang, sm.nam, sm.diemdo, dp.name as HUYEN from ytdp_somacchet sm, ytdp_matdomuoicai mdmc, ytdp_nhacomuoicai ncmc, ytdp_dungculangquang dclq, ytdp_nhacolangquang nclq, ytdp_tilenuoclangquang tlnlq, ytdp_thoitiet tt, ytdp_mua m, ytdp_nhietdo nd, ytdp_diaphuong dp where sm.thang = mdmc.thang AND sm.nam = mdmc.nam AND sm.diemdo = mdmc.diemdo AND sm.thang = ncmc.thang AND sm.nam = ncmc.nam AND sm.diemdo = ncmc.diemdo AND sm.thang = dclq.thang AND sm.nam = dclq.nam AND sm.diemdo = dclq.diemdo AND sm.thang = nclq.thang AND sm.nam = nclq.nam AND sm.diemdo = nclq.diemdo AND sm.thang = tlnlq.thang AND sm.nam = tlnlq.nam AND sm.diemdo = tlnlq.diemdo AND sm.thang = tt.thang AND sm.nam = tt.nam AND sm.diemdo = tt.diemdo AND sm.thang = m.thang AND sm.nam = m.nam AND sm.diemdo = m.diemdo AND sm.thang = nd.thang AND sm.nam = nd.nam AND sm.diemdo = nd.diemdo AND sm.diemdo = dp.id AND sm.name = 'SM' AND mdmc.name = 'DI' AND ncmc.name = 'HIM' AND dclq.name = 'BI' AND nclq.name = 'HIL' AND tlnlq.name = 'CI' AND tt.name = 'AD' AND m.name = 'M' AND nd.name = 'ND' order by sm.diemdo, sm.nam desc, sm.thang desc")    
    return(df_postgres)
}

init <- function() {
	df_postgres <<- getData()
	#convert NA to 0
	df_postgres[is.na(df_postgres)] <<- 0
	thang <<- unique(df_postgres$thang)
	nam <<- unique(df_postgres$nam)
	diemdo <<- unique(df_postgres$diemdo)
	number_of_gap_years <<- 1
	predictDataForNextYear()
}

saveOrUpdate  <- function(existsSQL, insertSQL, updateSQL) {
	#check if's existed
	isExisted <- dbGetQuery(con, existsSQL)

	if(isExisted == "FALSE"){
		print("going to INSERT")
		#not existed then INSERT NEW record
		rs <- dbSendQuery(con, insertSQL)
		dbClearResult(rs)
	} else {
		#if it's existed then UPDATE NEW value
		print("going to UPDATE")
		rs <- dbSendQuery(con, updateSQL)
		dbClearResult(rs)
	}
}

getMeanError <- function(model_type, diemdo){
	calculateMeanErrorSQL <- sprintf("select abs(p.sm_du_bao - s.giatri) as saiso, p.thang, p.nam from ytdp_prediction p, ytdp_somacchet s where p.phuong_phap = '%s' and p.diemdo = %d and s.diemdo = p.diemdo and s.name = 'SM' and p.nam=s.nam and p.thang = s.thang order by p.diemdo, p.nam, p.thang", model_type, diemdo)
	meanError <- dbGetQuery(con, calculateMeanErrorSQL)    
    return(meanError)
}

getSumMeanError <- function(model_type){
	calculateMeanErrorSQL <- sprintf("select sum(abs(p.sm_du_bao - s.giatri)) as tongsaiso, p.diemdo from ytdp_prediction p, ytdp_somacchet s where p.phuong_phap = '%s' and p.diemdo BETWEEN 11 and 20 and s.diemdo = p.diemdo and s.name = 'SM' and p.nam=s.nam and p.thang = s.thang group by p.diemdo", model_type)
	meanError <- dbGetQuery(con, calculateMeanErrorSQL)    
    return(meanError)
}

saveOrUpdatePrediction  <- function(predict_month, predict_year, location, model_type, predict) {
	existsSQL <- sprintf("SELECT EXISTS(SELECT 1 FROM ytdp_prediction WHERE thang = %d AND nam = %d AND diemdo = %d AND phuong_phap = '%s')", predict_month, predict_year, location, model_type)
	insertSQL <- sprintf("INSERT INTO ytdp_prediction (thang, nam, diemdo, phuong_phap, sm_du_bao) VALUES (%d, %d, %d, '%s', %e);", predict_month, predict_year, location, model_type, predict)
	updateSQL <- sprintf("UPDATE ytdp_prediction SET sm_du_bao = %e WHERE thang = %d AND nam =  %d AND diemdo = %d AND phuong_phap = '%s';", predict, predict_month, predict_year, location, model_type)
	#save or update predicted values
	saveOrUpdate(existsSQL, insertSQL, updateSQL)
}

saveOrUpdateRegressionModel  <- function(factor1_name, factor2_name, location, model, model_type) {
	existsSQL <- sprintf("SELECT EXISTS(SELECT 1 FROM ytdp_regression WHERE nhan_to_1 = '%s' AND nhan_to_2 = '%s' AND diemdo = %d AND phuong_phap = '%s')", factor1_name, factor2_name, location, model_type)
	insertSQL <- sprintf("INSERT INTO ytdp_regression (nhan_to_1, nhan_to_2, diemdo, mo_hinh, phuong_phap) VALUES ('%s', '%s', %d, '%s', '%s');", factor1_name, factor2_name, location, model, model_type)
	updateSQL <- sprintf("UPDATE ytdp_regression SET mo_hinh = '%s' WHERE nhan_to_1 = '%s' AND nhan_to_2 = '%s' AND diemdo = %d AND phuong_phap = '%s';", model, factor1_name, factor2_name, location, model_type)
	#save or update prediction model
	saveOrUpdate(existsSQL, insertSQL, updateSQL)
}

predictDataForNextYear <- function() {	
	# predict values and add to df_predict
	for (diem in diemdo) {
		#subset theo diem do, start o thang dau tien nam dau tien
		sub_df_diem <- subset(df_postgres, diemdo == diem)
		sub_df_diem <- sub_df_diem[nrow(sub_df_diem):1,]
		first_row <- head(sub_df_diem, n=1)
		first_month <- first_row$thang
		first_year <- first_row$nam
		last_row <- tail(sub_df_diem, n=12)
		months <- last_row$thang
		years <- last_row$nam
			
		concat_df <- last_row
		concat_df$nam <- concat_df$nam + 1
		# tim tat ca cac name, predict tat ca tru somac, diemdo, thang, nam, huyen
		sub_df_diem <- subset(sub_df_diem, select=-c(somac,diemdo,thang,nam,huyen))
		
		for(name in names(sub_df_diem)) {
			a <- ts(sub_df_diem[,c(name)],frequency=12,start=c(first_year,first_month))
	
			#chua ro input dau vao
			fit <- arima(a, order=c(1,0,0), list(order=c(2,1,0), period=12), method="CSS")
	
			#predict
			fore <- predict(fit, n.ahead=12)
			concat_df[[name]] <- as.numeric(fore$pred)
	
			#print predict data and save to db
			#for(i in 1:length(fore$pred)){
				#get month and year for prediction
			#	predict_month <- months[i]
			#	predict_year <- years[i] + 1
			#	predict <- fore$pred[i]
			#	if(is.na(predict)||predict < 0){
			#		predict <- 0
			#	}
				
			#	print(paste0("Predict value of ", predict_month, "/", predict_year, " is ", predict))
			#	table_names <- list("di"="ytdp_matdomuoicai", "him"="ytdp_nhacomuoicai", "bi"="ytdp_dungculangquang", "hil"="ytdp_nhacolangquang", "ci"="ytdp_tilenuoclangquang", "ad"="ytdp_thoitiet", "m"="ytdp_mua", "nd"="ytdp_nhietdo")
		
			#	existsSQL <- sprintf("SELECT EXISTS(SELECT 1 FROM %s WHERE thang = %d AND nam = %d AND diemdo = %d)", table_names[[name]], predict_month, predict_year, diem)
			#	insertSQL <- sprintf("INSERT INTO %s (thang, nam, diemdo, giatri, name) VALUES (%d, %d, %d, %e, '%s');", table_names[[name]], predict_month, predict_year, diem, predict, toupper(name))
			#	updateSQL <- sprintf("UPDATE %s SET giatri = %e WHERE thang = %d AND nam = %d AND diemdo = %d;", table_names[[name]], predict, predict_month, predict_year, diem)

				#save or update predicted values
			#	saveOrUpdate(existsSQL, insertSQL, updateSQL)
			#}
		}
		
		# merge df
		if (!exists("df_predict")) {
			df_predict <- concat_df
		} else {
			df_predict <- merge(df_predict, concat_df, all=TRUE)
		}
	}
	
	#sort df
	df_predict <<- df_predict[order(df_predict$diemdo,df_predict$thang),]
}

init()