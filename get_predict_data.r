source("db.R")

for (diem in diemdo) {
	#subset theo diem do, start o thang dau tien nam dau tien
	sub_df_diem <- subset(df_postgres, diemdo == diem, select = -c(huyen,diemdo,somac))
	sub_df_diem <- sub_df_diem[nrow(sub_df_diem):1,]
	first_row <- head(sub_df_diem, n=1)
	
	print("=======================================================")
	print(paste0("Predict for diemdo: ", diem))
	
	sub_df_predict <- subset(df_predict, diemdo == diem, select = -c(huyen,diemdo,somac))
	first_row_predict <- head(sub_df_predict, n=1)
	sub_df_predict <- subset(sub_df_predict, select = -c(thang,nam))
	
	png(paste0(diem,".png"))
	par(mfrow=c(4,2))

	for (name in names(sub_df_predict)) {
		real <- ts(sub_df_diem[,name],frequency=12,start=c(first_row$nam,first_row$thang))
		predict <- ts(sub_df_predict[,name],frequency=12,start=c(first_row_predict$nam,first_row_predict$thang))
		ts.plot(real, predict, col=c(1,2))
		title(main = paste("Predict for",name,"at diem do",diem))
	}	
	dev.off()
}