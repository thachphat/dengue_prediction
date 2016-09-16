source("db.R")

saveOrUpdateCorrelationResult <- function(factor1_name, factor2_name, year, location, method, correlation) {
	#re-assign to 0 if NA
	ifelse(is.na(correlation), correlation <- 0, correlation <- round(correlation, digits = 2))
	
	print(paste("Correlation of",factor1_name,"and",factor2_name,"based of huyen",location,"in year",year,"on method",method,"is:",correlation))

	existsSQL <- sprintf("SELECT EXISTS(SELECT 1 FROM ytdp_correlation_result WHERE nhan_to_1 = '%s' AND nhan_to_2 = '%s' AND nam = %d AND diemdo = %d AND phuong_phap = '%s')", factor1_name, factor2_name, year, location, method)
	insertSQL <- sprintf("INSERT INTO ytdp_correlation_result (nhan_to_1, nhan_to_2, nam, diemdo, phuong_phap, giatri) VALUES ('%s', '%s', %d, %d, '%s', %e);", factor1_name, factor2_name, year, location, method, correlation)
	updateSQL <- sprintf("UPDATE ytdp_correlation_result SET giatri = %e WHERE nhan_to_1 = '%s' AND nhan_to_2 = '%s' AND nam = %d AND diemdo = %d AND phuong_phap = '%s';", correlation, factor1_name, factor2_name, year, location, method)
	
	#save or update correlations
	saveOrUpdate(existsSQL, insertSQL, updateSQL)	
}

calculateCorrelations <- function(factor1_name, factor2_name, factor1_value, factor2_value, location, year) {
	correlation <- cor(factor1_value, factor2_value)
	saveOrUpdateCorrelationResult(factor1_name, factor2_name, year, location, "Pearson", correlation)
	
	correlation <- cor(factor1_value, factor2_value, method = "spearman")
	saveOrUpdateCorrelationResult(factor1_name, factor2_name, year, location, "Spearman", correlation)
	
	correlation <- cor(factor1_value, factor2_value, method = "kendall")
	saveOrUpdateCorrelationResult(factor1_name, factor2_name, year, location, "Kendall", correlation)	
}

processCorrelation <- function(data_frame, diemdo_id, year) {
	calculateCorrelations("SM", "DI", data_frame$somac, data_frame$di, diemdo_id, year)
	calculateCorrelations("SM", "BI", data_frame$somac, data_frame$bi, diemdo_id, year)
	calculateCorrelations("SM", "HIM", data_frame$somac, data_frame$him, diemdo_id, year)
	calculateCorrelations("SM", "HIL", data_frame$somac, data_frame$hil, diemdo_id, year)
	calculateCorrelations("SM", "CI", data_frame$somac, data_frame$ci, diemdo_id, year)
	calculateCorrelations("SM", "AD", data_frame$somac, data_frame$ad, diemdo_id, year)
	calculateCorrelations("SM", "M", data_frame$somac, data_frame$m, diemdo_id, year)
	calculateCorrelations("SM", "ND", data_frame$somac, data_frame$nd, diemdo_id, year)

	calculateCorrelations("DI", "BI", data_frame$di, data_frame$bi, diemdo_id, year)
	calculateCorrelations("DI", "HIM", data_frame$di, data_frame$him, diemdo_id, year)
	calculateCorrelations("DI", "HIL", data_frame$di, data_frame$hil, diemdo_id, year)
	calculateCorrelations("DI", "CI", data_frame$di, data_frame$ci, diemdo_id, year)
	calculateCorrelations("DI", "AD", data_frame$di, data_frame$ad, diemdo_id, year)
	calculateCorrelations("DI", "M", data_frame$di, data_frame$m, diemdo_id, year)
	calculateCorrelations("DI", "ND", data_frame$di, data_frame$nd, diemdo_id, year)

	calculateCorrelations("BI", "HIM", data_frame$bi, data_frame$him, diemdo_id, year)
	calculateCorrelations("BI", "HIL", data_frame$bi, data_frame$hil, diemdo_id, year)
	calculateCorrelations("BI", "CI", data_frame$bi, data_frame$ci, diemdo_id, year)
	calculateCorrelations("BI", "AD", data_frame$bi, data_frame$ad, diemdo_id, year)
	calculateCorrelations("BI", "M", data_frame$bi, data_frame$m, diemdo_id, year)
	calculateCorrelations("BI", "ND", data_frame$bi, data_frame$nd, diemdo_id, year)

	calculateCorrelations("HIM", "HIL", data_frame$him, data_frame$hil, diemdo_id, year)
	calculateCorrelations("HIM", "CI", data_frame$him, data_frame$ci, diemdo_id, year)
	calculateCorrelations("HIM", "AD", data_frame$him, data_frame$ad, diemdo_id, year)
	calculateCorrelations("HIM", "M", data_frame$him, data_frame$m, diemdo_id, year)
	calculateCorrelations("HIM", "ND", data_frame$him, data_frame$nd, diemdo_id, year)

	calculateCorrelations("HIL", "CI", data_frame$hil, data_frame$ci, diemdo_id, year)
	calculateCorrelations("HIL", "AD", data_frame$hil, data_frame$ad, diemdo_id, year)
	calculateCorrelations("HIL", "M", data_frame$hil, data_frame$m, diemdo_id, year)
	calculateCorrelations("HIL", "ND", data_frame$hil, data_frame$nd, diemdo_id, year)

	calculateCorrelations("CI", "AD", data_frame$ci, data_frame$ad, diemdo_id, year)
	calculateCorrelations("CI", "M", data_frame$ci, data_frame$m, diemdo_id, year)
	calculateCorrelations("CI", "ND", data_frame$ci, data_frame$nd, diemdo_id, year)

	calculateCorrelations("AD", "M", data_frame$ad, data_frame$m, diemdo_id, year)
	calculateCorrelations("AD", "ND", data_frame$ad, data_frame$nd, diemdo_id, year)

	calculateCorrelations("M", "ND", data_frame$m, data_frame$nd, diemdo_id, year)
}


#main
for (diem in diemdo) {
	df_subset <- subset(df_postgres, diemdo==diem)
	
	#calculate correlation for all years
	processCorrelation(df_subset, diem, 0)
	
	#calculate correlation for each year	
	for (n in nam) {
		df_subset1 <- subset(df_subset, nam == n)
		processCorrelation(df_subset1, diem, n)
	}
}
