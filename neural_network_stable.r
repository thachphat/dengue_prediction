require("neuralnet")
source("db.R")

model_type <- "NEURAL_NETWORK"
number_of_rep <- 30
min_threshold <- 0.001
selected_cols <- c("somac", "di", "bi", "him", "m", "ci", "hil", "nd", "thang", "nam")

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
}

saveOrUpdateNeuralNetwork <- function() {
	#create table if not exist
	#dbGetQuery(con, "CREATE TABLE IF NOT EXISTS public.ytdp_neural_network(id integer NOT NULL DEFAULT nextval('ytdp_neural_network_id_seq'::regclass), tham_so character varying(256) NOT NULL, diemdo integer NOT NULL, node11 character varying(256), node12 character varying(256), node13 character varying(256), node14 character varying(256), node15 character varying(256), node21 character varying(256), CONSTRAINT ytdp_neural_network_pkey PRIMARY KEY (id), CONSTRAINT ytdp_neural_network_diemdo_fkey FOREIGN KEY (diemdo) REFERENCES public.ytdp_diaphuong (id) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE SET NULL)WITH (OIDS=FALSE); ALTER TABLE public.ytdp_neural_network OWNER TO postgres; COMMENT ON TABLE public.ytdp_neural_network IS 'ytdp.neural_network'; COMMENT ON COLUMN public.ytdp_neural_network.tham_so IS 'Tham so'; COMMENT ON COLUMN public.ytdp_neural_network.node11 IS 'Node 11'; COMMENT ON COLUMN public.ytdp_neural_network.node12 IS 'Node 12'; COMMENT ON COLUMN public.ytdp_neural_network.node13 IS 'Node 13'; COMMENT ON COLUMN public.ytdp_neural_network.node14 IS 'Node 14'; COMMENT ON COLUMN public.ytdp_neural_network.node15 IS 'Node 15'; COMMENT ON COLUMN public.ytdp_neural_network.node21 IS 'Node 21'; COMMENT ON COLUMN public.ytdp_neural_network.diemdo IS 'Điểm đo';")
	
	# generate tham so
	remove <- c("somac","thang","nam")
	vector_tham_so <- setdiff(selected_cols, remove)
	tham_so <- toString(vector_tham_so)
	
	#generate node data
	node11 <- toString(nn$weights[[min_rep]][[1]][,1])
	node12 <- toString(nn$weights[[min_rep]][[1]][,2])
	node13 <- toString(nn$weights[[min_rep]][[1]][,3])
	node14 <- toString(nn$weights[[min_rep]][[1]][,4])
	node15 <- toString(nn$weights[[min_rep]][[1]][,5])
	node21 <- toString(nn$weights[[min_rep]][[2]][,1])
	
	#save to db	
	existsSQL <- sprintf("SELECT EXISTS(SELECT 1 FROM ytdp_neural_network WHERE diemdo = %d)", diem)
	insertSQL <- sprintf("INSERT INTO ytdp_neural_network (tham_so, diemdo, node11, node12, node13, node14, node15, node21) VALUES ('%s', %d, '%s', '%s', '%s', '%s', '%s', '%s');", tham_so, diem, node11, node12, node13, node14, node15, node21)
	updateSQL <- sprintf("UPDATE ytdp_neural_network SET tham_so = '%s', node11 = '%s', node12 = '%s', node13 = '%s', node14 = '%s', node15 = '%s', node21 = '%s' WHERE diemdo = %d;", tham_so, node11, node12, node13, node14, node15, node21, diem)
	#save or update predicted values
	saveOrUpdate(existsSQL, insertSQL, updateSQL)
}

# loop through diemdo, subset diemdo, make correlation for each nam
for (diem in diemdo) {

	#skip good model
	good_model_diemdo <- c(12,13,15,16,17,20)
	if (diem %in% good_model_diemdo) {
		next
	}

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
	number_of_nodes <- floor(length(selected_cols) / 2)
	nn <- neuralnet(f, data=scaled, hidden=c(number_of_nodes), rep=number_of_rep, threshold = min_threshold)
	
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
	sub_df_diem <- subset(df_predict, diemdo == diem, select = selected_cols)
	sub_df_diem <- sub_df_diem[nrow(sub_df_diem):1,]
	months <- sub_df_diem$thang
	years <- sub_df_diem$nam
	sub_df_diem <- subset(sub_df_diem, select = -c(nam,thang))
	
	scaled <- as.data.frame(scale(sub_df_diem, center = mins, scale = maxs - mins))
	predict.nn <- compute(nn, scaled[,2:length(sub_df_diem)], rep = min_rep)
	predict.nn_ <- predict.nn$net.result * (max_somac - min_somac) + min_somac
	calculatePredictData()
	plot_data <- append(predict.nn_[,1],plot_data)
	
	print("======================================")
	
	# plot nn 12/2014 -> 1/2008
	png(paste0(diem,".png"))
	plot_data <- rev(plot_data)
	plot(plot_data, type='l', col='red')
	real_data <- rev(subset(df_postgres, diemdo==diem)$somac)
	lines(real_data, col='blue')
	dev.off()
	
	#save model to DB
	saveOrUpdateNeuralNetwork()
}
