df_twt <- data.frame(matrix(ncol = 23, nrow = 45))
df_mi <- data.frame(matrix(ncol = 23, nrow = 45))

for(t in 1:23){
  
  #t <- 1
  i <- m[t]
  
  datMat <- datTweets %>% filter(YM==i) %>%
    select(user_id_str, VL_ID.area) %>%
    group_by(user_id_str, VL_ID.area) %>% 
    summarise(n_tweets=n())
  
  datMat_wide <- spread(datMat, VL_ID.area, n_tweets)
  datMat_wide <- as.data.frame(datMat_wide)
  row.names(datMat_wide) <- datMat_wide[,1]
  datMat_wide <- datMat_wide[,c(2:46)]
  datMat_wide[is.na(datMat_wide)] <- 0
  datMat_wide[,][datMat_wide[,] != 0] <- 1
  
  aff.df <- as.matrix(datMat_wide)
  mtx_area <-  t(aff.df) %*% aff.df
  diag(mtx_area) <- 0
  
  mtx_area <- mtx_area * (1/sum(mtx_area))
  
  twtM <- matrix(rep(1,45), nrow=45, ncol=1, byrow=TRUE)
  twtM_twt <- mtx_area %*% twtM
  df_twt[t] <- twtM_twt
  
  denM <- as.matrix(select(datDen, i))
  denM_air <- mtx_area %*% denM
  df_mi[t] <- denM_air
  
}