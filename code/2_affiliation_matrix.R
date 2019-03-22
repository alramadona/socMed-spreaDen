for(t in 1:length(m)){
  #t <- 1
  i <- m[t]
  
  datMat <- dataset %>% 
    filter(YM==i) %>%
    select(user_id_str, VL_ID.area) %>%
    group_by(user_id_str, VL_ID.area) %>% 
    summarise(
      n_tweets=n())
  
  datMat_wide <- spread(datMat, VL_ID.area, n_tweets)
  datMat_wide <- as.data.frame(datMat_wide)
  row.names(datMat_wide) <- datMat_wide[,1]
  datMat_wide <- datMat_wide[,c(2:46)]
  datMat_wide[is.na(datMat_wide)] <- 0
  datMat_wide[,][datMat_wide[,] != 0] <- 1
  
  ##
  aff.df <- as.matrix(datMat_wide)
  mtx_area <-  t(aff.df) %*% aff.df
  diag(mtx_area) <- 0
  
}