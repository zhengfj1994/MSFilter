# Author: Fujian Zheng
# Date: 2019/9/18

MSfilter <- function(ppm, tol_tr, filepath_1, filepath_2, filepath_3, ionmode, result_1, result_2, result_3) {
  require(openxlsx)
  if (ionmode == "pos"){
    sheetnum <- 1
  }
  else if (ionmode =="neg"){
    sheetnum <- 2
  }
  mstable_1 <- read.xlsx(filepath_1, sheet = sheetnum)
  mstable_2 <- read.xlsx(filepath_2, sheet = sheetnum)
  mstable_3 <- read.xlsx(filepath_3, sheet = sheetnum)
  
  mstable_1$match_num <- c(0)
  mstable_2$match_num <- c(0)
  mstable_3$match_num <- c(0)
  
  count_num <- 1
  for (i in c(1:length(mstable_1$mz))){
    for (j in c(1:length(mstable_2$mz))){
      mz_1 <- mstable_1$mz[i]
      mz_2 <- mstable_2$mz[j]
        
      tr_1 <- mstable_1$tr[i]
      tr_2 <- mstable_1$tr[j]
        
      if (abs(mz_1-mz_2)/mz_1*1000000 < ppm & abs(tr_1 - tr_2) < tol_tr){
        mstable_1$match_num[i] <- count_num
        mstable_2$match_num[j] <- count_num
        count_num <- count_num + 1
        break
      }
    }
  }
  mstable_1 <- mstable_1[mstable_1$match_num != 0,]
  mstable_2 <- mstable_2[mstable_2$match_num != 0,]
  mstable_2 <- mstable_2[order(mstable_2$match_num),]
  mstable_1$match_num <- 0
  mstable_2$match_num <- 0
  
  
  count_num <- 1
  for (i in c(1:length(mstable_1$mz))){
    for (j in c(1:length(mstable_3$mz))){
      mz_1 <- mstable_1$mz[i]
      mz_3 <- mstable_3$mz[j]
      
      tr_1 <- mstable_1$tr[i]
      tr_3 <- mstable_3$tr[j]
      
      if (abs(mz_1-mz_3)/mz_1*1000000 < ppm & abs(tr_1 - tr_3) < tol_tr){
        mstable_1$match_num[i] <- count_num
        mstable_2$match_num[i] <- count_num
        mstable_3$match_num[j] <- count_num
        count_num <- count_num + 1
        break
      }
    }
  }
  
  mstable_1 <- mstable_1[mstable_1$match_num != 0,]
  mstable_2 <- mstable_2[mstable_2$match_num != 0,]
  mstable_3 <- mstable_3[mstable_3$match_num != 0,]
  mstable_3 <- mstable_3[order(mstable_3$match_num),]
  write.xlsx(mstable_1, file = result_1)
  write.xlsx(mstable_2, file = result_2)
  write.xlsx(mstable_3, file = result_3)
}

MSfilter(ppm = 10, tol_tr = 0.2, filepath_1 = "E:\\自编小程序\\wqq-PCa\\SYP1(without control).xlsx",
         filepath_2 = "E:\\自编小程序\\wqq-PCa\\SYP2(with control).xlsx",
         filepath_3 = "E:\\自编小程序\\wqq-PCa\\WQQ.xlsx",
         ionmode = "pos",
         result_1 = "E:\\自编小程序\\wqq-PCa\\SYP1(without control)-2.xlsx",
         result_2 = "E:\\自编小程序\\wqq-PCa\\SYP2(with control)-2.xlsx",
         result_3 = "E:\\自编小程序\\wqq-PCa\\WQQ-2.xlsx")

