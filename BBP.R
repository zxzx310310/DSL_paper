#----資料初始化----
sourceData <- read.csv(file = "assets/StoreData.csv") #讀取原始資料
goodData <- sourceData #將原始資料複製一份

goodData <- cbind(goodData, "Selected" = 0) #新增被選擇欄位


#----環境參數設定----
maxVolume <- 52*38*28 #最大箱子體積
maxPrice <- 1200 #最大金額
requiredList <- c("油", "米", "醬油", "酒", "鹽", "糖", "麵食") #必需品商品
nonRequiredList <- c("沖泡", "罐頭", "飲品", "泡麵", "零食") #非必需品商品
#categoryList <- c("油", "米", "醬油", "酒", "鹽", "糖", "麵食", "沖泡", "罐頭", "飲品", "泡麵", "零食") #必需品商品
#Vegetarian <- 0 #是否為素食(0為否, 1為是, 3為不限)

nonRequiredValues <- 5 #選擇性商品的數量
#canQt <- 2 #罐頭數量
#drinkQt <- 3 #飲料數量
#InstNoodlesQt <- 3 #泡麵數量
#snackQt <- 4 #零食數量

popAmount <- 20 #人口數量

crossRate <- 0.7
mutationRate <- 0.01


#----單方測試----
#隨機選擇必要商品(單方執行)-1
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="油",]), size = 1)
# getIndex <- which(goodData$種類=="油")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="米",]), size = 1)
# getIndex <- which(goodData$種類=="米")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="醬油",]), size = 1)
# getIndex <- which(goodData$種類=="醬油")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="酒",]), size = 1)
# getIndex <- which(goodData$種類=="酒")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="糖",]), size = 1)
# getIndex <- which(goodData$種類=="糖")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="麵食",]), size = 1)
# getIndex <- which(goodData$種類=="麵食")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="沖泡",]), size = 1)
# getIndex <- which(goodData$種類=="沖泡")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# if(Vegetarian==0) {
#   requiredGood <- sample(1:nrow(goodData[goodData$種類=="罐頭(葷)",]), size = canQt)
#   getIndex <- which(goodData$種類=="罐頭(葷)")[requiredGood]
#   goodData$Selected[getIndex] <- 1  
# } else if(Vegetarian==1) {
#   requiredGood <- sample(1:nrow(goodData[goodData$種類=="罐頭(素)",]), size = canQt)
#   getIndex <- which(goodData$種類=="罐頭(素)")[requiredGood]
#   goodData$Selected[getIndex] <- 1  
# } else {
#   requiredGood <- sample(1:nrow(goodData[goodData$種類=="罐頭(素)" | goodData$種類=="罐頭(葷)",]), size = canQt)
#   getIndex <- which(goodData$種類=="罐頭(素)" | goodData$種類=="罐頭(葷)")[requiredGood]
#   goodData$Selected[getIndex] <- 1
# }
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="飲品",]), size = drinkQt)
# getIndex <- which(goodData$種類=="飲品")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="泡麵",]), size = InstNoodlesQt)
# getIndex <- which(goodData$種類=="泡麵")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="零食",]), size = snackQt)
# getIndex <- which(goodData$種類=="零食")[requiredGood]
# goodData$Selected[getIndex] <- 1



#隨機選擇必要商品(單方執行)-2
for (i in 1:length(requiredList)) {
  requiredGood <- sample(1:nrow(goodData[goodData$種類==requiredList[i],]), size = 1) #隨機選擇一個每個種類的必要商品
  getIndex <- which(goodData$種類==requiredList[i])[requiredGood] #抓取被選擇的必要商品
  if(goodData$Selected[getIndex] != 1) {
    goodData$Selected[getIndex] <- 1 #將被抓取的商品從0改為1  
  } else {
    i <- i-1 #如果該商品被選過, 則重新迴圈
  }
}

for (i in 1:nonRequiredValues) {
  categoryGoods <- sample(nonRequiredList, 1)
  requiredGood <- sample(1:nrow(goodData[goodData$種類==categoryGoods,]), size = 1)
  getIndex <- which(goodData$種類==categoryGoods)[requiredGood]
  if(goodData$Selected[getIndex] != 1) {
    goodData$Selected[getIndex] <- 1 
  } else {
    i <- i-1 #如果該商品被選過, 則重新迴圈
  }
}

crossOverList <- fitnessAfter #先將資料移轉給另外一個變數
for (i in 1:length(crossOverList)) {
  crossOverList[[i]]["corssOverState"] <- 0 #因所有基因都尚未被交配, 所以給0作為代表
}
for (i in 1:2) {
  selected <- sample(crossOverList, 2)
  
  selected[[1]]$corssOverState <- 1 #因被選擇的基因被交配, 所以給1作為代表
  selected[[2]]$corssOverState <- 1 #因被選擇的基因被交配, 所以給1作為代表
}

#----檢驗區----


goodData[goodData$Selected==1,] #檢驗已被選擇的商品

selectedGood <- goodData[goodData$Selected==1,]
sum(selectedGood$體積)
sum(selectedGood$單價)

allFitPrice <- unlist(lapply(fitnessAfter, function(x) x$fitPrice)) #拉出所有價格的fitness
min(allFitPrice) #找出最小的數值
which.min(allFitPrice) #找出最小數值的index

#檢測是否有違反體積限制而變為NA值的資料
if(length(which(is.na(fitnessAfter))) != 0) {
  paste("有違反體積限制的基因數為: ", length(which(is.na(fitnessAfter))))
} else {
  paste("所有基因完全符合體積限制, 共", length(which(!is.na(fitnessAfter))), "個")
}

#交配測試-1
getChromLength <- length(requiredList)+nonRequiredValues #取得染色體長度
rnd <- sort(sample(getChromLength, 2)) #隨機選擇切割地方(採雙點交配)
fitnessAfter[[1]]["crossState"] <- 0
fitnessAfter[[2]]["crossState"] <- 0
tempChrom_A <- fitnessAfter[[1]] #先將染色體給暫時變數A
tempChrom_B <- fitnessAfter[[2]] #先將染色體給暫時變數B
tempChrom_A$chromosome[c(rnd[1]+1):rnd[2]] <- fitnessAfter[[2]]$chromosome[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第二個基因切割的染色體給第一個基因
tempChrom_B$chromosome[c(rnd[1]+1):rnd[2]] <- fitnessAfter[[1]]$chromosome[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第一個基因切割的染色體給第二個基因
tempChrom_A[[1]][c(rnd[1]+1):rnd[2]] <- fitnessAfter[[2]]$chromosome[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第二個基因切割的商品給第一個基因
tempChrom_B[[1]][c(rnd[1]+1):rnd[2]] <- fitnessAfter[[1]]$chromosome[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第一個基因切割的商品給第二個基因
fitnessAfter[[1]]["crossState"] <- 1
fitnessAfter[[2]]["crossState"] <- 1


#交配測試-2
paste("交配率:", crossRate)
rndCrossRate <- round(runif(n = 1, min = 0, max = 1),3)
paste("亂數交配率:", rndCrossRate)


#----Function----

#初始人口方法(素食選擇)
# initial_pop <- function(good_data, category_list) {
#   for (i in 1:length(category_list)) {
#     #若為罐頭則判斷是否為素食
#     if(category_list[i]=="罐頭") {
#       if(Vegetarian==0) {
#         requiredGood <- sample(1:nrow(good_data[good_data$種類=="罐頭(葷)",]), size = canQt)
#         getIndex <- which(good_data$種類=="罐頭(葷)")[requiredGood]
#         good_data$Selected[getIndex] <- 1  
#       } else if(Vegetarian==1) {
#         requiredGood <- sample(1:nrow(good_data[good_data$種類=="罐頭(素)",]), size = canQt)
#         getIndex <- which(good_data$種類=="罐頭(素)")[requiredGood]
#         good_data$Selected[getIndex] <- 1  
#       } else {
#         requiredGood <- sample(1:nrow(good_data[good_data$種類=="罐頭(素)" | good_data$種類=="罐頭(葷)",]), size = canQt)
#         getIndex <- which(good_data$種類=="罐頭(素)" | good_data$種類=="罐頭(葷)")[requiredGood]
#         good_data$Selected[getIndex] <- 1
#       }
#     } else if(category_list[i]=="飲品") {
#       requiredGood <- sample(1:nrow(good_data[good_data$種類==category_list[i],]), size = drinkQt)
#       getIndex <- which(good_data$種類==category_list[i])[requiredGood]
#       good_data$Selected[getIndex] <- 1 
#     } else if(category_list[i]=="泡麵") {
#       requiredGood <- sample(1:nrow(good_data[good_data$種類==category_list[i],]), size = InstNoodlesQt)
#       getIndex <- which(good_data$種類==category_list[i])[requiredGood]
#       good_data$Selected[getIndex] <- 1 
#     } else if(category_list[i]=="零食") {  
#       requiredGood <- sample(1:nrow(good_data[good_data$種類==category_list[i],]), size = snackQt)
#       getIndex <- which(good_data$種類==category_list[i])[requiredGood]
#       good_data$Selected[getIndex] <- 1 
#     } else {
#       requiredGood <- sample(1:nrow(good_data[good_data$種類==category_list[i],]), size = 1)
#       getIndex <- which(good_data$種類==category_list[i])[requiredGood]
#       good_data$Selected[getIndex] <- 1 
#     }
#   }
#   #print(good_data[good_data$Selected==1,])
# }


#初始人口方法(無素食, 選擇性商品為參數)
initial_pop <- function(good_data, require_goods, non_require_goods, non_require_values) {
  #good_data: 原始商品資料集
  #require_goods: 必要性的商品清單
  #non_require_goods: 不必要性的商品清單
  #non_require_values: 不必要性的商品數量
  
  for (i in 1:length(require_goods)) {
    requiredGood <- sample(1:nrow(good_data[good_data$種類==require_goods[i],]), size = 1) #依必要商品的每個類別去隨機挑選出商品
    getIndex <- which(good_data$種類==require_goods[i])[requiredGood] #取得剛才隨機挑選的商品位置
    if(good_data$Selected[getIndex] != 1) {
      good_data$Selected[getIndex] <- 1 #將被選擇的欄位改為1  
    } else {
      i <- i-1 #如果該商品被選過, 則重新迴圈
    }
  }
  
  for (i in 1:non_require_values) {
    categoryGoods <- sample(non_require_goods, 1) #隨機挑選選擇性商品的類別
    requiredGood <- sample(1:nrow(good_data[good_data$種類==categoryGoods,]), size = 1) #依選擇性商品的類別去隨機挑選出商品
    getIndex <- which(good_data$種類==categoryGoods)[requiredGood] #取得剛才隨機挑選的商品位置
    if(good_data$Selected[getIndex] != 1) {
      good_data$Selected[getIndex] <- 1 #將被選擇的欄位改為1  
    } else {
      i <- i-1 #如果該商品被選過, 則重新迴圈
    }
  }
  selectedGood <- good_data[good_data$Selected==1,] #將所有被選擇的欄位為1的資料拉出
  return(selectedGood) #回傳結果
}

#編碼染色體
create_chromosome <- function(gene_list) {
  #gene_list: 被選擇出的基因清單
  for(i in 1:length(gene_list)) {
     chromosome <- as.vector(gene_list[[i]][[1]]$'產品代號')
     gene_list[[i]]["chromosome"] <- list(chromosome) 
  }
  return(gene_list)
}


#計算總重量
total_Volume <- function(gene_list) {
  #gene_list: 被選擇出的基因清單
  
  for (i in 1:length(gene_list)) {
    totalVolume <- sum(gene_list[[i]][[1]]$'體積') #總和每個基因的體積
    gene_list[[i]]["totalVolume"] <- totalVolume 
  }
  return(gene_list)
}

#價格的適應度方法
fitness_price <- function(gene_list, limt_price) {
  #gene_list: 被選擇出的基因清單
  #limt_price: 價格最高限制
  
  for (i in 1:length(gene_list)) {
    sumPrice <- sum(gene_list[[i]][[1]]$'單價') #將最大限制金額減去每個基因的總金額
    reuslt <- (limt_price-sumPrice)/limt_price
    if (reuslt==0 || reuslt>=0 && reuslt<=0.1) {
      reuslt <- reuslt + 1
    } else if (reuslt>0.1 && reuslt<=0.5) {
      reuslt <- reuslt + 2
    } else {
      reuslt <- reuslt + 3
    }
    gene_list[[i]]["fitPrice"] <- reuslt 
  }
  return(gene_list)
}



#交配(雙點交配)-錯誤, 未考慮到適應函數值及未採用菁英政策和交配率
cross_over <- function(gene_list, require_goods, non_require_values) {
  #gene_list: 被選擇出的基因清單
  #require_goods: 必要性的商品清單
  #non_require_values: 不必要性的商品數量
  
  for (i in 1:length(gene_list)) {
    #先給予交配狀態, 0表示未交配, 1表示已交配
    gene_list[[i]]["crossState"] <- 0
  }
  
  get_chrom_length <- length(require_goods)+non_require_values #取得染色體長度

  if (length(which(!is.na(gene_list)))%%2==0) {
    #若基因數量為雙數時開始執行

    for (i in 1:length(gene_list)/2) {
      get_cross_state <- unlist(lapply(gene_list, function(x) x$crossState))

      if (length(get_cross_state==1)==length(gene_list)) {
        get_index <- sample(which(get_cross_state!=1),2)
        rnd <- sort(sample(get_chrom_length, 2)) #隨機選擇切割地方(採雙點交配)
        tempChrom_A <- gene_list[[get_index[1]]] #先將染色體給暫時變數A
        tempChrom_B <- gene_list[[get_index[2]]] #先將染色體給暫時變數B
        tempChrom_A$'chromosome'[c(rnd[1]+1):rnd[2]] <- gene_list[[get_index[2]]]$'chromosome'[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第二個基因切割的染色體給第一個基因
        tempChrom_B$'chromosome'[c(rnd[1]+1):rnd[2]] <- gene_list[[get_index[1]]]$'chromosome'[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第一個基因切割的染色體給第二個基因
        tempChrom_A[[1]][c(rnd[1]+1):rnd[2]] <- gene_list[[get_index[2]]]$'chromosome'[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第二個基因切割的商品給第一個基因
        tempChrom_B[[1]][c(rnd[1]+1):rnd[2]] <- gene_list[[get_index[1]]]$'chromosome'[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第一個基因切割的商品給第二個基因
        gene_list[[get_index[1]]]$'crossState' <- 1
        gene_list[[get_index[2]]]$'crossState' <- 1  
      }
    }
  }
  return(gene_list)
}



#交配(雙點交配)-需考慮適應函數值(包含懲罰執)和採用菁英政策及交配率
cross_over <- function() {
  
}




#----暫存區----
x <- "C12-11"
strsplit(x, "(?<=[A-Za-z])|(-)", perl = TRUE)



#----執行----

#產生初始口(遵照popAmount數量)
geneList <- list()
for (i in 1:popAmount) {
  gene <- initial_pop(good_data = goodData, require_goods = requiredList, non_require_goods = nonRequiredList, non_require_values = nonRequiredValues)
  geneList[[i]] <- list(gene)
}

#編碼染色體
geneList <- create_chromosome(gene_list = geneList)

#計算總體積
totalGene <- list()
totalGene <- total_Volume(gene_list = geneList)

#計算價格適應度
fitnessPriceAfter <- list()
fitnessPriceAfter <- fitness_price(gene_list = totalGene, limt_price = maxPrice)

#判斷體積是否超過最大限制, 若超過則將該欄位變為NA
for (i in 1:length(fitnessAfter)) {
  if(fitnessAfter[[i]]$totalVolume > maxVolume) {
    fitnessAfter[[i]] <- NA
  }
}

crossAfter <- list()
crossAfter <- cross_over(gene_list = fitnessAfter, require_goods = requiredList, non_require_values = nonRequiredValues)

