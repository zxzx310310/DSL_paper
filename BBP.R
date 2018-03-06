sourceData <- read.csv(file = "assets/StoreData.csv")
goodData <- sourceData

#新增被選擇欄位
goodData <- cbind(goodData, "Selected" = 0)


#----參數----
maxVolume <- 52*38*28 #最大箱子體積
maxPrice <- 1200 #最大金額
categoryList <- c("油", "米", "醬油", "酒", "糖", "麵食", "沖泡", "罐頭", "飲品", "泡麵", "零食") #必需品商品
#NonRequiredList <- c("飲品", "泡麵", "零食") #非必需品商品
Vegetarian <- 0 #是否為素食(0為否, 1為是, 3為不限)

canQt <- 2 #罐頭數量
drinkQt <- 3 #飲料數量
InstNoodlesQt <- 3 #泡麵數量
snackQt <- 4 #零食數量

popAmount <- 100 #人口數量


#隨機選擇必要商品(單方執行)
requiredGood <- sample(1:nrow(goodData[goodData$種類=="油",]), size = 1)
getIndex <- which(goodData$種類=="油")[requiredGood]
goodData$Selected[getIndex] <- 1

requiredGood <- sample(1:nrow(goodData[goodData$種類=="米",]), size = 1)
getIndex <- which(goodData$種類=="米")[requiredGood]
goodData$Selected[getIndex] <- 1

requiredGood <- sample(1:nrow(goodData[goodData$種類=="醬油",]), size = 1)
getIndex <- which(goodData$種類=="醬油")[requiredGood]
goodData$Selected[getIndex] <- 1

requiredGood <- sample(1:nrow(goodData[goodData$種類=="酒",]), size = 1)
getIndex <- which(goodData$種類=="酒")[requiredGood]
goodData$Selected[getIndex] <- 1

requiredGood <- sample(1:nrow(goodData[goodData$種類=="糖",]), size = 1)
getIndex <- which(goodData$種類=="糖")[requiredGood]
goodData$Selected[getIndex] <- 1

requiredGood <- sample(1:nrow(goodData[goodData$種類=="麵食",]), size = 1)
getIndex <- which(goodData$種類=="麵食")[requiredGood]
goodData$Selected[getIndex] <- 1

requiredGood <- sample(1:nrow(goodData[goodData$種類=="沖泡",]), size = 1)
getIndex <- which(goodData$種類=="沖泡")[requiredGood]
goodData$Selected[getIndex] <- 1

if(Vegetarian==0) {
  requiredGood <- sample(1:nrow(goodData[goodData$種類=="罐頭(葷)",]), size = canQt)
  getIndex <- which(goodData$種類=="罐頭(葷)")[requiredGood]
  goodData$Selected[getIndex] <- 1  
} else if(Vegetarian==1) {
  requiredGood <- sample(1:nrow(goodData[goodData$種類=="罐頭(素)",]), size = canQt)
  getIndex <- which(goodData$種類=="罐頭(素)")[requiredGood]
  goodData$Selected[getIndex] <- 1  
} else {
  requiredGood <- sample(1:nrow(goodData[goodData$種類=="罐頭(素)" | goodData$種類=="罐頭(葷)",]), size = canQt)
  getIndex <- which(goodData$種類=="罐頭(素)" | goodData$種類=="罐頭(葷)")[requiredGood]
  goodData$Selected[getIndex] <- 1
}

requiredGood <- sample(1:nrow(goodData[goodData$種類=="飲品",]), size = drinkQt)
getIndex <- which(goodData$種類=="飲品")[requiredGood]
goodData$Selected[getIndex] <- 1

requiredGood <- sample(1:nrow(goodData[goodData$種類=="泡麵",]), size = InstNoodlesQt)
getIndex <- which(goodData$種類=="泡麵")[requiredGood]
goodData$Selected[getIndex] <- 1

requiredGood <- sample(1:nrow(goodData[goodData$種類=="零食",]), size = snackQt)
getIndex <- which(goodData$種類=="零食")[requiredGood]
goodData$Selected[getIndex] <- 1



#檢驗已被選擇的商品
goodData[goodData$Selected==1,]

selectedGood <- goodData[goodData$Selected==1,]
sum(selectedGood$體積)
sum(selectedGood$單價)



#----Function----

#初始人口方法
initial_pop <- function(good_data, category_list) {
  for (i in 1:length(category_list)) {
    #若為罐頭則判斷是否為素食
    if(category_list[i]=="罐頭") {
      if(Vegetarian==0) {
        requiredGood <- sample(1:nrow(good_data[good_data$種類=="罐頭(葷)",]), size = canQt)
        getIndex <- which(good_data$種類=="罐頭(葷)")[requiredGood]
        good_data$Selected[getIndex] <- 1  
      } else if(Vegetarian==1) {
        requiredGood <- sample(1:nrow(good_data[good_data$種類=="罐頭(素)",]), size = canQt)
        getIndex <- which(good_data$種類=="罐頭(素)")[requiredGood]
        good_data$Selected[getIndex] <- 1  
      } else {
        requiredGood <- sample(1:nrow(good_data[good_data$種類=="罐頭(素)" | good_data$種類=="罐頭(葷)",]), size = canQt)
        getIndex <- which(good_data$種類=="罐頭(素)" | good_data$種類=="罐頭(葷)")[requiredGood]
        good_data$Selected[getIndex] <- 1
      }
    } else if(category_list[i]=="飲品") {
      requiredGood <- sample(1:nrow(good_data[good_data$種類==category_list[i],]), size = drinkQt)
      getIndex <- which(good_data$種類==category_list[i])[requiredGood]
      good_data$Selected[getIndex] <- 1 
    } else if(category_list[i]=="泡麵") {
      requiredGood <- sample(1:nrow(good_data[good_data$種類==category_list[i],]), size = InstNoodlesQt)
      getIndex <- which(good_data$種類==category_list[i])[requiredGood]
      good_data$Selected[getIndex] <- 1 
    } else if(category_list[i]=="零食") {  
      requiredGood <- sample(1:nrow(good_data[good_data$種類==category_list[i],]), size = snackQt)
      getIndex <- which(good_data$種類==category_list[i])[requiredGood]
      good_data$Selected[getIndex] <- 1 
    } else {
      requiredGood <- sample(1:nrow(good_data[good_data$種類==category_list[i],]), size = 1)
      getIndex <- which(good_data$種類==category_list[i])[requiredGood]
      good_data$Selected[getIndex] <- 1 
    }
  }
  #print(good_data[good_data$Selected==1,])
}


#價格的適應度方法
fitness_price <- function(gene_list) {
  for (i in 1:length(gene_list)) {
    fitPrice <- maxPrice - sum(gene_list[[i]][[1]]$'單價')
    gene_list[[i]]["fitPrice"] <- fitPrice
  }
  return(gene_list)
}



cross_over <- function() {
  #交配
}



#----執行----

#產生初始口(遵照popAmount數量)
geneList <- list()
for (i in 1:popAmount) {
  gene <- initial_pop(good_data = goodData, category_list = categoryList)
  geneList[[i]] <- list(gene)
}

#計算價格適應度
fitnessAfter <- list()
fitnessAfter <- fitness_price(gene_list = geneList)
