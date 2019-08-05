totalFitness <- vector()
price <- vector()
weight <- vector()
volumeRate <- vector()
timeStamp <- vector()

for (loop in 1:10) {
  #----時間紀錄(開始)----
  startTime <- Sys.time()
  
  #----資料初始化(本地端)----
  sourceData <- read.csv(file = "assets/商品資料庫.csv") #讀取原始資料
  preferenceTable <- read.csv(file = "assets/preferenceTable.csv") #讀取商品偏好表
  sourceData <- sourceData[c(-1, -13)] #移除不必要的資料欄位
  names(sourceData)[11] <- "重量" #重新命名欄位名稱
  
  goodData <- sourceData #將原始資料複製一份
  goodData <- cbind(goodData, "Selected" = 0, "Preference" = 1) #新增被選擇欄位
  
  #----環境參數設定----
  maxVolume <- 47*32*39 #最大箱子體積
  maxWeight <- 16000 #最大重量(g)
  popAmount <- 30 #人口數量
  crossRate <- 0.7 #交配率
  mutationRate <- 0.005 #突變率
  eliteValues <- round(popAmount*0.1) #菁英數量
  maxGen <- 30000 #世代次數
  
  #----使用者需輸入的參數(假設)----
  dietHabit <- "葷食" #葷食與素食的選擇
  userItemValues <- 22 #使用者需要的數量
  maxPrice <- 1500 #使用者金額
  
  #----Function----
  #偏好值與類別合併:
  #將使用者對商品種類的偏好與原始商品資料進行合併成一個Data Frame, 使原始資料有使用者對每個商品的品項偏好
  preference_match <- function(good_data, preference_table) {
    #gene_list: 被選擇出的基因清單
    #require_goods: 必要性的商品清單
    #non_require_goods: 不必要性的商品清單
    #user_preference: 使用者對商品種類的偏好
    
    for (i in 1:dim(preference_table)[1]) {
      good_data[good_data$種類==preference_table$category[i],]$Preference <- as.numeric(preference_table$preference[i])
    }
    return(good_data)
  }
  
  #飲食習慣(葷或素食):
  #在開始演算法前先將該飲食習慣給加入
  #若為葷食則包含素食和葷食, 反之只有素食
  diet_select <- function(good_data, diet_habit_list) {
    #good_data: 原始商品資料集
    #diet_habit_list: 葷食或素食的選擇
    
    if(diet_habit_list=="素食") {
      good_data <- good_data[good_data$'葷素'==diet_habit_list,] #如果是素食就將屬於素食的產品篩選出來
      good_data$'種類' <- factor(good_data$種類)
    }
    return(good_data)
  }
  
  initial_pop <- function(good_data, require_goods, non_require_goods, non_require_values, limit_weight) {
    #good_data: 原始商品資料集
    #require_goods: 必要性的商品清單
    #non_require_goods: 不必要性的商品清單
    #non_require_values: 不必要性的商品數量
    #limit_weight: 最大重量限制
    
    repeat {
      temp_good <- good_data #先將原始資料暫時給另外一個變數使用
      
      for (i in 1:length(require_goods)) {
        get_index <- sample(which(temp_good$'種類'==require_goods[i] & temp_good$'Selected'!=1), 1) #隨機抓出符合種類並Selected欄位不等於1的列
        temp_good$'Selected'[get_index] <- 1 #將被選擇的欄位改為1    
      }
      
      selected_require <- sample(non_require_goods, non_require_values)
      for (i in 1:length(selected_require)) {
        get_index <- sample(which(temp_good$'種類'==selected_require[i] & temp_good$'Selected'!=1), 1) #隨機抓出符合種類並Selected欄位不等於1的列
        temp_good$'Selected'[get_index] <- 1 #將被選擇的欄位改為1
      }
      
      selected_good <- temp_good[temp_good$'Selected'==1,] #將被選擇的商品放入新變數中
      sum_weight <- sum(selected_good$'重量') #計算此基因的總重量
      
      if (sum_weight <= limit_weight) {
        break
      }
    }
    return(selected_good) #回傳結果
  }
  
  
  #編碼染色體:
  #必要性商品必定方在最前段, 選擇性商品必定放在後段
  create_chromosome <- function(gene_list) {
    #gene_list: 被選擇出的基因清單
    for(i in 1:length(gene_list)) {
      chromosome <- as.vector(gene_list[[i]][[1]]$'產品代號')
      gene_list[[i]]["chromosome"] <- list(chromosome) 
    }
    return(gene_list)
  }
  
  
  #計算總重量
  total_weight <- function(gene_list) {
    #gene_list: 被選擇出的基因清單
    
    for(i in 1:length(gene_list)) {
      sum_weight <- sum(gene_list[[i]][[1]]$'重量')
      gene_list[[i]]["totalWeight"] <- list(sum_weight) 
    }
    return(gene_list)
  }
  
  #偏好的適應度方法(算式分母為偏好值1~偏好的最大值)
  fitness_category <- function(gene_list, require_goods, non_require_goods) {
    #gene_list: 被選擇出的基因清單
    #require_goods: 必要性的商品清單
    #non_require_goods: 不必要性的商品清單
    #user_preference: 使用者對商品種類的偏好
    
    all_category <- c(require_goods, non_require_goods) #所有種類
    for (i in 1:length(gene_list)) {
      match_index <- all_category %in% gene_list[[i]][[1]]$'種類' #與所有種類相比, 若有該種類的話會是T, 否則F
      F_values <- length(match_index[match_index == TRUE]) #符合種類的數量
      subtraction_category <- length(all_category)-F_values #相減得遺漏的種類數量
      reuslt <- abs(subtraction_category)/length(all_category) #將體積適應度算出
      
      if (subtraction_category==0) {
        reuslt <- 1 #若適應度等於0就給予懲罰值1
      } else {
        reuslt <- reuslt + 3 #若適應度不等於0就給予懲罰值3
      }
      levels(gene_list[[i]][[1]]$'種類') <- all_category
      gene_list[[i]]["fitCategory"] <- reuslt 
    }
    return(gene_list)
  }
  
  
  #體積的適應度方法(已加入懲罰值)
  fitness_volume <- function(gene_list, bin_volume) {
    #gene_list: 被選擇出的基因清單
    #bin_volume: 箱子的乘積
    
    for (i in 1:length(gene_list)) {
      sum_volume <- sum(gene_list[[i]][[1]]$'體積') #將最大限制體積減去每個基因的總體積
      subtraction_volume <- bin_volume-sum_volume #容積上限與選擇商品之總體積的差額
      reuslt <- abs(subtraction_volume)/bin_volume #將體積適應度算出
      
      if (subtraction_volume==0) {
        reuslt <- 1 #若適應度等於0就給予懲罰值1, e.g. (49795.2-27749.25)/49795.2=0.4427324, 愈接近0表示價格差距越小
      } else {
        reuslt <- reuslt + 1 #若適應度大於0就給予懲罰值2
      } 
      gene_list[[i]]["fitVolume"] <- reuslt 
      gene_list[[i]]["totalVolume"] <- sum_volume 
    }
    return(gene_list)
  }
  
  #價格的適應度方法(已加入懲罰值)
  fitness_price <- function(gene_list, limit_price) {
    #gene_list: 被選擇出的基因清單
    #limit_price: 價格最高限制
    
    for (i in 1:length(gene_list)) {
      sum_price <- sum(gene_list[[i]][[1]]$'單價') #將最大限制金額減去每個基因的總金額
      subtraction_price <- limit_price-sum_price #預算與商品組合之總價格的差額
      reuslt <- abs(subtraction_price)/limit_price #將價格適應度算出
      if (subtraction_price==0) {
        reuslt <- 1
      } else {
        reuslt <- reuslt + 2
      }
      gene_list[[i]]["fitPrice"] <- reuslt
      gene_list[[i]]["totalPrice"] <- sum_price
    }
    return(gene_list)
  }
  
  #總體的適應度方法
  fitness_total <- function(gene_list) {
    #gene_list: 被選擇出的基因清單
    
    sum_fit <- unlist(lapply(gene_list, function(x) x$fitVolume*x$fitPrice))
    
    for (i in 1:length(gene_list)) {
      sum_fit <- gene_list[[i]]$'fitVolume'*gene_list[[i]]$'fitPrice'*gene_list[[i]]$'fitCategory'
      gene_list[[i]]["totalFit"] <- sum_fit
    }
    return(gene_list)
  }
  
  #選擇(競賽法)-1:
  #從父母中隨機挑選出兩個染色體, 這兩染色體互相比較總適應度, 越低者獲勝, 將被複製至交配池中, 直至交配池內的數量與人口數相同
  selection_first <- function(gene_list, pop_amount) {
    #gene_list: 被選擇出的基因清單
    #pop_amount: 人口數量
    
    result <- list()
    for (i in 1:pop_amount) {
      compare_list <- sample(gene_list, 2) #隨機挑選兩個染色體
      if (compare_list[[1]]$'totalFit'<compare_list[[2]]$'totalFit') {
        result[[i]] <- compare_list[[1]]
      } else if (compare_list[[1]]$'totalFit'>compare_list[[2]]$'totalFit') {
        result[[i]] <- compare_list[[2]]
      } else {
        result[[i]] <- sample(compare_list, 1)[[1]]
      }
    }
    
    return(result)
  }
  
  #交配(雙點交配)-需考慮適應函數值(包含懲罰值)、交配率和重量限制
  cross_over <- function(good_data, gene_list, require_goods, non_require_goods, non_require_values, cross_rate) {
    get_chrom_length <- length(require_goods)+non_require_values #取得染色體長度
    
    for (i in 1:length(gene_list)) {
      gene_list[[i]]["crossState"] <- 0 #先給予交配狀態, 0表示未交配, 1表示已交配
    }
    
    for(i in 1:c(length(gene_list)/2)){
      get_cross_state <- unlist(lapply(gene_list, function(x) x$crossState)) #給定目前交配狀態
      get_index <- as.vector(sample(which(get_cross_state!=1),2)) #抽取要被交配的基因
      rnd_cross_rate <- round(runif(n = 1, min = 0, max = 1),3) #產生亂數
      
      if(rnd_cross_rate<=cross_rate){
        #亂數小於等於交配率, 則進行交配
        divide_index <- sort(as.vector(sample(get_chrom_length, 2))) #隨機選擇切割地方(採雙點交配)
        tempChrom_A <- gene_list[[get_index[1]]] #先將染色體給暫時變數A
        tempChrom_B <- gene_list[[get_index[2]]] #先將染色體給暫時變數B
        tempChrom_A$'chromosome'[divide_index[1]:divide_index[2]] <- gene_list[[get_index[2]]]$'chromosome'[divide_index[1]:divide_index[2]] #開始進行交配, 將第二個基因切割的染色體給第一個基因
        tempChrom_B$'chromosome'[divide_index[1]:divide_index[2]] <- gene_list[[get_index[1]]]$'chromosome'[divide_index[1]:divide_index[2]] #開始進行交配, 將第一個基因切割的染色體給第二個基因
        tempChrom_A[[1]][divide_index[1]:divide_index[2],] <- gene_list[[get_index[2]]][[1]][divide_index[1]:divide_index[2],] #開始進行交配, 將第二個基因切割的商品給第一個基因
        tempChrom_B[[1]][divide_index[1]:divide_index[2],] <- gene_list[[get_index[1]]][[1]][divide_index[1]:divide_index[2],] #開始進行交配, 將第一個基因切割的商品給第二個基因
        tempChrom_A$'totalWeight' <- sum(tempChrom_A[[1]]$'重量') #重新計算總重量
        tempChrom_B$'totalWeight' <- sum(tempChrom_B[[1]]$'重量') #重新計算總重量
        
        repeat{  
          if(length(which(duplicated(tempChrom_A[[1]]$'種類'))) != 0){
            #抓出重複的物品
            repeat_index <- which(good_data$'種類' %in% as.character(tempChrom_A[[1]]$'種類')) #從資料集中找出與tempChrom_A相同的種類商品
            temp_df <- good_data[-repeat_index,] #去除掉相同的種類商品
            sample_item <- temp_df[sample(nrow(temp_df), 1),] #從未被選重的商品類別中隨機取物
            drop_rows <- which(duplicated(tempChrom_A[[1]]$'種類')) #抓出重複的第一個物品
            tempChrom_A[[1]][drop_rows,] <- sample_item #在data frame中取代重複的物品
            tempChrom_A$'chromosome'[drop_rows] <- as.character(tempChrom_A[[1]][drop_rows,]$'產品代號') #在編碼中取代重複的物品
          }
          
          if(length(which(duplicated(tempChrom_B[[1]]$'種類'))) != 0){
            #抓出重複的物品
            repeat_index <- which(good_data$'種類' %in% as.character(tempChrom_B[[1]]$'種類')) #從資料集中找出與tempChrom_B相同的種類商品
            temp_df <- good_data[-repeat_index,] #去除掉相同的種類商品
            sample_item <- temp_df[sample(nrow(temp_df), 1),] #從未被選重的商品類別中隨機取物
            drop_rows <- which(duplicated(tempChrom_B[[1]]$'種類')) #抓出重複的第一個物品
            tempChrom_B[[1]][drop_rows,] <- sample_item #在data frame中取代重複的物品
            tempChrom_B$'chromosome'[drop_rows] <- as.character(tempChrom_B[[1]][drop_rows,]$'產品代號') #在編碼中取代重複的物品
          }
          
          
          if(length(which(duplicated(tempChrom_A[[1]]$'種類'))) == 0 & length(which(duplicated(tempChrom_B[[1]]$'種類'))) == 0){
            tempChrom_A$'crossState' <- 1
            tempChrom_B$'crossState' <- 1
            gene_list[[get_index[1]]] <- tempChrom_A
            gene_list[[get_index[2]]] <- tempChrom_B
            break
          }
        }
      } else {
        #亂數大於交配率, 則不進行交配
        gene_list[[get_index[1]]]$'crossState' <- 1
        gene_list[[get_index[2]]]$'crossState' <- 1
      }
    }
    return(gene_list)
  }
  
  
  #突變方法, 加入重量限制(突變部分直接隨機突變非必選的商品)
  mutation_FN <- function(good_data, gene_list, mutation_rate, require_goods, non_require_values, non_require_goods) {
    #good_data: 商品資料集
    #gene_list: 已交配過的基因人口群
    #mutation_rate: 交配率
    #require_goods: 必要性商品清單
    #non_require_values: 選擇性商品數量
    
    get_chrom_length <- length(require_goods)+non_require_values #基因的長度
    good_data_non_require <- good_data[good_data$'種類' %in% non_require_goods,] #取得所有選擇性商品
    
    for(i in 1:length(gene_list)) {
      mutation_index <- as.numeric(sample(get_chrom_length, 1)) #隨機取得要突變的位置
      rnd_mutation_rate <- runif(n = 1, min = 0, max = 1) #產生亂數
      
      if(rnd_mutation_rate <= mutation_rate){
        mutation_category <- as.character(gene_list[[i]][[1]][mutation_index,]$'種類') #取得染色體中要被突變的基因商品種類
        
        if(gene_list[[i]][[1]][mutation_index,]$'種類' %in% require_goods) {
          temp_df <- good_data[good_data$'種類'==mutation_category & good_data$'產品代號'!=gene_list[[i]][[1]][mutation_index,]$'產品代號',] #建立符合突變基因商品資料(不包含自己)
          temp_good <- temp_df[sample(nrow(temp_df), 1), ] #從商品資料中隨機取得符合該基因突變的商品(不包含自己)
          gene_list[[i]][[1]][mutation_index,] <-  temp_good #將商品進行變異
          gene_list[[i]]$'chromosome'[mutation_index] <- as.character(temp_good$'產品代號') #更改編碼基因代號
          gene_list[[i]]$'totalWeight' <- sum(gene_list[[i]][[1]]$'重量') #更新並計算總重量
        } else {
          temp_df <- good_data_non_require[good_data_non_require$'種類'==mutation_category & good_data_non_require$'產品代號'!=gene_list[[i]][[1]][mutation_index,]$'產品代號',] #建立選擇性商品資料(不包含自己)
          temp_good <- temp_df[sample(nrow(temp_df), 1), ] #從選擇性商品資料中隨機取得符合該基因突變的商品(不包含自己)
          gene_list[[i]][[1]][mutation_index,] <-  temp_good #將商品進行變異
          gene_list[[i]]$'chromosome'[mutation_index] <- as.character(temp_good$'產品代號') #更改編碼基因代號
          gene_list[[i]]$'totalWeight' <- sum(gene_list[[i]][[1]]$'重量')
        }
      }
    }
    return(gene_list)
  }
  
  #將符合體重的群組合併起來(包含菁英)
  merge_population <- function(first_gene, second_gene, elite_pop, limit_weight, limit_volume) {
    new_pop <- list()
    new_pop <- first_gene #將此代染色體放入新的變數
    new_pop <- append(new_pop, second_gene) #將下代染色體加入變數
    condition_pop <- list() 
    for (i in 1:length(new_pop)) {
      if(new_pop[[i]]$'totalWeight' <= limit_weight & new_pop[[i]]$'totalVolume' <= limit_volume){
        condition_pop <- append(condition_pop, new_pop[i]) #將未超過限制重量的染色體放入新的群組
      }
    }
    if(length(elite_pop)!=0){
      new_pop <- append(new_pop, elite_pop) #將菁英染色體加入變數
    }
    condition_pop <- condition_pop[order(sapply(condition_pop, function(x) x$totalFit), decreasing=FALSE)] #將人口按照適應函數遞減排序
    return(condition_pop)
  }
  
  #將精英群複製出來
  elite_population <- function(merge_list, elite_pop) {
    elite_list <- list()
    elite_list <- head(merge_list[order(sapply(merge_list, function(x) x$totalFit), decreasing=FALSE)], elite_pop) #取得population(人口數量)的族群
    return(elite_list)
  }
  
  
  #新的下一代, 抓取符合群組數量
  new_population <- function(merge_list, pop_amount) {
    new_pop_list <- head(merge_list, pop_amount) #取得符合群組數量的染色體數
    return(new_pop_list)
  }
  
  #更新菁英群組
  new_elite_population <- function(old_elite_list, now_elite_list, elite_pop) {
    now_elite_list <- append(now_elite_list, old_elite_list) #將舊的菁英與新的菁英合併
    new_elite <- head(now_elite_list[order(sapply(now_elite_list, function(x) x$totalFit), decreasing=FALSE)], elite_pop) #將菁英人口按照適應函數遞減排序, 並取得elite_pop(菁英數量)的成員
    return(new_elite)
  }
  
  #----執行----
  #葷素的方法
  goodData <- diet_select(good_data = goodData, diet_habit_list = dietHabit)
  
  
  level <- levels(goodData$種類)
  requiredList <- level[order(nchar(level), level)][1:6]
  nonRequiredList <- level[order(nchar(level), level)][-1:-length(requiredList)]
  nonRequiredValues <-  userItemValues-length(requiredList) #選擇性商品的數量
  
  goodData <- preference_match(good_data = goodData, preference_table = preferenceTable)
  
  geneList <- list()
  for (i in 1:popAmount) {
    gene <- initial_pop(good_data = goodData, require_goods = requiredList, non_require_goods = nonRequiredList, non_require_values = nonRequiredValues, limit_weight = maxWeight)
    geneList[[i]] <- list(gene)
  }
  
  #編碼染色體
  geneList <- create_chromosome(gene_list = geneList)
  
  #計算每個基因總重量
  geneList <- total_weight(gene_list = geneList)
  
  #計算偏好適應度(目前僅計算總偏好值)
  fitnessCategory <- list()
  fitnessCategory <- fitness_category(gene_list = geneList, require_goods = requiredList, non_require_goods =  nonRequiredList)
  
  #計算體積適應度
  fitnessVolumeAfter <- list()
  fitnessVolumeAfter <- fitness_volume(gene_list = fitnessCategory, bin_volume = maxVolume)
  
  #計算價格適應度
  fitnessPriceAfter <- list()
  fitnessPriceAfter <- fitness_price(gene_list = fitnessVolumeAfter, limit_price = maxPrice)
  
  #計算總體適應度
  fitnessTotalAfter <- list()
  fitnessTotalAfter <- fitness_total(gene_list = fitnessPriceAfter)
  
  #選擇
  selectionAfter <- list()
  selectionAfter <- selection_first(gene_list = fitnessTotalAfter, pop_amount = popAmount)
  
  #交配
  crossAfter <- list()
  crossAfter <- cross_over(good_data = goodData, gene_list = selectionAfter, require_goods = requiredList, non_require_values = nonRequiredValues, cross_rate = crossRate)
  
  #重新計算偏好適應函數, 體積適應函數, 種類適應函數
  crossAfter <- fitness_category(gene_list = crossAfter, require_goods = requiredList, non_require_goods = nonRequiredList)
  crossAfter <- fitness_volume(gene_list = crossAfter, bin_volume = maxVolume) 
  crossAfter <- fitness_price(gene_list = crossAfter, limit_price = maxPrice)
  crossAfter <- fitness_total(gene_list = crossAfter)
  
  #突變
  mutationAfter <- list()
  mutationAfter <- mutation_FN(good_data = goodData, gene_list = crossAfter, mutation_rate = mutationRate, require_goods = requiredList, non_require_values = nonRequiredValues, non_require_goods = nonRequiredList)
  
  #重新計算偏好適應函數, 體積適應函數, 價格適應函數
  mutationAfter <- fitness_category(gene_list = mutationAfter, require_goods = requiredList, non_require_goods = nonRequiredList)
  mutationAfter <- fitness_volume(gene_list = mutationAfter, bin_volume = maxVolume) 
  mutationAfter <- fitness_price(gene_list = mutationAfter, limit_price = maxPrice)
  mutationAfter <- fitness_total(gene_list = mutationAfter)
  
  #將父母代與孩子合併
  mergeList <- list()
  mergeList <- merge_population(first_gene = fitnessTotalAfter, second_gene = mutationAfter, elite_pop = latestElite, limit_weight = maxWeight, limit_volume = maxVolume)
  
  #此代的菁英群組
  latestElite <- elite_population(merge_list = mergeList, elite_pop = eliteValues)
  
  #新的下一代
  newPopulation <- list()
  newPopulation <- new_population(merge_list = mergeList, pop_amount = popAmount)
  
  gen_values_best <- vector() #紀錄最好的基因總體適應函數
  gen_values_loss <- vector() #紀錄最差的基因總體適應函數
  gen_price_best <- vector()
  gen_preference_best <- vector()
  
  gen_values_best[1] <- latestElite[[1]]$totalFit
  gen_values_loss[1] <- newPopulation[[popAmount]]$totalFit #紀錄最差的總體適應函數
  gen_price_best[1] <- latestElite[[1]]$totalPrice #紀錄最佳的總價格
  print(paste("============第", 1, "代============"))
  
  
  for (i in 2:maxGen) {
    #選擇
    selectionAfter <- list()
    selectionAfter <- selection_first(gene_list = newPopulation, pop_amount = popAmount)
    
    #交配
    crossAfter <- list()
    crossAfter <- cross_over(good_data = goodData, gene_list = selectionAfter, require_goods = requiredList, non_require_values = nonRequiredValues, cross_rate = crossRate)
    
    #突變
    mutationAfter <- list()
    mutationAfter <- mutation_FN(good_data = goodData, gene_list = crossAfter, mutation_rate = mutationRate, require_goods = requiredList, non_require_values = nonRequiredValues, non_require_goods = nonRequiredList)
    
    #重新計算偏好適應函數, 體積適應函數, 種類適應函數
    mutationAfter <- fitness_category(gene_list = mutationAfter, require_goods = requiredList, non_require_goods = nonRequiredList)
    mutationAfter <- fitness_volume(gene_list = mutationAfter, bin_volume = maxVolume) 
    mutationAfter <- fitness_price(gene_list = mutationAfter, limit_price = maxPrice)
    mutationAfter <- fitness_total(gene_list = mutationAfter)
    
    #將父母代與孩子合併
    mergeList <- list()
    mergeList <- merge_population(first_gene = newPopulation, second_gene = mutationAfter, elite_pop = latestElite, limit_weight = maxWeight, limit_volume = maxVolume)
    
    #此代的菁英群組
    nowEliteLiet <- list()
    nowEliteLiet <- elite_population(merge_list = mergeList, elite_pop = eliteValues)
    
    #更新舊代的菁英群組
    latestElite <- new_elite_population(old_elite_list = latestElite, now_elite_list = nowEliteLiet, elite_pop = eliteValues)
    
    
    #新的下一代
    newPopulation <- list()
    newPopulation <- new_population(merge_list = mergeList, pop_amount = popAmount)
    
    gen_values_best[i] <- latestElite[[1]]$totalFit
    gen_values_loss[i] <- newPopulation[[popAmount]]$totalFit #紀錄最差的總體適應函數
    gen_price_best[i] <- latestElite[[1]]$totalPrice #紀錄最佳的總價格
    print(paste("============第", i, "代============"))
  }
  
  plot(gen_values_best, main = paste("裝箱演算法-第", loop, "次"), xlab = "世代次數", ylab = "總體適應函數") #畫圖來顯示總體適應函數的起伏
  
  library(ggplot2)
  temp_DF <- data.frame("世代數" = c(1:length(gen_values_best)), "適應函數" = gen_values_best)
  p <- ggplot(temp_DF, aes(x = 世代數, y = 適應函數)) + geom_line(colour = 'red')
  p
  
  #----時間紀錄(結束)----
  endTime <- Sys.time()
  resultTime <- endTime - startTime
  print(resultTime)
  print(latestElite[[1]]$totalFit) #適應性
  print(sum(latestElite[[1]][[1]]$'體積')/(maxVolume))
  print(latestElite[[1]]$'totalWeight')
  print(sum(latestElite[[1]][[1]]$'單價'))
  
  totalFitness[loop] <- latestElite[[1]]$'totalFit'
  price[loop] <- sum(latestElite[[1]][[1]]$'單價')
  weight[loop] <- latestElite[[1]]$'totalWeight'
  volumeRate[loop] <- sum(latestElite[[1]][[1]]$'體積')/(maxVolume)
  timeStamp[loop] <- resultTime
}

tempDF <- data.frame(總適應函數 = totalFitness, 總價格 = price, 總重量 = weight, 體積率 = volumeRate, 執行時間 = timeStamp)
write.csv(x = tempDF, file = "result2.csv", row.names = FALSE)