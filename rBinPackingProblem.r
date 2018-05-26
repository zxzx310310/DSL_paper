#----使用者需輸入的參數(假設)----
args <- commandArgs(TRUE)
dietHabit <- as.character(args[1]) #葷素食
if(dietHabit == "葷食") {
  userItemValues <- as.integer(args[2]) #使用者需要的數量
  maxPrice <- as.integer(unlist(strsplit(as.character(args[3]),split="-",fixed=T))[2]) #最大金額
  exceptBrandList <- as.character(args[4]) #將要剔除的品牌
  userPreference <- as.integer(c(args[5], args[6], args[7], args[8], args[9],
                                 args[10], args[11], args[12], args[13], args[14],
                                 args[15], args[16], args[17], args[18], args[19], args[20])) #葷食志願序
} else if(dietHabit == "素食"){
  userItemValues <- as.integer(args[2]) #使用者需要的數量
  maxPrice <- as.integer(unlist(strsplit(as.character(args[3]),split="-",fixed=T))[2]) #最大金額
  exceptBrandList <- as.character(args[4]) #將要剔除的品牌
  userPreference <- as.integer(c(args[5], args[6], args[7], args[8], args[9],
                                 args[10], args[11], args[12], args[13], args[14],
                                 args[15], args[16])) #素食的志願序
}

#write.table(x = c(dietHabit, userItemValues, maxPrice, exceptBrandList, userPreference), file = "test.txt", sep = ",")

#----資料初始化(資料庫)----
pkgs = "RMySQL"
pkgs = pkgs[!( pkgs %in% installed.packages()[,"Package"] )]
if(length(pkgs)) install.packages(pkgs) #確認是否有此套件, 若無就安裝
library(RMySQL)

mydb = dbConnect(MySQL(), user="root", password="", dbname="rpreferdatabase", host='127.0.0.1')
dbSendQuery(mydb,"SET NAMES big5")
result <- dbSendQuery(mydb, 'SELECT * FROM classicbase;')

sourceData <- fetch(result, n = -1)

sourceData <- sourceData[c(-1, -13)] #移除不必要的資料欄位

names(sourceData)[1] <- "產品代號"
names(sourceData)[2] <- "品名"
names(sourceData)[3] <- "單價"
names(sourceData)[4] <- "體積"
names(sourceData)[5] <- "廠牌"
names(sourceData)[6] <- "長"
names(sourceData)[7] <- "寬"
names(sourceData)[8] <- "高"
names(sourceData)[9] <- "種類"
names(sourceData)[10] <- "葷素"
names(sourceData)[11] <- "重量" #重新命名欄位名稱

sourceData$'單價' <- as.integer(sourceData$'單價')
sourceData$'體積' <- as.numeric(sourceData$'體積')
sourceData$'廠牌' <- as.factor(sourceData$'廠牌')
sourceData$'種類' <- as.factor(sourceData$'種類')
sourceData$'葷素' <- as.factor(sourceData$'葷素')
sourceData$'重量' <- as.numeric(sourceData$'重量')

goodData <- sourceData #將原始資料複製一份
goodData <- cbind(goodData, "Selected" = 0, "Preference" = 1) #新增被選擇欄位


#----環境參數設定----
maxVolume <- 47*32*39 #最大箱子體積
maxWeight <- 16000 #最大重量
alpha <- 0.936 #體積鬆弛因子
popAmount <- 30 #人口數量
crossRate <- 0.7 #交配率
mutationRate <- 0.01 #突變率
eliteValues <- 1 #菁英數量
maxGen <- 500 #世代次數

#----Function----
#偏好值與類別合併
preference_match <- function(good_data, require_goods, non_require_goods, user_preference) {
  total_list <- as.character(c(require_goods, non_require_goods)) #將必需性商品與選擇性商品類別合併
  good_preference <- data.frame(category = total_list, preference = c(sample(1, length(require_goods), replace = TRUE), user_preference)) #將商品類別和偏好值合併為DF型態

  for (i in 1:dim(good_preference)[1]) {
    # good_data[good_data$種類==good_preference$category[i],]$Preference <- as.numeric(good_preference$preference[i])^2
    good_data[good_data$種類==good_preference$category[i],]$Preference <- as.numeric(good_preference$preference[i])
  }

  return(good_data)
}


#剔除品牌的方法
except_brand <- function(good_data, except_brand_list) {
  #good_data: 原始商品資料集
  #except_brand_list: 剔除品牌的名稱

  for (i in 1:length(except_brand_list)) {
    good_data <- good_data[good_data$'廠牌'!=except_brand_list[i],] #將要剔除的廠牌移除
  }
  return(good_data)
}


#葷素食的選擇
diet_select <- function(good_data, diet_habit_list) {
  #good_data: 原始商品資料集
  #diet_habit_list: 葷食或素食的選擇

  if(diet_habit_list=="素食") {
    good_data <- good_data[good_data$'葷素'==diet_habit_list,] #如果是素食就將屬於素食的產品篩選出來
    good_data$'種類' <- factor(good_data$種類)
  }

  return(good_data)
}


#初始人口方法3(無素食, 選擇性商品為參數, 重量為硬性條件)
initial_pop <- function(good_data, require_goods, non_require_goods, non_require_values, limit_weight) {
  #good_data: 原始商品資料集
  #require_goods: 必要性的商品清單
  #non_require_goods: 不必要性的商品清單
  #non_require_values: 不必要性的商品數量
  #limit_weight: 最大重量限制

  temp_good <- good_data #先將原始資料暫時給另外一個變數使用

  for (i in 1:length(require_goods)) {
    get_index <- sample(which(temp_good$'種類'==require_goods[i] & temp_good$'Selected'!=1), 1) #隨機抓出符合種類並Selected欄位不等於1的列
    temp_good$'Selected'[get_index] <- 1 #將被選擇的欄位改為1
  }

  for (i in 1:non_require_values) {
    category_goods <- sample(non_require_goods, 1) #隨機挑選選擇性商品的類別
    get_index <- sample(which(temp_good$'種類'==category_goods & temp_good$'Selected'!=1), 1) #隨機抓出符合種類並Selected欄位不等於1的列
    temp_good$'Selected'[get_index] <- 1 #將被選擇的欄位改為1
  }

  selected_good <- temp_good[temp_good$'Selected'==1,] #將被選擇的商品放入新變數中
  sum_weight <- sum(selected_good$'重量') #計算此基因的總重量

  while (sum_weight > limit_weight) {
    #重量的硬性條件
    #先開始判別第一次產生的初始人口總重量有沒有超過預先的最大值, 若有就繼續隨機選擇商品出來做初始人口, 直到總重量沒有大於最大值

    print(paste("重量超過:", sum_weight, ">", limit_weight))
    temp_good <- good_data #重新給一次原始資料, 因selected欄位有被修改過

    for (i in 1:length(require_goods)) {
      get_index <- sample(which(temp_good$'種類'==require_goods[i] & temp_good$'Selected'!=1), 1) #隨機抓出符合種類並Selected欄位不等於1的列
      while (dim(temp_good[temp_good$'種類'==require_goods[i],])[1]==1) {
        #若該種類的商品數量等於1, 則重新隨機選擇其他類別
        get_index <- sample(which(temp_good$'種類'==require_goods[i] & temp_good$'Selected'!=1), 1) #隨機抓出符合種類並Selected欄位不等於1的列
      }
      temp_good$'Selected'[get_index] <- 1 #將被選擇的欄位改為1
    }

    for (i in 1:non_require_values) {
      category_goods <- sample(non_require_goods, 1) #隨機挑選選擇性商品的類別
      while (dim(temp_good[temp_good$'種類'==category_goods,])[1]==1) {
        #如果取得到的位置該商品只有一個且已經被選擇, 沒有商品可選時, 再重新挑選種跟商品
        remove_index <- which(non_require_goods==category_goods)
        category_goods <- sample(non_require_goods[-remove_index], 1) #隨機挑選選擇性商品的類別
      }
      get_index <- sample(which(temp_good$'種類'==category_goods & temp_good$'Selected'!=1), 1) #隨機抓出符合種類並Selected欄位不等於1的列
      temp_good$'Selected'[get_index] <- 1 #將被選擇的欄位改為1
    }

    selected_good <- temp_good[temp_good$'Selected'==1,] #將被選擇的商品重新放入變數中
    sum_weight <- sum(selected_good$'重量') #計算此基因的總重量
  }

  return(selected_good) #回傳結果
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
total_weight <- function(gene_list) {
  #gene_list: 被選擇出的基因清單
  for(i in 1:length(gene_list)) {
    sum_weight <- sum(gene_list[[i]][[1]]$'重量')
    gene_list[[i]]["totalWeight"] <- list(sum_weight)
  }
  return(gene_list)
}


#偏好的適應度方法(已加入懲罰值)
fitness_preference <- function(gene_list, require_goods, non_require_values) {
  #被選擇出的基因清單
  print("開始計算偏好適應函數...")
  for(i in 1:length(gene_list)) {
    reuslt <- 1
    for (k in 1:sum(length(require_goods), non_require_values)) {
      temp_preferencd <- 1+as.numeric((geneList[[i]][[1]]$'Preference'[k])^2 - 1) / sum(1:non_require_values) #偏好的計算公式
      reuslt <- reuslt*temp_preferencd
    }
    gene_list[[i]]["fitPreference"] <- list(reuslt)
    print(paste("偏好適應函數:", reuslt))
    print(paste("========第", i, "個基因========"))
  }
  return(gene_list)
}


#體積的適應度方法(已加入懲罰值)
fitness_volume <- function(gene_list, bin_volume, volume_alpha) {
  #gene_list: 被選擇出的基因清單
  #bin_volume: 箱子的乘積
  #volume_alpha: 箱子的體積鬆弛因子
  print("開始計算體積適應函數...")
  for (i in 1:length(gene_list)) {
    limit_volume <- bin_volume*volume_alpha #體積與鬆弛因子相乘
    sum_volume <- sum(gene_list[[i]][[1]]$'體積') #將最大限制體積減去每個基因的總體積
    reuslt <- (limit_volume-sum_volume)/limit_volume #將體積適應度算出
    if (reuslt==0 || reuslt>0 && reuslt<=0.1) {
      reuslt <- reuslt + 1 #若適應度等於0或大於0並小於等於0.1就給予懲罰值1, e.g. (49795.2-27749.25)/49795.2=0.4427324, 愈接近0表示價格差距越小
    } else if (reuslt>0.1 && reuslt<=0.5) {
      reuslt <- reuslt + 2 #若適應度大於0.1並小於等於0.5就給予懲罰值2
    } else {
      reuslt <- reuslt + 3 #剩下結果將給予懲罰值3
    }
    gene_list[[i]]["fitVolume"] <- reuslt
    print(paste("體積適應函數:", reuslt))
    print(paste("========第", i, "個基因========"))
  }
  return(gene_list)
}


#價格的適應度方法(已加入懲罰值)
fitness_price <- function(gene_list, limit_price) {
  #gene_list: 被選擇出的基因清單
  #limit_price: 價格最高限制
  print("開始計算價格適應函數...")
  for (i in 1:length(gene_list)) {
    sum_price <- sum(gene_list[[i]][[1]]$'單價') #將最大限制金額減去每個基因的總金額
    reuslt <- (limit_price-sum_price)/limit_price #將價格適應度算出
    if (reuslt==0 || reuslt>0 && reuslt<=0.1) {
      reuslt <- reuslt + 1 #若適應度等於0或大於0並小於等於0.1就給予懲罰值1, e.g. (1200-1123)/1200=0.06416667, 愈接近0表示價格差距越小
    } else if (reuslt>0.1 && reuslt<=0.5) {
      reuslt <- reuslt + 2 #若適應度大於0.1並小於等於0.5就給予懲罰值2
    } else {
      reuslt <- reuslt + 3 #剩下結果將給予懲罰值3
    }
    gene_list[[i]]["fitPrice"] <- reuslt
    print(paste("價格適應函數:", reuslt))
    print(paste("========第", i, "個基因========"))
  }
  return(gene_list)
}


#總體的適應度方法
fitness_total <- function(gene_list) {
  #gene_list: 被選擇出的基因清單

  print("開始計算總體適應函數...")
  sum_fit <- unlist(lapply(fitnessPriceAfter, function(x) x$fitVolume*x$fitPrice))

  for (i in 1:length(gene_list)) {
    sum_fit <- gene_list[[i]]$'fitVolume'*gene_list[[i]]$'fitPrice'*gene_list[[i]]$'fitPreference'

    gene_list[[i]]["totalFit"] <- sum_fit
    print(paste("總體適應函數:", sum_fit))
    print(paste("========第", i, "個基因========"))
  }
  return(gene_list)
}


#交配(雙點交配)-需考慮適應函數值(包含懲罰值)、交配率和重量限制
cross_over <- function(gene_list, require_goods, non_require_values, cross_rate, limit_weight) {

  for (i in 1:length(gene_list)) {
    #先給予交配狀態, 0表示未交配, 1表示已交配
    gene_list[[i]]["crossState"] <- 0
  }
  get_chrom_length <- length(require_goods)+non_require_values #取得染色體長度
  print("開始交配")

  for(i in 1:c(length(gene_list)/2)){
    get_cross_state <- unlist(lapply(gene_list, function(x) x$crossState)) #給定目前交配狀態
    rnd_cross_rate <- round(runif(n = 1, min = 0, max = 1),3) #產生亂數
    get_index <- as.vector(sample(which(get_cross_state!=1),2)) #抽取要被交配的基因

    loop_value <- 0

    if(rnd_cross_rate<=cross_rate) {
      #亂數小於等於交配率, 則進行交配

      divide_index <- sort(as.vector(sample(get_chrom_length, 2))) #隨機選擇切割地方(採雙點交配)

      tempChrom_A <- gene_list[[get_index[1]]] #先將染色體給暫時變數A
      tempChrom_B <- gene_list[[get_index[2]]] #先將染色體給暫時變數B
      tempChrom_A$'chromosome'[(divide_index[1]+1):divide_index[2]] <- gene_list[[get_index[2]]]$'chromosome'[(divide_index[1]+1):divide_index[2]] #開始進行交配, 將第二個基因切割的染色體給第一個基因
      tempChrom_B$'chromosome'[(divide_index[1]+1):divide_index[2]] <- gene_list[[get_index[1]]]$'chromosome'[(divide_index[1]+1):divide_index[2]] #開始進行交配, 將第一個基因切割的染色體給第二個基因
      tempChrom_A[[1]][(divide_index[1]+1):divide_index[2],] <- gene_list[[get_index[2]]][[1]][(divide_index[1]+1):divide_index[2],] #開始進行交配, 將第二個基因切割的商品給第一個基因
      tempChrom_B[[1]][(divide_index[1]+1):divide_index[2],] <- gene_list[[get_index[1]]][[1]][(divide_index[1]+1):divide_index[2],] #開始進行交配, 將第一個基因切割的商品給第二個基因
      tempChrom_A$'totalWeight' <- sum(tempChrom_A[[1]]$'重量') #重新計算總重量
      tempChrom_B$'totalWeight' <- sum(tempChrom_B[[1]]$'重量') #重新計算總重量

      while (tempChrom_A$'totalWeight' > limit_weight || tempChrom_B$'totalWeight' > limit_weight && length(tempChrom_A$chromosome)!=length(unique(tempChrom_A$chromosome))) {
        loop_value <- loop_value+1

        print(paste("重量超過:", tempChrom_A$'totalWeight', "或", tempChrom_B$'totalWeight', ">", limit_weight, "或 有重複值"))

        get_index <- as.vector(sample(which(get_cross_state!=1),2)) #重新抽取要被交配的基因
        divide_index <- sort(as.vector(sample(get_chrom_length, 2))) #重新隨機選擇切割地方(採雙點交配)

        tempChrom_A <- gene_list[[get_index[1]]] #先將染色體給暫時變數A
        tempChrom_B <- gene_list[[get_index[2]]] #先將染色體給暫時變數B
        tempChrom_A$'chromosome'[(divide_index[1]+1):divide_index[2]] <- gene_list[[get_index[2]]]$'chromosome'[(divide_index[1]+1):divide_index[2]] #開始進行交配, 將第二個基因切割的染色體給第一個基因
        tempChrom_B$'chromosome'[(divide_index[1]+1):divide_index[2]] <- gene_list[[get_index[1]]]$'chromosome'[(divide_index[1]+1):divide_index[2]] #開始進行交配, 將第一個基因切割的染色體給第二個基因
        tempChrom_A[[1]][(divide_index[1]+1):divide_index[2],] <- gene_list[[get_index[2]]][[1]][(divide_index[1]+1):divide_index[2],] #開始進行交配, 將第二個基因切割的商品給第一個基因
        tempChrom_B[[1]][(divide_index[1]+1):divide_index[2],] <- gene_list[[get_index[1]]][[1]][(divide_index[1]+1):divide_index[2],] #開始進行交配, 將第一個基因切割的商品給第二個基因
        tempChrom_A$'totalWeight' <- sum(tempChrom_A[[1]]$'重量') #重新計算總重量
        tempChrom_B$'totalWeight' <- sum(tempChrom_B[[1]]$'重量') #重新計算總重量

        # if(loop_value > 500) {
        #   print("已成無窮解, 不進行交配")
        #   break
        # }
        temp_log <- as.numeric(table(tempChrom_A$'chromosome'==tempChrom_B$'chromosome')["TRUE"])

        if(temp_log == get_chrom_length) {
          print("跳出")
          break
        }
      }

      tempChrom_A$'crossState' <- 1
      tempChrom_B$'crossState' <- 1
      gene_list[[get_index[1]]] <- tempChrom_A
      gene_list[[get_index[2]]] <- tempChrom_B

      print("交配囉!")
      print(paste("========第", i, "對========"))
    } else {
      #亂數大於交配率, 則不進行交配
      gene_list[[get_index[1]]]$'crossState' <- 1
      gene_list[[get_index[2]]]$'crossState' <- 1
      print("沒交配!")
      print(paste("========第", i, "對========"))
    }
  }
  return(gene_list)
}


#突變方法, 加入重量限制
mutation_FN <- function(good_data, gene_list, mutation_rate, require_goods, non_require_values, soure_data, limit_weight) {
  #good_data: 商品資料集
  #gene_list: 已交配過的基因人口群
  #mutation_rate: 交配率
  #require_goods: 必要性商品清單
  #non_require_values: 選擇性商品數量
  #soure_data: 原始資料(未經過修改的)

  get_chrom_length <- length(require_goods)+non_require_values #基因染色體的長度
  temp_list <- gene_list #先將資料給另外一個變數
  print("開始突變")

  for(i in 1:length(gene_list)) {
    mutation_index <- as.numeric(sample(get_chrom_length, 1)) #隨機取得要突變的位置
    rnd_mutation_rate <- runif(n = 1, min = 0, max = 1) #產生亂數
    print(paste("亂數突變率:", rnd_mutation_rate)) #顯示亂數的突變率

    if(rnd_mutation_rate <= mutation_rate){
      mutation_category <- as.character(temp_list[[i]][[1]][mutation_index,]$'種類') #取得基因中要被突變的染色體商品種類
      mutation_list <- soure_data[soure_data$'種類'==mutation_category,] #取得原始資料中符合要被突變的商品種類
      temp_good_data <- good_data[good_data$'種類'==mutation_category, ] #取得資料集該種類的資料

      repeat {
        #重複篩選商品, 直到沒有與原本基因相同的商品
        rnd_mutation_value <- sample(nrow(mutation_list), 1) #隨機取得該種類的商品

        comput_weight_list <- temp_list[[i]][[1]] #暫時將處理中的資料給另一變數
        comput_weight_list[mutation_index,] <- mutation_list[rnd_mutation_value,] #將取得到突變的位置進行突變
        sum_weight <- sum(comput_weight_list$'重量') #計算突變後總重量

        if(temp_list[[i]][[1]][mutation_index,]$'產品代號'!=mutation_list[rnd_mutation_value,]$'產品代號' && sum_weight < limit_weight && dim(temp_good_data)[1]!=1){
          #當商品兩者不同時, 並突變後的重量不可大於最大重量限制, 且該突變位置的商品種類數量不等於1, 則跳出, 表示已拿到非重複的商品, 且總重量沒有超過限制, 及該商品種類在剔除品牌後數量不等於1
          break
        }
        print(paste("商品重複且重量超過:", sum_weight, "<", limit_weight))
        mutation_index <- as.numeric(sample(get_chrom_length, 1)) #隨機取得要突變的位置
        mutation_category <- as.character(temp_list[[i]][[1]][mutation_index,]$'種類') #取得基因中要被突變的染色體商品種類
        mutation_list <- soure_data[soure_data$'種類'==mutation_category,] #取得原始資料中符合要被突變的商品種類
        temp_good_data <- good_data[good_data$'種類'==mutation_category, ] #取得資料集該種類的資料
      }
      temp_list[[i]][[1]][mutation_index,] <- mutation_list[rnd_mutation_value,] #將商品進行變異
      temp_list[[i]]$'chromosome'[mutation_index] <- as.character(mutation_list[rnd_mutation_value,]$'產品代號') #將基因進行變異
      temp_list[[i]]$'totalWeight' <- sum_weight

      print("完成突變!")
    } else {
      print(paste(rnd_mutation_rate, ">=", mutation_rate)) #顯示目前突變率
    }
    print(paste("========第", i, "次========"))
  }
  return(temp_list)
}


#此代與下代合併的方法, 並挑選出新的族群(採用菁英政策)
new_population <- function(first_gene, second_gene, elite_values, pop_amount) {
  #first_gene: 此代基因
  #second_gene: 下一代基因
  #elite_values: 菁英數量
  #pop_amount: 族群大小

  new_pop <- list()
  new_pop <- first_gene #將此代基因放入新的變數
  new_pop <- append(new_pop, second_gene) #將下代基因加入變數

  for (i in 1:length(new_pop)) {
    #產生菁英狀態
    new_pop[[i]]["elite"] <- 0 #將菁英狀態皆設定為0, 0表示不是菁英值; 1表示被挑選為菁英值
  }

  result_pop <- list() #初始最終結果族群清單

  last_list <- head(order(unlist(lapply(new_pop, function(x) x$totalFit)), decreasing = FALSE), pop_amount) #取得popAmount(數量)的族群
  for (z in 1:length(elite_values)) {
    #將最好的適應函數設定為精英值, 並放入新的群組
    new_pop[[last_list[z]]]$'elite' <- 1
    result_pop[[z]] <- new_pop[[last_list[z]]] #將菁英的基因放入新的族群中
    print(paste("菁英編碼:", unlist(result_pop[[z]]$'chromosome')))
    print(paste("菁英的總體適應函數:", result_pop[[z]]$'totalFit'))
  }

  for (k in (length(elite_values)+1):pop_amount) {
    #將剩下的基因加入到族群
    result_pop[[k]] <- new_pop[[last_list[k]]]
  }

  return(result_pop)
}


#----執行----
#葷素的方法
goodData <- diet_select(good_data = goodData, diet_habit_list = dietHabit)
level <- levels(goodData$'種類')
level <- level[order(nchar(level), level)]
#levels(goodData$'種類') <- level[order(nchar(level), level)]
#goodData$'種類' <- factor(goodData$'種類', levels = level)
requiredList <- level[order(nchar(level), level)][1:6]
#requiredList <- level[order(nchar(level), level)][1:6]
if (dietHabit == "葷食") {
  nonRequiredList <- c("冬粉與炊粉", "麵條", "沖泡飲", "罐頭_瓜", "罐頭_魚", "罐頭_筍菇", "罐頭_肉醬_多入裝", "罐頭_麵筋_多入裝", "飲料_汽水_家庭號", "飲料_甜品_多入裝", "泡麵_家庭號", "餅乾_堅果海苔", "餅乾_組合包", "餅乾_蘇打餅", "餅乾_洋芋片", "餅乾_中西小點")
} else {
  nonRequiredList <- c("冬粉與炊粉", "麵條", "沖泡飲", "罐頭_瓜", "罐頭_筍菇", "罐頭_麵筋_多入裝", "飲料_汽水_家庭號", "飲料_甜品_多入裝", "餅乾_堅果海苔", "餅乾_組合包", "餅乾_洋芋片", "餅乾_中西小點")
}
#nonRequiredList <- level[order(nchar(level), level)][-1:-length(requiredList)]
nonRequiredValues <-  length(nonRequiredList) #選擇性商品的數量
#userPreference <- sample(c(1:length(nonRequiredList)), length(nonRequiredList), replace = FALSE)

goodData <- preference_match(good_data = goodData, require_goods = requiredList, non_require_goods = nonRequiredList, user_preference = userPreference)

#剔除掉不想要的品牌
goodData <- except_brand(good_data = goodData, except_brand_list = exceptBrandList)

#產生初始口(遵照popAmount數量)
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
fitnessPreference <- list()
fitnessPreference <- fitness_preference(gene_list = geneList, require_goods = requiredList, non_require_values =  nonRequiredValues)

#計算體積適應度
fitnessVolumeAfter <- list()
fitnessVolumeAfter <- fitness_volume(gene_list = fitnessPreference, bin_volume = maxVolume, volume_alpha = alpha)

#計算價格適應度
fitnessPriceAfter <- list()
fitnessPriceAfter <- fitness_price(gene_list = fitnessVolumeAfter, limit_price = maxPrice)

#計算總體適應度
fitnessTotalAfter <- list()
fitnessTotalAfter <- fitness_total(gene_list = fitnessPriceAfter)

#開始進行交配
crossAfter <- list()
crossAfter <- cross_over(gene_list = fitnessTotalAfter, require_goods = requiredList, non_require_values = nonRequiredValues, cross_rate = crossRate, limit_weight = maxWeight)

#開始進行突變
mutationAfter <- list()
mutationAfter <- mutation_FN(good_data = goodData, gene_list = crossAfter, mutation_rate = mutationRate, require_goods = requiredList, non_require_values = nonRequiredValues, soure_data = goodData, limit_weight = maxWeight)

#計算新一代基因的價格及體積的適應函數
mutationAfter <- fitness_volume(gene_list = mutationAfter, bin_volume = maxVolume, volume_alpha = alpha)
mutationAfter <- fitness_price(gene_list = mutationAfter, limit_price = maxPrice)
mutationAfter <- fitness_total(gene_list = mutationAfter)


#到此位置已經有第一代與第二代的基因
#初始基因為fitnessPriceAfter
#第二代基因為mutationAfter

#合併此代與下一代基因, 並採用菁英政策和產出新的族群
newPopulation <- new_population(first_gene = fitnessTotalAfter, second_gene = mutationAfter, elite_values = eliteValues, pop_amount = popAmount)


gen_values_best <- vector() #紀錄最好的基因總體適應函數
gen_values_loss <- vector() #紀錄最差的基因總體適應函數
for (i in 1:maxGen) {
  #開始進行基因演算
  crossAfter <- list()
  crossAfter <- cross_over(gene_list = newPopulation, require_goods = requiredList, non_require_values = nonRequiredValues, cross_rate = crossRate, limit_weight = maxWeight)

  mutationAfter <- list()
  mutationAfter <- mutation_FN(good_data = goodData, gene_list = crossAfter, mutation_rate = mutationRate, require_goods = requiredList, non_require_values = nonRequiredValues, soure_data = goodData, limit_weight = maxWeight)

  mutationAfter <- fitness_volume(gene_list = mutationAfter, bin_volume = maxVolume, volume_alpha = alpha)
  mutationAfter <- fitness_price(gene_list = mutationAfter, limit_price = maxPrice)
  mutationAfter <- fitness_total(gene_list = mutationAfter)

  newPopulation <- new_population(first_gene = newPopulation, second_gene = mutationAfter, elite_values = eliteValues, pop_amount = popAmount)

  gen_values_best[i] <- newPopulation[[1]]$totalFit
  gen_values_loss[i] <- newPopulation[[20]]$totalFit
  print(paste("============第", i, "代============"))
}

nowDateTime <- unlist(strsplit(as.character(Sys.time()), split = " ")) #切割日期與時間
resultDF <- newPopulation[[1]][[1]][-12] #移除不必要欄位
resultDF <- cbind(resultDF, 日期戳記 = nowDateTime[1], 時間戳記 = nowDateTime[2]) #增加時間的戳記

mydb = dbConnect(MySQL(), user="root", password="", dbname="rpreferdatabase", host='127.0.0.1')
dbSendQuery(mydb,"SET NAMES big5")
dbWriteTable(mydb, name = "history", value = resultDF, append = TRUE, field.types=list(產品代號 = "varchar(100)", 品名 = "varchar(100)", 單價 = "int(100)", 體積 = "double(10,2)", 廠牌 = "varchar(100)", 長 = "double(10,2)", 寬 = "double(10,2)", 高 = "double(10,2)", 種類 = "	varchar(100)", 葷素 = "	varchar(100)", 重量 = "int(100)", Preference = "int(100)", 日期戳記 = "varchar(100)", 時間戳記 = "varchar(100)"), row.names = FALSE) #資料庫編碼請設定big5_chinese_ci
dbDisconnect(mydb)

#Rscript rBinPackingProblem.r "葷食" 22 1300-1599 "大同" 11 2 8 13 5 16 1 9 10 3 6 7 14 4 12 1

#write.csv(x = newPopulation[[1]][[1]][-12], file = "solution.csv", row.names = FALSE)