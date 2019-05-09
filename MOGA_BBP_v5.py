import datetime
import pandas as pd
import numpy as np
import random
import copy

#----時間紀錄(開始)----
startTime = datetime.datetime.now()

#----資料初始化(本地端)----
sourceData = pd.read_csv('assets_py/StoreData.csv') #讀取原始資料
preferenceTable = pd.read_csv('assets_py/preferenceTable.csv') #讀取商品偏好表
sourceData.drop(['序', '輸入格式(序,產品代碼+品名,單價,體積,種類)'], axis = 1, inplace=True) #移除不必要的資料欄位
sourceData.rename(index=str, columns={"重量(g)": "重量"}, inplace=True) #重新命名欄位名稱

goodData = copy.copy(sourceData) #將原始資料複製一份
goodData['Selected'], goodData['Preference'] = 0, 1 #新增被選擇欄位

#----環境參數設定----
maxVolume = 47*32*39 #最大箱子體積
maxWeight = 16000 #最大重量(g)
popAmount = 20 #人口數量
crossRate = 0.9 #交配率
mutationRate = 0.2 #突變率
eliteValues = round(popAmount*0.1) #菁英數量
maxGen = 100 #世代次數

#----使用者需輸入的參數(假設)----
dietHabit = '葷食' #葷食與素食的選擇
userItemValues = 22 #使用者需要的數量
maxPrice = 1500 #使用者金額
exceptBrandList = ['大同'] #將要剔除的品牌

#----Function----
#飲食習慣(葷或素食):
#在開始演算法前先將該飲食習慣給加入
#若為葷食則包含素食和葷食, 反之只有素食
def diet_select(good_data, diet_habit_list):
    #good_data: 原始商品資料集
    #diet_habit_list: 葷食或素食的選擇
    if diet_habit_list=='素食':
        good_data = good_data[good_data['葷素']==diet_habit_list]
    return(good_data)

#剔除品牌的方法:
#在開始演算法前先將該品牌給移除
def except_brand(good_data, except_brand_list):
    #good_data: 原始商品資料集
    #except_brand_list: 剔除品牌的名稱
    for i in range(len(except_brand_list)):
        good_data = goodData[goodData['廠牌']!=except_brand_list[i]] #將要剔除的廠牌移除
    good_data.index = range(len(good_data))
    return(good_data)

#偏好值與類別合併:
#將使用者對商品種類的偏好與原始商品資料進行合併成一個Data Frame, 使原始資料有使用者對每個商品的品項偏好
def preference_match(good_data, preference_table):
    #gene_list: 被選擇出的基因清單
    #require_goods: 必要性的商品清單
    #non_require_goods: 不必要性的商品清單
    #user_preference: 使用者對商品種類的偏好
    for i in range(len(preference_table)):
      good_data.loc[good_data['種類']==preference_table['category'][i],'Preference'] = preference_table['preference'][i]
    return(good_data)

def initial_pop(good_data, require_goods, non_require_goods, non_require_values, limit_weight):
    #good_data: 原始商品資料集
    #require_goods: 必要性的商品清單
    #non_require_goods: 不必要性的商品清單
    #non_require_values: 不必要性的商品數量
    #limit_weight: 最大重量限制
    while True:
        temp_good = copy.copy(good_data) #先將原始資料暫時給另外一個變數使用    
        for i in range(len(require_goods)):
            get_index = (temp_good['Selected'][(temp_good['種類']==require_goods[i]) & (temp_good['Selected']!=1)]).index.tolist() #取得符合條件的資料, 並轉為list型態
            get_index = int(random.sample(get_index, 1)[0]) #隨機取得index, 並轉為整數型態
            temp_good['Selected'][get_index] = 1
    
        selected_require = (non_require_goods.sample(non_require_values)).tolist()
        
        for i in range(len(selected_require)):
            get_index = (temp_good['Selected'][(temp_good['種類']==selected_require[i]) & (temp_good['Selected']!=1)]).index.tolist() #取得符合條件的資料, 並轉為list型態
            get_index = int(random.sample(get_index, 1)[0]) #隨機取得index, 並轉為整數型態
            temp_good['Selected'][get_index] = 1

        selected_good = temp_good.loc[(temp_good['Selected']==1)]
        sum_weight = selected_good['重量'].sum()
        selected_good.index = range(len(selected_good))
        if sum_weight <= limit_weight:
            break
    return(selected_good) #回傳結果

#編碼染色體:
#必要性商品必定方在最前段, 選擇性商品必定放在後段
def create_chromosome(gene_list):
    #gene_list: 被選擇出的基因清單
    for i in range(len(gene_list)):
        chromosome = (geneList[i]['data.frame']['產品代號']).tolist()
        geneList[i]['chromosome'] = chromosome
    return(gene_list)

#計算總重量
def total_weight(gene_list):
    #gene_list: 被選擇出的基因清單
    for i in range(len(gene_list)):
        sum_weight = (geneList[i]['data.frame']['重量']).sum()
        geneList[i]['totalWeight'] = sum_weight
    return(gene_list)

#偏好的適應度方法(算式分母為偏好值1~偏好的最大值)
def fitness_preference(gene_list, require_goods, non_require_values, preference_table):
    #gene_list: 被選擇出的基因清單
    #require_goods: 必要性的商品清單
    #non_require_goods: 不必要性的商品清單
    #user_preference: 使用者對商品種類的偏好
    
    max_preference = preferenceTable['preference'].max()
    total_preference = 0
    for i in range(0, max_preference+1, 1):
        total_preference = total_preference+i**2
    
    for i in range(len(gene_list)):
        reuslt = 1
        for j in range((len(require_goods) + non_require_values)):
            temp_preferenced = 1+(((gene_list[i]['data.frame']['Preference'][j]**2)-1) / total_preference)
            reuslt = reuslt*temp_preferenced
            #temp.append(reuslt)
        
        geneList[i]['fitPreference'] = reuslt
        sum_preferenced = (gene_list[i]['data.frame']['Preference']).sum()
        geneList[i]['totalPreference'] = sum_preferenced
    return(gene_list)

#體積的適應度方法(已加入懲罰值)
def fitness_volume(gene_list, bin_volume):
    #gene_list: 被選擇出的基因清單
    #bin_volume: 箱子的乘積
    for i in range(len(gene_list)):
        sum_volume = (geneList[i]['data.frame']['體積']).sum() #將最大限制體積減去每個基因的總體積
        subtraction_volume = bin_volume-sum_volume #容積上限與選擇商品之總體積的差額
        reuslt = abs(subtraction_volume)/bin_volume #將體積適應度算出
        
        if (sum_volume >=(bin_volume*0.7)) & (sum_volume <=bin_volume):
            if subtraction_volume==0:
                reuslt = reuslt + 1 #若適應度等於0就給予懲罰值1, e.g. (49795.2-27749.25)/49795.2=0.4427324, 愈接近0表示價格差距越小
            reuslt = reuslt + 2 #若適應度大於0就給予懲罰值2
        reuslt = reuslt + 3
        
        geneList[i]['fitVolume'] = reuslt
    return(gene_list)

#價格的適應度方法(已加入懲罰值)
def fitness_price(gene_list, limit_price):
    #gene_list: 被選擇出的基因清單
    #limit_price: 價格最高限制
    for i in range(len(gene_list)):
        sum_price = (geneList[i]['data.frame']['單價']).sum() #將最大限制金額減去每個基因的總金額
        subtraction_price = limit_price-sum_price #預算與商品組合之總價格的差額
        reuslt = abs(subtraction_price)/limit_price #將價格適應度算出
        
        if subtraction_price==0:
            reuslt = reuslt + 1
        elif subtraction_price>0:
            reuslt = reuslt + 2
        else:
            reuslt = reuslt + 3
            
        geneList[i]['fitPrice'] = reuslt
        geneList[i]['totalPrice'] = sum_price
    return(gene_list)

#總體的適應度方法
def fitness_total(gene_list):
    #gene_list: 被選擇出的基因清單
    for i in range(len(gene_list)):
        sum_fit = fitnessPriceAfter[i]['fitPrice']*fitnessPriceAfter[i]['fitVolume']*fitnessPriceAfter[i]['fitPreference']
        geneList[i]['totalFit'] = sum_fit
    return(gene_list)

#選擇(競賽法):
#從父母中隨機挑選出兩個染色體, 這兩染色體互相比較總適應度, 越低者獲勝, 將被複製至交配池中, 直至交配池內的數量與人口數相同
def selection(gene_list, pop_amount):
    #gene_list: 被選擇出的基因清單
    #pop_amount: 人口數量
    result = []
    for i in range(pop_amount):
        compare_list = random.sample(gene_list, 2)
        if compare_list[0]['totalFit'] < compare_list[1]['totalFit']:
            result.append(compare_list[0])
        elif compare_list[0]['totalFit'] > compare_list[1]['totalFit']:
            result.append(compare_list[1])
        else:
            result.append(random.sample(compare_list, 1))
    return(result)

#交配(雙點交配)-需考慮適應函數值(包含懲罰值)、交配率和重量限制
def cross_over(good_data, gene_list, require_goods, non_require_goods, non_require_values, cross_rate):
    get_chrom_length = len(require_goods)+non_require_values #取得染色體長度
    for i in range(len(gene_list)):
        gene_list[i]['crossState'] = 0 #先給予交配狀態, 0表示未交配, 1表示已交配
    
    for i in range(int(gene_list/2)):
        get_cross_state = []
        for j in range(len(gene_list)):
            get_cross_state.append(j)
            
        get_index = random.sample(get_cross_state, 2) #抽取要被交配的基因
        rnd_cross_rate = round(random.random(), 3) #產生亂數
        if rnd_cross_rate<=cross_rate:
            divide_index = sorted(random.sample(range(0, get_chrom_length), 2)) #隨機選擇切割地方(採雙點交配)
            tempChrom_A = copy.copy(gene_list[get_index[0]]) #先將染色體給暫時變數A
            tempChrom_B = copy.copy(gene_list[get_index[1]]) #先將染色體給暫時變數B
            tempChrom_A['chromosome'][divide_index[0]:divide_index[1]] = copy.copy(gene_list[get_index[1]]['chromosome'][divide_index[0]:divide_index[1]]) #開始進行交配, 將第二個基因切割的染色體給第一個基因
            tempChrom_B['chromosome'][divide_index[0]:divide_index[1]] = copy.copy(gene_list[get_index[0]]['chromosome'][divide_index[0]:divide_index[1]]) #開始進行交配, 將第二個基因切割的染色體給第一個基因
            tempChrom_A['data.frame'].iloc[divide_index[0]:divide_index[1],:] = get_index[get_index[1]]['data.frame'].iloc[divide_index[0]:divide_index[1],:] #開始進行交配, 將第二個基因切割的商品給第一個基因
            tempChrom_B['data.frame'].iloc[divide_index[0]:divide_index[1],:] = get_index[get_index[0]]['data.frame'].iloc[divide_index[0]:divide_index[1],:] #開始進行交配, 將第二個基因切割的商品給第一個基因
            tempChrom_A['totalWeight'] = tempChrom_A['data.frame']['重量'].sum() #重新計算總重量
            tempChrom_B['totalWeight'] = tempChrom_B['data.frame']['重量'].sum() #重新計算總重量
        
        while True:
            
        
        
        
    
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


#----執行----
#葷素的方法
goodData = diet_select(good_data = goodData, diet_habit_list = dietHabit)

#剔除掉不想要的品牌
goodData = except_brand(good_data = goodData, except_brand_list = exceptBrandList)


level = preferenceTable['category']
requiredList = level[0:6]
nonRequiredList = level[len(requiredList):len(level)]
nonRequiredValues = userItemValues-len(requiredList) #選擇性商品的數量

goodData = preference_match(good_data = goodData, preference_table = preferenceTable)

##基因演算法開始
#產生初始口(遵照popAmount數量)
geneList = []
for i in range(popAmount):
    #geneList.append([])
    geneDict = {'data.frame': initial_pop(good_data = goodData, require_goods = requiredList, non_require_goods = nonRequiredList, non_require_values = nonRequiredValues, limit_weight = maxWeight)}
    #geneList[i].append(geneDict)
    geneList.append(geneDict)
    
#編碼染色體
geneList = create_chromosome(gene_list = geneList)

#計算每個基因總重量
geneList = total_weight(gene_list = geneList)

#計算偏好適應度(目前僅計算總偏好值)
fitnessPreference = fitness_preference(gene_list = geneList, require_goods = requiredList, non_require_values =  nonRequiredValues, preference_table = preferenceTable)

#計算體積適應度
fitnessVolumeAfter = fitness_volume(gene_list = fitnessPreference, bin_volume = maxVolume)

#計算價格適應度
fitnessPriceAfter = fitness_price(gene_list = fitnessVolumeAfter, limit_price = maxPrice)

#計算總體適應度
fitnessTotalAfter = fitness_total(gene_list = fitnessPriceAfter)

#選擇(競賽法)
selectionAfter = selection(gene_list = fitnessTotalAfter, pop_amount = popAmount)

#交配
#crossAfter = cross_over(good_data = goodData, gene_list = selectionAfter, require_goods = requiredList, non_require_goods = nonRequiredList,non_require_values = nonRequiredValues, cross_rate = crossRate)