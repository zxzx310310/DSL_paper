# -*- coding: utf-8 -*-
import numpy as np
from numba import vectorize, int64
from numba import guvectorize, int64
from numba import jit
from pyculib import rand as curand
#from numba import jit
import time
import pandas as pd
import random
import matplotlib.pyplot as plt
from matplotlib.font_manager import FontProperties
import copy
import os
from numba import cuda
os.chdir('D:/R.data/DSL_paper')

#----時間紀錄(開始)----
startTime = time.time()

#----資料初始化(本地端)----
sourceData = pd.read_csv('assets_py/StoreData.csv') #讀取原始資料
preferenceTable = pd.read_csv('assets_py/preferenceTable.csv') #讀取商品偏好表
sourceData.drop(['序', '輸入格式(序,產品代碼+品名,單價,體積,種類)'], axis = 1, inplace=True) #移除不必要的資料欄位
sourceData.rename(index=str, columns={"重量(g)": "重量"}, inplace=True) #重新命名欄位名稱
goodData = copy.copy(sourceData) #將原始資料複製一份
sourceData = sourceData.values

goodData['Selected'], goodData['Preference'] = 0, 1 #新增被選擇欄位
goodData = goodData.values #轉換成ndarry型態
preferenceTable = preferenceTable.values


#----環境參數設定----
maxVolume = 47*32*39 #最大箱子體積
maxWeight = 16000 #最大重量(g)
popAmount = 20 #人口數量
crossRate = 1 #交配率
mutationRate = 1 #突變率
eliteValues = round(popAmount*0.1) #菁英數量
maxGen = 10000 #世代次數

#----使用者需輸入的參數(假設)----
dietHabit = '葷食' #葷食與素食的選擇
userItemValues = 22 #使用者需要的數量
maxPrice = 1500 #使用者金額
exceptBrandList = np.array(['大同']) #將要剔除的品牌

#----Function----
#飲食習慣(葷或素食):
#在開始演算法前先將該飲食習慣給加入
#若為葷食則包含素食和葷食, 反之只有素食
#@jit
def diet_select(good_data, diet_habit_list):
    #good_data: 原始商品資料集
    #diet_habit_list: 葷食或素食的選擇
    if diet_habit_list=='素食':
        good_data = good_data[good_data[:, 9] == diet_habit_list]
    return(good_data)

#剔除品牌的方法:
#在開始演算法前先將該品牌給移除
#@jit
@jit(nopython=True)
def except_brand(good_data, except_brand_list):
    #good_data: 原始商品資料集
    #except_brand_list: 剔除品牌的名稱
    for i in range(len(except_brand_list)):
        good_data = good_data[good_data[:, 4] != except_brand_list[i]] #將要剔除的廠牌移除
    return(good_data)

#偏好值與類別合併:
#將使用者對商品種類的偏好與原始商品資料進行合併成一個Data Frame, 使原始資料有使用者對每個商品的品項偏好
#@vectorize(['int32(int32, int32)'], target='cuda')
#@guvectorize("int64[:], int64", "(n) -> ()")
#@jit
def preference_match(good_data, preference_table):
    #gene_list: 被選擇出的基因清單
    #require_goods: 必要性的商品清單
    #non_require_goods: 不必要性的商品清單
    #user_preference: 使用者對商品種類的偏好
    for i in range(len(preference_table)):
        temp = good_data[good_data[:, 8] == preference_table[:,0][i]]
        for j in range(len(good_data[good_data[:, 8] == preference_table[:,0][i]])):
            temp[:, 12][j] = preference_table[:,1][i]
        good_data[good_data[:, 8] == preference_table[:,0][i]] = temp
    return(good_data)



#@vectorize(['int64(int64, int64, int64, int64, int64)'], target='cuda')
#@guvectorize([(int64[:], int64[:], int64[:], int64, int64)], '(n), (n), (n), (), ()')
#@vectorize(['int64(int64[:], int64[:], int64[:], int64, int64)'], target='cuda')

def initial_pop(good_data, require_goods, non_require_goods, non_require_values, limit_weight):
    #good_data: 原始商品資料集
    #require_goods: 必要性的商品清單
    #non_require_goods: 不必要性的商品清單
    #non_require_values: 不必要性的商品數量
    #limit_weight: 最大重量限制
    while True:
#        temp_good = np.copy(good_data) #先將原始資料暫時給另外一個變數使用    
        temp_good = np.copy(good_data) #先將原始資料暫時給另外一個變數使用    
        for i in range(len(require_goods)):
            get_index = temp_good[(temp_good[:,8] == require_goods[i]) & (temp_good[:, 11] !=1)] #取得符合條件的資料
            temp_index = random.randint(0, (len(get_index)-1)) #隨機取得品項
            get_index = get_index[temp_index] #取得商品名稱
            get_index[11] = 1
            temp_good[temp_good[:,0] == get_index[0]] = get_index
    
        selected_require = np.random.choice(non_require_goods, non_require_values, replace=False)
        
        for i in range(len(selected_require)):
            get_index = temp_good[(temp_good[:,8] == selected_require[i]) & (temp_good[:, 11] !=1)] #取得符合條件的資料
            temp_index = random.randint(0, (len(get_index)-1)) #隨機取得品項
            get_index = get_index[temp_index] #取得商品名稱
            get_index[11] = 1
            temp_good[temp_good[:,0] == get_index[0]] = get_index

        selected_good = temp_good[temp_good[:, 11] == 1]
        sum_weight = np.sum(selected_good[:,10])
        if sum_weight <= limit_weight:
            break
    return(selected_good) #回傳結果

#編碼染色體:
#必要性商品必定方在最前段, 選擇性商品必定放在後段
#@jit
def create_chromosome(gene_list):
    #gene_list: 被選擇出的基因清單
    for i in range(len(gene_list)):
        chromosome = (gene_list[i]['data.frame'][:,0])     
        gene_list[i]['chromosome'] = chromosome
    return(gene_list)

#計算總重量
#@jit
def total_weight(gene_list):
    #gene_list: 被選擇出的基因清單
    for i in range(len(gene_list)):
        sum_weight = np.sum(gene_list[i]['data.frame'][:,10])
        gene_list[i]['totalWeight'] = np.array([sum_weight])
    return(gene_list)

#偏好的適應度方法(算式分母為偏好值1~偏好的最大值)
#@jit
def fitness_preference(gene_list, require_goods, non_require_values, preference_table):
    #gene_list: 被選擇出的基因清單
    #require_goods: 必要性的商品清單
    #non_require_goods: 不必要性的商品清單
    #user_preference: 使用者對商品種類的偏好
    
    max_preference = np.max(preference_table[:,1])
    total_preference = 0
    for i in range(0, max_preference+1, 1):
        total_preference = total_preference+i**2
    
    for i in range(len(gene_list)):
        reuslt = 1
        for j in range((len(require_goods) + non_require_values)):
            temp_preferenced = 1+(((gene_list[i]['data.frame'][:,12][j]**2)-1) / total_preference)
            reuslt *=temp_preferenced
            #temp.append(reuslt)
        
        gene_list[i]['fitPreference'] = np.array([reuslt])
        sum_preferenced = (gene_list[i]['data.frame'][:,12]).sum()
        gene_list[i]['totalPreference'] = np.array([sum_preferenced])
    return(gene_list)

#體積的適應度方法(已加入懲罰值)
#@jit    
def fitness_volume(gene_list, bin_volume):
    #gene_list: 被選擇出的基因清單
    #bin_volume: 箱子的乘積
    for i in range(len(gene_list)):
        sum_volume = np.sum(gene_list[i]['data.frame'][:,3]) #將最大限制體積減去每個基因的總體積
        subtraction_volume = bin_volume-sum_volume #容積上限與選擇商品之總體積的差額
        reuslt = abs(subtraction_volume)/bin_volume #將體積適應度算出
        
        if (sum_volume >=(bin_volume*0.7)) & (sum_volume <=bin_volume):
            if subtraction_volume==0:
                reuslt = reuslt + 1 #若適應度等於0就給予懲罰值1, e.g. (49795.2-27749.25)/49795.2=0.4427324, 愈接近0表示價格差距越小
            reuslt = reuslt + 2 #若適應度大於0就給予懲罰值2
        reuslt = reuslt + 3
        
        gene_list[i]['fitVolume'] = np.array([reuslt])
    return(gene_list)

#價格的適應度方法(已加入懲罰值)
#@jit
def fitness_price(gene_list, limit_price):
    #gene_list: 被選擇出的基因清單
    #limit_price: 價格最高限制
    for i in range(len(gene_list)):
        sum_price = np.sum(gene_list[i]['data.frame'][:,2]) #將最大限制金額減去每個基因的總金額
        subtraction_price = limit_price-sum_price #預算與商品組合之總價格的差額
        reuslt = abs(subtraction_price)/limit_price #將價格適應度算出
        
        if subtraction_price==0:
            reuslt = reuslt + 1
        elif subtraction_price>0:
            reuslt = reuslt + 2
        else:
            reuslt = reuslt + 3
            
        gene_list[i]['fitPrice'] = np.array([reuslt])
        gene_list[i]['totalPrice'] = np.array([sum_price])
    return(gene_list)

#總體的適應度方法
#@jit
def fitness_total(gene_list):
    #gene_list: 被選擇出的基因清單
    for i in range(len(gene_list)):
        sum_fit = gene_list[i]['fitPrice'][0]*gene_list[i]['fitVolume'][0]*gene_list[i]['fitPreference'][0]
        gene_list[i]['totalFit'] = np.array([sum_fit])
    return(gene_list)

#選擇(競賽法):
#從父母中隨機挑選出兩個染色體, 這兩染色體互相比較總適應度, 越低者獲勝, 將被複製至交配池中, 直至交配池內的數量與人口數相同
#@jit
def selection(gene_list, pop_amount):
    #gene_list: 被選擇出的基因清單
    #pop_amount: 人口數量
    result = []
    for i in range(pop_amount):
        compare_list = random.sample(gene_list, 2)
        if compare_list[0]['totalFit'][0] < compare_list[1]['totalFit'][0]:
            result.append(compare_list[0])
        elif compare_list[0]['totalFit'][0] > compare_list[1]['totalFit'][0]:
            result.append(compare_list[1])
        else:
            result.append(random.sample(compare_list, 1))
    return(result)
    
#選擇(競賽法)-2:
#從父母中隨機挑選出兩個染色體, 這兩染色體互相比較總適應度, 越低者獲勝, 將被複製至交配池中
#直至交配池內的數量與剩餘人口數(人口數-除菁英數量)相同
#@jit    
def selection_second(gene_list, pop_amount, elite_list):
    #gene_list: 被選擇出的基因清單
    #pop_amount: 人口數量
    #elite_list: 菁英清單
    result = []
    for i in range(pop_amount-len(elite_list)):
        compare_list = random.sample(gene_list, 2)
        if compare_list[0]['totalFit'][0] < compare_list[1]['totalFit'][0]:
            result.append(compare_list[0])
        elif compare_list[0]['totalFit'][0] > compare_list[1]['totalFit'][0]:
            result.append(compare_list[1])
        else:
            result.append(random.sample(compare_list, 1)[0])
    for i in range(len(elite_list)):
        result.append(elite_list[i])
    return(result)   


#交配(雙點交配)-需考慮適應函數值(包含懲罰值)、交配率和重量限制
def cross_over(good_data, gene_list, require_goods, non_require_values, cross_rate):
    get_chrom_length = len(require_goods)+non_require_values #取得染色體長度
    get_cross_index = [] #宣告一個放所有染色體index的陣列
    for j in range(len(gene_list)):
            get_cross_index.append(j) #把所有染色體的index放入
    
    for i in range(int(len(gene_list)/2)):
        get_index = random.sample(get_cross_index, 2) #抽取要被交配的基因
        rnd_cross_rate = round(random.random(), 3) #產生亂數
        if rnd_cross_rate<=cross_rate:
            divide_index = sorted(random.sample(range(0, get_chrom_length), 2)) #隨機選擇切割地方(採雙點交配)
            tempChrom_A = copy.deepcopy(gene_list[get_index[0]]) #先將染色體給暫時變數A
            tempChrom_B = copy.deepcopy(gene_list[get_index[1]]) #先將染色體給暫時變數B
            tempChrom_A['chromosome'][divide_index[0]:divide_index[1]] = copy.deepcopy(gene_list[get_index[1]]['chromosome'][divide_index[0]:divide_index[1]]) #開始進行交配, 將第二個基因切割的染色體給第一個基因
            tempChrom_B['chromosome'][divide_index[0]:divide_index[1]] = copy.deepcopy(gene_list[get_index[0]]['chromosome'][divide_index[0]:divide_index[1]]) #開始進行交配, 將第二個基因切割的染色體給第一個基因
            tempChrom_A['data.frame'][divide_index[0]:divide_index[1],:] = copy.deepcopy(gene_list[get_index[1]]['data.frame'][divide_index[0]:divide_index[1],:]) #開始進行交配, 將第二個基因切割的商品給第一個基因
            tempChrom_B['data.frame'][divide_index[0]:divide_index[1],:] = copy.deepcopy(gene_list[get_index[0]]['data.frame'][divide_index[0]:divide_index[1],:]) #開始進行交配, 將第二個基因切割的商品給第一個基因
            tempChrom_A['totalWeight'] = np.sum(tempChrom_A['data.frame'][:,10]) #重新計算總重量
            tempChrom_B['totalWeight'] = np.sum(tempChrom_B['data.frame'][:,10]) #重新計算總重量
        
            tempChrom_A_length = len(tempChrom_A['data.frame']) #取得原始長度
            tempChrom_A_category = np.unique(tempChrom_A['data.frame'][:,8]) #取得所有不重複的種類
            
            for k in range(len(tempChrom_A_category)):
                duplicate_index = np.where(tempChrom_A['data.frame'][:,8] == tempChrom_A_category[k])[0] #取得每個相對應種類的index
                if len(duplicate_index) >= 2:
                    tempChrom_A['data.frame'] = np.delete(tempChrom_A['data.frame'], duplicate_index[1:], axis = 0) #除了第一個商品不做刪除, 其餘皆刪除
            
            while len(tempChrom_A['data.frame']) < tempChrom_A_length:
                tempChrom_A_category = tempChrom_A['data.frame'][:,8] #抓出tempChrom_A中data frame的所有種類
                temp_df = copy.deepcopy(good_data) #將原始資料複製一份
                for j in range(len(tempChrom_A_category)):
                    temp_df = temp_df[temp_df[:,8]!=tempChrom_A_category[j]] #挑出tempChrom_A中沒有的種類品項
                    random_df_row = random.randint(0,(len(temp_df)-1)) #隨機取得沒有重複種類的品項
                    random_df_row = temp_df[random_df_row]
                tempChrom_A['data.frame'] = np.vstack((tempChrom_A['data.frame'], random_df_row)) #將隨機取出的資料放入染色體中
                tempChrom_A['data.frame'].sort(axis=0) #將資料按照產品代號排序
                tempChrom_A['chromosome'] = tempChrom_A['data.frame'][:,0] #重新將染色體編碼
                
            
            tempChrom_B_length = len(tempChrom_B['data.frame']) #取得原始長度
            tempChrom_B_category = np.unique(tempChrom_B['data.frame'][:,8]) #取得所有不重複的種類
            
            for k in range(len(tempChrom_B_category)):
                duplicate_index = np.where(tempChrom_B['data.frame'][:,8] == tempChrom_B_category[k])[0] #取得每個相對應種類的index
                if len(duplicate_index) >= 2:
                    tempChrom_B['data.frame'] = np.delete(tempChrom_B['data.frame'], duplicate_index[1:], axis = 0) #除了第一個商品不做刪除, 其餘皆刪除
            
            while len(tempChrom_B['data.frame']) < tempChrom_B_length:
                tempChrom_B_category = tempChrom_B['data.frame'][:,8] #抓出tempChrom_A中data frame的所有種類
                temp_df = copy.deepcopy(good_data) #將原始資料複製一份
                for j in range(len(tempChrom_B_category)):
                    temp_df = temp_df[temp_df[:,8]!=tempChrom_B_category[j]] #挑出tempChrom_A中沒有的種類品項
                    random_df_row = random.randint(0,(len(temp_df)-1)) #隨機取得沒有重複種類的品項
                    random_df_row = temp_df[random_df_row]
                tempChrom_B['data.frame'] = np.vstack((tempChrom_B['data.frame'], random_df_row)) #將隨機取出的資料放入染色體中
                tempChrom_B['data.frame'].sort(axis=0) #將資料按照產品代號排序
                tempChrom_B['chromosome'] = tempChrom_B['data.frame'][:,0] #重新將染色體編碼
                               
            gene_list[get_index[0]] = copy.copy(tempChrom_A) #將處理完畢的所有資料放回去
            gene_list[get_index[1]] = copy.copy(tempChrom_B) #將處理完畢的所有資料放回去
            get_cross_index.remove(get_index[0]) #刪除已交配完的染色體
            get_cross_index.remove(get_index[1]) #刪除已交配完的染色體
        else:
            get_cross_index.remove(get_index[0]) #刪除已交配完的染色體
            get_cross_index.remove(get_index[1]) #刪除已交配完的染色體
    return(gene_list)

#突變方法, 加入重量限制(突變部分直接隨機突變非必選的商品)
#@jit    
def mutation_FN(good_data, gene_list, mutation_rate):
    #good_data: 商品資料集
    #gene_list: 已交配過的基因人口群
    #mutation_rate: 交配率
    for i in range(len(gene_list)):
        rnd_mutation_rate = round(random.random(), 3) #產生亂數
        mutation_index = random.randint(0,(len(gene_list[i]['data.frame'])-1)) #隨機取得要突變的位置
        mutation_data = gene_list[i]['data.frame'][mutation_index] #取得該品項資料
        mutation_id = mutation_data[0] #取得該資料的產品代號
        if rnd_mutation_rate <= mutation_rate:
            mutation_category = mutation_data[8] #取得染色體中要被突變的基因商品種類
            temp_df = goodData[(goodData[:,8]==mutation_category) & (goodData[:,0]!=mutation_id)]
            temp_index = random.randint(0,(len(temp_df)-1)) #從商品資料中隨機取得符合該基因突變的index(不包含自己)
            temp_good = temp_df[temp_index] #取得商品資料
            gene_list[i]['data.frame'][mutation_index] = temp_good #將品項更換程新品項
            gene_list[i]['totalWeight'] = np.sum(crossAfter[i]['data.frame'][:,10]) #重新計算總重量
            gene_list[i]['chromosome'] = gene_list[i]['data.frame'][:,0] #重新將染色體編碼
    return(gene_list)
        
    
#氣泡排序法(遞增)
#@jit
def bubble_sort_flag(gene_list):
    for i in range(len(gene_list)-1):
        flag = False
        for j in range(len(gene_list)-1-i):
            if gene_list[j]['totalFit'] > gene_list[j+1]['totalFit']:
                flag = True
                tmp = gene_list[j]
                gene_list[j] = gene_list[j+1]
                gene_list[j+1] = tmp
        if flag == False:
            break
    return(gene_list)    


#將符合體重的群組合併起來
#@jit
def merge_population(first_gene, second_gene, limit_weight):
    new_pop = copy.deepcopy(first_gene) #將此代基因放入新的變數
    for i in range(len(second_gene)):
        new_pop.append(second_gene[i]) #將下代基因加入變數
    condition_pop = []
    for i in range(len(new_pop)):
        if new_pop[i]['totalWeight']<=limit_weight:
            condition_pop.append(new_pop[i])
    result_list = bubble_sort_flag(condition_pop)
    return(result_list)

#將精英群挑選出來
#@jit
def elite_population(merge_list, elite_pop):
    elite_list = merge_list[0:elite_pop]
    return(elite_list)

#新的下一代, 須刪除屬於菁英的染色體, 並且抓取符合群組數量
#@jit
def new_population(merge_list, elite_list, pop_amount):
    new_pop_list = merge_list[len(elite_list):(pop_amount+len(elite_list))] #刪除掉已經是屬於菁英的染色體
    return(new_pop_list)

#更新菁英群組
#@jit
def new_elite_population(old_elite_list, now_elite_list, elite_pop):
    for i in range(len(old_elite_list)):
        now_elite_list.append(old_elite_list[i]) #將舊的菁英與新的菁英合併
    now_elite_list = bubble_sort_flag(now_elite_list) #將菁英人口按照適應函數遞減排序
    new_elite = now_elite_list[0:elite_pop] #取得elite_pop(菁英數量)的成員
    return(new_elite)


#----執行----
#葷素的方法
goodData = diet_select(good_data = goodData, diet_habit_list = dietHabit)

#剔除掉不想要的品牌
goodData = except_brand(good_data = goodData, except_brand_list = exceptBrandList)


level = preferenceTable[:,0]
requiredList = level[0:6]
nonRequiredList = (level[len(requiredList):len(level)])
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
crossAfter = cross_over(good_data = goodData, gene_list = selectionAfter, require_goods = requiredList, non_require_values = nonRequiredValues, cross_rate = crossRate)

#突變
mutationAfter = mutation_FN(good_data = goodData, gene_list = crossAfter, mutation_rate = mutationRate)

#重新計算偏好適應函數, 體積適應函數, 價格適應函數
mutationAfter = fitness_preference(gene_list = copy.deepcopy(mutationAfter), require_goods = requiredList, non_require_values =  nonRequiredValues, preference_table = preferenceTable)
mutationAfter = fitness_volume(gene_list = copy.deepcopy(mutationAfter), bin_volume = maxVolume) 
mutationAfter = fitness_price(gene_list = copy.deepcopy(mutationAfter), limit_price = maxPrice)
mutationAfter = fitness_total(gene_list = copy.deepcopy(mutationAfter))

#將父母代與孩子合併
mergeList = merge_population(first_gene = fitnessTotalAfter, second_gene = mutationAfter, limit_weight = maxWeight)

#此代的菁英群組
latestElite = elite_population(merge_list = mergeList, elite_pop = eliteValues)

#新的下一代
newPopulation = new_population(merge_list = mergeList, elite_list = latestElite, pop_amount = popAmount)

gen_values_best = [] #紀錄最好的基因總體適應函數
gen_values_loss = [] #紀錄最差的基因總體適應函數
gen_price_best = []
gen_preference_best = []

gen_values_best.append(latestElite[0]['totalFit'])
gen_values_loss.append(newPopulation[popAmount-1]['totalFit']) #紀錄最差的總體適應函數
gen_price_best.append(latestElite[0]['totalPrice']) #紀錄最佳的總價格
gen_preference_best.append(latestElite[0]['totalPreference']) #紀錄最佳的總偏好
print(("============第 {0} 代============").format(1))

for i in range(1, maxGen):
    #選擇
    selectionAfter = selection_second(gene_list = newPopulation, pop_amount = popAmount, elite_list = latestElite)
    
    #交配
    crossAfter = cross_over(good_data = goodData, gene_list = selectionAfter, require_goods = requiredList, non_require_values = nonRequiredValues, cross_rate = crossRate)
    
    #突變
    mutationAfter = mutation_FN(good_data = goodData, gene_list = crossAfter, mutation_rate = mutationRate)
    
    #重新計算偏好適應函數, 體積適應函數, 價格適應函數
    mutationAfter = fitness_preference(gene_list = mutationAfter, require_goods = requiredList, non_require_values =  nonRequiredValues, preference_table = preferenceTable)
    mutationAfter = fitness_volume(gene_list = mutationAfter, bin_volume = maxVolume) 
    mutationAfter = fitness_price(gene_list = mutationAfter, limit_price = maxPrice)
    mutationAfter = fitness_total(gene_list = mutationAfter)
    
    #將父母代與孩子合併
    mergeList = merge_population(first_gene = newPopulation, second_gene = mutationAfter, limit_weight = maxWeight)
    
    #此代的菁英群組
    nowEliteLiet = elite_population(merge_list = mergeList, elite_pop = eliteValues)
    
    #更新舊代的菁英群組
    latestElite = new_elite_population(old_elite_list = latestElite, now_elite_list = nowEliteLiet, elite_pop = eliteValues)
    
    #下一代基因
    newPopulation = new_population(merge_list = mergeList, elite_list = latestElite, pop_amount = popAmount)
    
    gen_values_best.append(latestElite[0]['totalFit'][0])
    gen_values_loss.append(newPopulation[popAmount-1]['totalFit'][0]) #紀錄最差的總體適應函數
    gen_price_best.append(latestElite[0]['totalPrice'][0]) #紀錄最佳的總價格
    gen_preference_best.append(latestElite[0]['totalPreference'][0]) #紀錄最佳的總偏好
    print(("============第 {0} 代============").format(i+1))

resultTime = time.time()-startTime
print('花費時間: {0} {1}'.format(resultTime, '秒'))

plt.rcParams['font.sans-serif'] = ['Microsoft JhengHei'] 
plt.rcParams['axes.unicode_minus'] = False
plt.style.use('bmh')
plt.plot(gen_values_best, '-') #畫圖來顯示總體適應函數的起伏
plt.title('裝箱演算法')
plt.xlabel("世代數")
plt.ylabel("總體適應值")










#print(time.time() - startTime)