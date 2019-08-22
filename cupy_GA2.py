# -*- coding: utf-8 -*-
import numpy as np
import cupy as cp
#from numba import jit
import time
import pandas as pd
import random
import matplotlib.pyplot as plt
from matplotlib.font_manager import FontProperties
import copy
import os
os.chdir('D:/R.data/DSL_paper') #設定路徑

#----時間紀錄(開始)----
startTime = time.time()

#----資料初始化(本地端)----
sourceData = pd.read_csv('assets_cupy/StoreData.csv') #讀取原始資料
preferenceTable = pd.read_csv('assets_cupy/preferenceTable.csv') #讀取商品偏好表
sourceData.drop(['產品代號', '品名','輸入格式(序,產品代碼+品名,單價,體積,種類)'], axis = 1, inplace=True) #移除不必要的資料欄位
sourceData.rename(index=str, columns={"重量(g)": "重量"}, inplace=True) #重新命名欄位名稱
goodData = copy.copy(sourceData) #將原始資料複製一份
sourceData = sourceData.values 

goodData['Selected'], goodData['Preference'] = 0, 1 #新增被選擇欄位
#goodData = goodData.values #轉換成ndarry型態
#preferenceTable = preferenceTable.values 

#----環境參數設定----
maxVolume = 47*32*39 #最大箱子體積
maxWeight = 16000 #最大重量(g)
popAmount = 30 #人口數量
crossRate = 1 #交配率
mutationRate = 0.001#突變率
eliteValues = round(popAmount*0.1) #菁英數量
maxGen = 100 #世代次數

#----使用者需輸入的參數(假設)----
dietHabit = '葷食' #葷食與素食的選擇
userItemValues = 22 #使用者需要的數量
maxPrice = 1500 #使用者金額
exceptBrandList = np.array(['大同']) #將要剔除的品牌

#----Function----
#飲食習慣(葷或素食):
#在開始演算法前先將該飲食習慣給加入
#若為葷食則包含素食和葷食, 反之只有素食
def diet_select(good_data, diet_habit_list):
    #good_data: 原始商品資料集
    #diet_habit_list: 葷食或素食的選擇
    if diet_habit_list=='素食':
        good_data = good_data[good_data.iloc[:, 8] == diet_habit_list]
    return(good_data)

#剔除品牌的方法:
#在開始演算法前先將該品牌給移除
def except_brand(good_data, except_brand_list):
    #good_data: 原始商品資料集
    #except_brand_list: 剔除品牌的名稱
    for i in range(len(except_brand_list)):
        good_data = good_data[good_data.iloc[:,3] != exceptBrandList[i]] #將要剔除的廠牌移除
    return(good_data)

#偏好值與類別合併:
#將使用者對商品種類的偏好與原始商品資料進行合併成一個Data Frame, 使原始資料有使用者對每個商品的品項偏好
def preference_match(good_data, preference_table):
    #gene_list: 被選擇出的基因清單
    #require_goods: 必要性的商品清單
    #non_require_goods: 不必要性的商品清單
    #user_preference: 使用者對商品種類的偏好
    for i in range(len(preference_table)):
        temp = good_data[good_data.iloc[:, 7] == preference_table.iloc[:,0][i]]
        temp.loc[:, 'Preference'] = preference_table.iloc[:,1][i]
        good_data[good_data.iloc[:, 7] == preference_table.iloc[:,0][i]] = temp
    return(good_data)

#初始族群
def initial_pop(good_data, require_goods, non_require_goods, non_require_values, limit_weight):
    #good_data: 原始商品資料集
    #require_goods: 必要性的商品清單
    #non_require_goods: 不必要性的商品清單
    #non_require_values: 不必要性的商品數量
    #limit_weight: 最大重量限制
    while True:   
        temp_good = copy.copy(good_data) #先將原始資料暫時給另外一個變數使用    
        for i in range(len(require_goods)):
            get_index = temp_good[(temp_good[:,6] == require_goods[i]) & (temp_good[:, 8] !=1)] #取得符合條件的資料
            temp_index = random.randint(0, (len(get_index)-1)) #隨機取得品項
            get_index = get_index[temp_index] #取得商品名稱
            get_index[8] = 1
            temp_good[temp_good[:,0] == get_index[0]] = get_index
    
        selected_require = np.random.choice(non_require_goods, non_require_values, replace=False)
        
        for i in range(len(selected_require)):
            get_index = temp_good[(temp_good[:,6] == selected_require[i]) & (temp_good[:, 8] !=1)] #取得符合條件的資料
            temp_index = random.randint(0, (len(get_index)-1)) #隨機取得品項
            get_index = get_index[temp_index] #取得商品名稱
            get_index[8] = 1
            temp_good[temp_good[:,0] == get_index[0]] = get_index

        selected_good = temp_good[temp_good[:, 8] == 1]
        sum_weight = np.sum(selected_good[:,7])
        if sum_weight <= limit_weight:
            break
    return(selected_good) #回傳結果

#編碼染色體:
#必要性商品必定方在最前段, 選擇性商品必定放在後段
def create_chromosome(gene_list):
    #gene_list: 被選擇出的基因清單
    for i in range(len(gene_list)):
        chromosome = (gene_list[i]['data.frame'][:,0])     
        gene_list[i]['chromosome'] = chromosome
    return(gene_list)
    
#計算總重量
def total_weight(gene_list):
    #gene_list: 被選擇出的基因清單
    for i in range(len(gene_list)):
        sum_weight = cp.sum(gene_list[i]['data.frame'][:,8])
        gene_list[i]['totalWeight'] = cp.array([sum_weight])
    return(gene_list)
    
#偏好的適應度方法(算式分母為偏好值1~偏好的最大值)
def fitness_preference(gene_list, require_goods, non_require_values, preference_table):
    #gene_list: 被選擇出的基因清單
    #require_goods: 必要性的商品清單
    #non_require_goods: 不必要性的商品清單
    #user_preference: 使用者對商品種類的偏好
    
    max_preference = np.max(preference_table[:,1])
    total_preference = 0
    for i in range(0, max_preference+1, 1):
        total_preference = total_preference + i**2
    
    for i in range(len(gene_list)):
        reuslt = 1
        for j in range((len(require_goods) + non_require_values)):
            temp_preferenced = 1+(((gene_list[i]['data.frame'][:,9][j]**2)-1) / total_preference)
            reuslt *=temp_preferenced
            #temp.append(reuslt)
        
        gene_list[i]['fitPreference'] = reuslt
        sum_preferenced = (gene_list[i]['data.frame'][:,9]).sum()
        gene_list[i]['totalPreference'] = sum_preferenced
    return(gene_list)



#----執行----
goodData = diet_select(good_data = goodData, diet_habit_list = dietHabit) #葷素的方法
goodData = except_brand(good_data = goodData, except_brand_list = exceptBrandList) #剔除掉不想要的品牌

level = preferenceTable.iloc[:,0] #取得所有種類
requiredList = level[0:6] #必要性商品種類
nonRequiredList = (level[len(requiredList):len(level)]) #選擇性商品種類
nonRequiredValues = userItemValues-len(requiredList) #選擇性商品的數量

goodData = preference_match(good_data = goodData, preference_table = preferenceTable) #必要性商品種類
goodData.drop(['廠牌', '葷素'], axis = 1, inplace=True) #移除不必要的資料欄位
goodData = goodData.values #轉換成ndarry型態
preferenceTable = preferenceTable .values

##基因演算法開始
#產生初始口(遵照popAmount數量)
geneList = []
for i in range(popAmount):
    geneDict = {'data.frame': initial_pop(good_data = goodData, require_goods = requiredList, non_require_goods = nonRequiredList, non_require_values = nonRequiredValues, limit_weight = maxWeight)}
    geneList.append(geneDict)

#編碼染色體
geneList = create_chromosome(gene_list = geneList)

#計算每個基因總重量
geneList = total_weight(gene_list = geneList)

#計算偏好適應度(目前僅計算總偏好值)
fitnessPreference = fitness_preference(gene_list = geneList, require_goods = requiredList, non_require_values =  nonRequiredValues, preference_table = preferenceTable)
