import numpy as np
from numba import vectorize, int64
from numba import guvectorize, int64
from pyculib import rand as curand
#from numba import jit
import time
import pandas as pd
import random
import matplotlib.pyplot as plt
from matplotlib.font_manager import FontProperties
import copy
import os
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
exceptBrandList = ['大同'] #將要剔除的品牌

#----Function----
#飲食習慣(葷或素食):
#在開始演算法前先將該飲食習慣給加入
#若為葷食則包含素食和葷食, 反之只有素食
def diet_select(good_data, diet_habit_list):
    #good_data: 原始商品資料集
    #diet_habit_list: 葷食或素食的選擇
    if diet_habit_list=='素食':
        good_data = good_data[good_data[:, 9] == diet_habit_list]
    return(good_data)
    
    
    
    
def diet_select(good_data, diet_habit_list):
    #good_data: 原始商品資料集
    #diet_habit_list: 葷食或素食的選擇
    if diet_habit_list=='素食':
        good_data = good_data[good_data[:, 9] == diet_habit_list]
    return(good_data)

#剔除品牌的方法:
#在開始演算法前先將該品牌給移除
def except_brand(good_data, except_brand_list):
    #good_data: 原始商品資料集
    #except_brand_list: 剔除品牌的名稱
    for i in range(len(except_brand_list)):
        good_data = good_data[good_data[:, 4] != except_brand_list[i]] #將要剔除的廠牌移除
#    good_data.index = range(len(good_data))
    return(good_data)

#偏好值與類別合併:
#將使用者對商品種類的偏好與原始商品資料進行合併成一個Data Frame, 使原始資料有使用者對每個商品的品項偏好
#@vectorize(['int32(int32, int32)'], target='cuda')
#@guvectorize("int64[:], int64", "(n) -> ()")
def preference_match(good_data, preference_table):
    #gene_list: 被選擇出的基因清單
    #require_goods: 必要性的商品清單
    #non_require_goods: 不必要性的商品清單
    #user_preference: 使用者對商品種類的偏好
    for i in range(len(preference_table)):
        temp = good_data[good_data[:, 8] == preference_table['category'][i]]
        for j in range(len(good_data[good_data[:, 8] == preference_table['category'][i]])):
            temp[:, 12][j] = preference_table['preference'][i]
        good_data[good_data[:, 8] == preference_table['category'][i]] = temp
    return(good_data)

@guvectorize([(int64[:])], '(n)')
def initial_require_goods(get_index):
    temp_index = random.randint(0,(len(get_index)-1)) #隨機取得品項
    get_index = get_index[temp_index] #取得商品名稱
    get_index[11] = 1
    temp_good[temp_good[:,0] == get_index[0]] = get_index



def initial_pop(good_data, require_goods, non_require_goods, non_require_values, limit_weight):
    #good_data: 原始商品資料集
    #require_goods: 必要性的商品清單
    #non_require_goods: 不必要性的商品清單
    #non_require_values: 不必要性的商品數量
    #limit_weight: 最大重量限制
    while True:
#        temp_good = np.copy(good_data) #先將原始資料暫時給另外一個變數使用    
        temp_good = good_data #先將原始資料暫時給另外一個變數使用    
        for i in range(len(require_goods)):
            get_index = temp_good[(temp_good[:,8] == require_goods[i]) & (temp_good[:, 11] !=1)] #取得符合條件的資料
            initial_require_goods(get_index)
#            temp_index = random.randint(0,(len(get_index)-1)) #隨機取得品項
#            get_index = get_index[temp_index] #取得商品名稱
#            get_index[11] = 1
#            temp_good[temp_good[:,0] == get_index[0]] = get_index
    
        selected_require = (non_require_goods.sample(non_require_values)).tolist()
        
        for i in range(len(selected_require)):
            get_index = temp_good[(temp_good[:,8] == selected_require[i]) & (temp_good[:, 11] !=1)] #取得符合條件的資料
            temp_index = random.randint(0,(len(get_index)-1)) #隨機取得品項
            get_index = get_index[temp_index] #取得商品名稱
            get_index[11] = 1
            temp_good[temp_good[:,0] == get_index[0]] = get_index

        selected_good = temp_good[temp_good[:, 11] == 1]
        sum_weight = np.sum(selected_good[:,10])
        if sum_weight <= limit_weight:
            break
    return(selected_good) #回傳結果
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    















goodData = diet_select(good_data = goodData, diet_habit_list = dietHabit)

#剔除掉不想要的品牌
goodData = except_brand(good_data = goodData, except_brand_list = exceptBrandList)


level = preferenceTable['category']
requiredList = level[0:6]
nonRequiredList = (level[len(requiredList):len(level)]).reset_index(drop = True)
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
















