import datetime
import pandas as pd

#----時間紀錄(開始)----
startTime = datetime.datetime.now()

#----資料初始化(本地端)----
sourceData = pd.read_csv("assets_py/StoreData.csv") #讀取原始資料
preferenceTable = pd.read_csv("assets_py/preferenceTable.csv") #讀取商品偏好表
sourceData.drop(["序", "輸入格式(序,產品代碼+品名,單價,體積,種類)"], axis = 1, inplace=True) #移除不必要的資料欄位
sourceData.rename(index=str, columns={"重量(g)": "重量"}, inplace=True) #重新命名欄位名稱

goodData = sourceData #將原始資料複製一份
goodData["Selected"], goodData["Preference"] = 0, 1 #新增被選擇欄位

#----環境參數設定----
maxVolume = 47*32*39 #最大箱子體積
maxWeight = 16000 #最大重量(g)
popAmount = 100 #人口數量
crossRate = 0.9 #交配率
mutationRate = 0.2 #突變率
eliteValues = round(popAmount*0.1) #菁英數量
maxGen = 100 #世代次數

#----使用者需輸入的參數(假設)----
dietHabit = "葷食" #葷食與素食的選擇
userItemValues = 18 #使用者需要的數量
maxPrice = 1500 #使用者金額

def preference_match(good_data, preference_table):
  #gene_list: 被選擇出的基因清單
  #require_goods: 必要性的商品清單
  #non_require_goods: 不必要性的商品清單
  #user_preference: 使用者對商品種類的偏好
    
#  for (i in 1:dim(preference_table)[1]) {
#    # good_data[good_data$種類==good_preference$category[i],]$Preference <- as.numeric(good_preference$preference[i])^2
#    good_data[good_data$種類==preference_table$category[i],]$Preference <- as.numeric(preference_table$preference[i])
#  }
#  return(good_data)
  for i in range(len(preference_table)):
      



#----執行----
#葷素的方法
  
  

