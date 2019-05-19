
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

/*************** 宣告三維陣列 *****************/
double*** create_Array_3(int len_1, int len_2, int len_3){
    static double*** Array;
    Array = malloc(len_1 * sizeof(*Array));
    for(int i = 0; i < len_1; i++)
    {
        Array[i] = malloc(len_2 * sizeof(**Array));
        for(int j = 0; j < len_2; j++)
            Array[i][j] = malloc(len_3 * sizeof(***Array));
    }
    return Array;
}

/*************** 產生一組不重複亂數 *****************/
int* create_randArray(int l){
    static int out[30];
	int r,check;
	for (int i =0; i<l;i++){
		r = rand() % l;
		check = 0;
		for (int j =0; j<i;j++){
			if (r == out[j]){
				i--;
				check = 1;
				break;
			}
		}
		if (check == 1)
			continue;
		else 
			out[i] = r;
	}
    return out;
}
/*************** 產生兩個不重複亂數(介於0~99之間) *****************/
int create_cut_rand (int start, int end){
	int r1,r2,temp;
	static int r_out;
	while(1){
		r1 = (rand() % (end - start + 1)) + start; // 1~21
		r2 = (rand() % (end - start + 1)) + start; // 1~21
		if (r1 != r2)
			break;
	}
	if (r1 > r2){
		temp = r1;
		r1 = r2;
		r2 = temp;
	}
	r_out = r1 * 100 + r2;
	return r_out;
}
/*************** 計算偏好適應值 *****************/
double calculation_preference (int input){
	int max = 16;
	static double out_preference = 0;
	for (int i =1;i <=max; i++){
		out_preference += (i * i);
	}
	out_preference = 1 + (((input * input) - 1) / out_preference);
	return out_preference;
}
int main(){
	srand(time(NULL)); // 設定亂數種子
	FILE *goodTxt;
	int goodLength = 777; // 資料數
	int ga_len = 10; // 族群數
	int ga_block_len = 18; // 品項數量
	int category_len = 22; // 種類數
	int data_len = 7; // 資料種類數量
	int important_len = 6; // 基因塊數重要的數量 例如：6 等於 0~5種類必拿
	double goodArray[goodLength][data_len];
	int sum = 0;
	double id,price,vol,category,food,weight,preference;
	goodTxt = fopen("./goodArray.txt", "r");
	while(!feof(goodTxt)) {	
		fscanf(goodTxt, "%lf,%lf,%lf,%lf,%lf,%lf,%lf", &id,&price,&vol,&category,&food,&weight,&preference);
		goodArray[sum][0] = id;
		goodArray[sum][1] = price;
		goodArray[sum][2] = vol;
		goodArray[sum][3] = category;
		goodArray[sum][4] = food;
		goodArray[sum][5] = weight;
		goodArray[sum][6] = preference;
		sum++;
	}
	fclose(goodTxt);
	int categoryArray[category_len];
	int temp;
	/****** 統計分類的個別數量 這樣才可以產生亂數 *******/
	for (int i = 0; i < category_len; i++)
		categoryArray[i] = 0;
	for (int i = 0; i < goodLength; i++){
		temp = goodArray[i][3];
		categoryArray[temp]++;
	}
	/****** 找出種類中最多的數量 *******/
	int category_max = 0;
	for (int i = 0; i < category_len; i++){
		if (categoryArray[i] > category_max)
			category_max = categoryArray[i];
	}
	/****** 將資料轉換成以種類為主的三維陣列 *******/
	double*** category_goodArray = create_Array_3(category_len,category_max,data_len);
	int num;
	for (int i = 0; i < category_len; i++){
		num = 0;
		for (int j = 0; j < goodLength; j++){
			if (goodArray[j][3] == i){
				for (int k = 0; k<data_len;k++){
					category_goodArray[i][num][k] = goodArray[j][k];
				}
				num++;
			}
		}
	}
	/****** 產生染色體 30 x 22 *******/
	double*** ga_Array = create_Array_3(ga_len,ga_block_len,data_len);
	int weight_sum; // 統計重量
	int weight_T = 16000; // 重量門檻
	int category_r = 0; // 亂數取得種類
	int check;
	for (int i = 0; i< ga_len; i++){
		weight_sum = 0;
		/***** 先把重要的基因塊放進去 ******/
		for (int j = 0; j < important_len; j++){
			temp = rand() % categoryArray[j];
			for (int k = 0; k<data_len;k++){
				ga_Array[i][j][k] = category_goodArray[j][temp][k];
			}
			weight_sum += ga_Array[i][j][5]; //重量
		}
		/***** 把剩下的基因塊補進去 ******/
		for (int j = important_len; j < ga_block_len; j++){
			if (j != 0){
				check = 1;
				while(check){
					// (總種類數 - 前面必拿的數量 + 1) + 前面必拿的數量  Ex: (22 - 6 + 1) + 6 -> 產生 6~21
					category_r = rand() % ((category_len-1) - important_len + 1) + important_len; 
					check = 0;
					for(int k = 0; k<j; k++){
						if (category_r == ga_Array[i][k][3])
							check = 1;
					}
				}
			}
			temp = rand() % categoryArray[category_r]; // 該種類的第亂數個
			for (int k = 0; k<data_len;k++){
				ga_Array[i][j][k] = category_goodArray[category_r][temp][k];
			}
			weight_sum += ga_Array[i][j][5]; //重量
		}
		/****** 重量不符 所以拋棄 *******/
		if (weight_sum > weight_T)
			i--;
	}
	/****** 計算適應值 *******/
	double price_sum,vol_sum,preference_sum; // 統計 價格 體積
	int price_T = 1500; // 限制價格
	int vol_T = 47 * 32 * 39; // 限制體積
	double price_adaptArray[ga_len*2]; //價格適應值
	double vol_adaptArray[ga_len*2]; //體積適應值
	double preference_adaptArray[ga_len*2]; //偏好適應值
	double all_adaptArray[ga_len]; //總體適應值
	for (int i = 0; i< ga_len; i++){
		weight_sum = 0;
		price_sum = 0;
		preference_sum = 0;
		for (int j = 0; j < ga_block_len; j++){
			price_sum += ga_Array[i][j][1]; // 價錢
			vol_sum += ga_Array[i][j][2]; //體積
			preference_sum *= calculation_preference(ga_Array[i][j][6]);
		}
		/****** 計算價格適應值 *******/
		price_adaptArray[i] = fabs(price_T - price_sum) / price_T;
		if ((price_T - price_sum) == 0)
			price_adaptArray[i] += 1;
		else if ((price_T - price_sum) > 0)
			price_adaptArray[i] += 2;
		else if ((price_T - price_sum) < 0)
			price_adaptArray[i] += 3;
		/****** 計算體積適應值 *******/
		vol_adaptArray[i] = fabs(vol_T - vol_sum) / vol_T;
		if ((vol_sum >= (vol_T * 0.7)) && (vol_sum <= vol_T)){
			if ((vol_T - vol_sum) == 0)
				vol_adaptArray[i] += 1;
			else 
				vol_adaptArray[i] += 2;
		}
		else {
			vol_adaptArray[i] += 3;
		}
		/****** 計算偏好適應值 *******/
		preference_adaptArray[i] = preference_sum;
		all_adaptArray[i] = price_adaptArray[i] + vol_adaptArray[i] + preference_adaptArray[i];
	}
	int t = 0; // 世代次數
	// while(t < 10){
		/****** 開始交配前處理 Select (分數越小越好) *******/
		double*** ga_selectArray = create_Array_3(ga_len,ga_block_len,data_len);
		int cut_r,cut_r1,cut_r2;
		for (int i = 0; i<ga_len; i++){
			cut_r = create_cut_rand(0,(ga_len-1));
			cut_r1 = cut_r / 100;
			cut_r2 = cut_r % 100;
			if (all_adaptArray[cut_r1] < all_adaptArray[cut_r2]){
				for (int j = 0; j<ga_block_len; j++)
					for (int k = 0; k<data_len; k++)
						ga_selectArray[i][j][k] = ga_Array[cut_r1][j][k];
			}
			else if (all_adaptArray[cut_r1] > all_adaptArray[cut_r2]){
				for (int j = 0; j<ga_block_len; j++)
					for (int k = 0; k<data_len; k++)
						ga_selectArray[i][j][k] = ga_Array[cut_r2][j][k];
			}
			else{
				for (int j = 0; j<ga_block_len; j++)
					for (int k = 0; k<data_len; k++)
						ga_selectArray[i][j][k] = ga_Array[cut_r1][j][k];
			}
		}
		/****** 開始交配 *******/
		double*** cross_over_Array = create_Array_3(ga_len,ga_block_len,data_len);
		int* randArray = create_randArray(ga_len); // 0~29
		for (int i = 0 ; i<ga_len; i+=2){
			cut_r = create_cut_rand(1,21);
			cut_r1 = cut_r / 100;
			cut_r2 = cut_r % 100;
			for (int j = 0; j < ga_block_len; j++){
				for (int k =0; k<data_len ; k++){
					if ((j > cut_r1) && (j < cut_r2)){
						cross_over_Array[i+1][j][k] = ga_selectArray[randArray[i]][j][k];
						cross_over_Array[i][j][k] = ga_selectArray[randArray[i+1]][j][k];
					}
					else {
						cross_over_Array[i][j][k] = ga_selectArray[randArray[i]][j][k];
						cross_over_Array[i+1][j][k] = ga_selectArray[randArray[i+1]][j][k];
					}
				}
			}
		}
		/****** 開始除錯(過濾重複) *******/
		for (int i = 0; i < ga_len; i++){
			for (int j = 0; j < ga_block_len; j++){
				for (int k =0; k<j ; k++){
					if (cross_over_Array[i][j][3] == cross_over_Array[i][k][3]){
						check = 1;
						while(check){
							category_r = rand() % category_len; 
							check = 0;
							for(int l = 0; l<j; l++){
								if (category_r == ga_Array[i][k][3])
									check = 1;
							}
						}
						temp = rand() % categoryArray[category_r]; // 該種類的第亂數個
						for (int l = 0; l<data_len;l++){
							cross_over_Array[i][j][l] = category_goodArray[category_r][temp][l];
						}
						break;
					}
				}
			}
		}
		/****** 開始突變 *******/
		double mutation_T = 0.1; // 突變值門檻
		double mutation_id; // 突變id
		int mutation_category,mutation_r,mutation_category_sum,mutation_category_num; // 突變種類 突變亂數值 
		for (int i = 0; i < ga_len; i++){
			mutation_r = (double)rand() / (RAND_MAX + 1.0);
			// 突變
			if (mutation_r <= mutation_T){
				mutation_category_sum = 0;
				mutation_r = rand() % ga_block_len;
				mutation_category = (int)cross_over_Array[i][mutation_r][3];
				mutation_id = cross_over_Array[i][mutation_r][0];
				check = 1;
				while(check){
					temp = rand() % categoryArray[mutation_category];
					if (mutation_id != category_goodArray[mutation_category][temp][0]){
						for (int k = 0; k<data_len;k++)
							cross_over_Array[i][mutation_r][k] = category_goodArray[mutation_category][temp][k];
						check = 0;
						break;
					}
					else 
						continue;
				}
			}
		}
		/****** 將父親與孩子合併到同一個陣列 *******/
		double*** ga_merge_Array = create_Array_3(ga_len*2,ga_block_len,data_len); // 父母與孩子
		for (int i = 0; i< ga_len*2; i++){
			for (int j = 0; j < ga_block_len; j++){
				for (int k = 0; k < data_len; k++){
					if (i < 30)
						ga_merge_Array[i][j][k] = ga_Array[i][j][k];
					else 
						ga_merge_Array[i][j][k] = cross_over_Array[i-ga_len][j][k];
				}
			}
		}
		double ga_score_Array[ga_len*2];  // 統計父母與孩子的分數
		/****** 統計分數 *******/
		for (int i = 0; i< ga_len*2; i++){
			price_sum = 0;
			vol_sum = 0;
			preference_sum = 0;
			for (int j = 0; j < ga_block_len; j++){
				price_sum += ga_merge_Array[i][j][1]; // 價錢
				vol_sum += ga_merge_Array[i][j][2]; //體積
				preference_sum *= calculation_preference(ga_merge_Array[i][j][6]);
			}
			/****** 計算價格適應值 *******/
			price_adaptArray[i] = fabs(price_T - price_sum) / price_T;
			if ((price_T - price_sum) == 0)
				price_adaptArray[i] += 1;
			else if ((price_T - price_sum) > 0)
				price_adaptArray[i] += 2;
			else if ((price_T - price_sum) < 0)
				price_adaptArray[i] += 3;
			/****** 計算體積適應值 *******/
			vol_adaptArray[i] = fabs(vol_T - vol_sum) / vol_T;
			if ((vol_sum >= (vol_T * 0.7)) && (vol_sum <= vol_T)){
				if ((vol_T - vol_sum) == 0)
					vol_adaptArray[i] += 1;
				else 
					vol_adaptArray[i] += 2;
			}
			else {
				vol_adaptArray[i] += 3;
			}
			/****** 計算偏好適應值 *******/
			preference_adaptArray[i] = preference_sum;
			ga_score_Array[i] = price_adaptArray[i] + vol_adaptArray[i] + preference_adaptArray[i];
		}
		/****** 將父親與孩子總計60條染色體進行排序 *******/
		double score_temp = 0;
		for (int i = 0; i < (ga_len * 2); i++){
			for (int j =0; j < i; j++){
				if (ga_score_Array[i] < ga_score_Array[j]){
					score_temp = ga_score_Array[i];
					ga_score_Array[i] = ga_score_Array[j];
					ga_score_Array[j] = score_temp;
					for (int k = 0 ; k < ga_block_len; k++){
						for (int l = 0; l < data_len; l++){
							score_temp = ga_merge_Array[i][k][l];
							ga_merge_Array[j][k][l] = ga_merge_Array[i][k][l];
							ga_merge_Array[i][k][l] = score_temp;
						}
					}
				}
			}
		}
		/****** 從父母與後代中挑選出好的30個 *******/
		num = 0;
		for (int i = 0; i< ga_len*2; i++){
			weight_sum = 0;
			for (int j = 0; j < ga_block_len; j++){
				weight_sum += ga_merge_Array[i][j][5]; //重量
			}
			/****** 重量不符 所以拋棄 *******/
			if (weight_sum <= weight_T){
				for (int k = 0 ; k < ga_block_len; k++){
					for (int l = 0; l < data_len; l++){
						ga_Array[num][k][l] = ga_merge_Array[i][k][l];
					}
				}
				num++;
			}
			if (num >= ga_len)
				break;
		}
		printf("%d:%f\n",t,ga_score_Array[0]);
		/******* 開始釋放陣列記憶體 ******/
		// free(ga_selectArray);
		// free(cross_over_Array);
		// free(randArray);
		// free(ga_merge_Array);
		// free(ga_score_Array);
		t++;
		
	// }
	printf("123");
	return 0;
	
}
