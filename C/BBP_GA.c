#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

/*************** 宣告可變一維陣列方法 *****************/
double* create_Array_1(int len_1){
    static double* Array;
    Array = malloc(len_1 * sizeof(*Array));
    return Array;
}

/*************** 宣告可變三維陣列方法 *****************/
double*** create_Array_3(int len_1, int len_2, int len_3){
    static double*** Array;
    Array = malloc(len_1 * sizeof(*Array));
    for(int i = 0; i < len_1; i++){
        Array[i] = malloc(len_2 * sizeof(**Array));
        for(int j = 0; j < len_2; j++)
            Array[i][j] = malloc(len_3 * sizeof(***Array));
    }
    return Array;
}

/*************** 計算偏好適應值 *****************/
double calculation_preference (double input){
    // printf("%d\n", input);
	int max = 16;
	double out_preference = 0;
	for (int i = 1; i <= max; i++){
		out_preference += (i * i);
	}
	out_preference = 1 + (((input * input) - 1) / out_preference);
	return out_preference;
}

/*************** 產生兩個不重複亂數(介於0~99之間) *****************/
int create_cut_rand (int start, int end){
	int r1, r2, temp;
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

/*************** 產生一組不重複亂數 *****************/
int* create_randArray(int length){
    static int* out;
    out = malloc(length * sizeof(*out));
	int r, check;
	for (int i = 0; i < length; i++){
		r = rand() % length;
		check = 0;
		for (int j = 0; j < i;j++){
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

int main(){
    clock_t start, finish;
    start = clock(); 
    srand(time(NULL)); //用時間當作種子，使每次執行亂數都會不同

    /****** GA參數值 ******/
	int populationCount = 10; //族群數
    double crossRate = 0.9; // 交配率
    double mutationRate = 1; //突變率
    int generation = 10;
	
    /***** 使用者設定 *****/
    int maxWeight = 16000; // 重量限制
    int itemValues = 18; //品項數量
    int maxPrice = 1500; // 限制價格
    int maxVolume = 47 * 32 * 39; // 箱子體積大小
    int requireLen = 6; //必要性商品數量
    int nonRequireLen = 16; //選擇性商品數量

    /***** 其他變數 *****/
    FILE *goodTxt; //宣告讀檔變數
    FILE *resultTxt; //宣告存檔變數
	int goodLength = 777; //資料總長度
    double goodArray[goodLength][7]; //宣告二維陣列，0-產品代號，1-單價，2-體積，3-種類，4-葷素，5-重量，6-偏好
    double id, price, volume, category, food, weight, preference; //宣告欄位在陣列的位置
    int sum = 0; //讀檔用計數變數
    int temp; //暫時計數變數
    int check; //檢查值
    int checkSum; //檢查值的加總
    int categoryNum;
    int columnLen = 7; //資料欄位數量
	int weightSum; //計算重量總和
    int categoryR = 0; //取得亂數種類
    int crossRand; //隨機交配值
    double mutationID; // 突變id
	int mutationCategory, mutationRand; // 突變種類，突變亂數值 
    int iteration = 1; //迭代次數
    int categoryLen = requireLen + nonRequireLen; // 種類數量
    int nonRequireValues = itemValues - requireLen; //需要的選擇性商品數量
    double genPreferenceBest[generation]; //紀錄每一代最好的偏好值
    double genVolumeBest[generation]; //紀錄每一代最好的體積率
    double genPriceBest[generation]; //紀錄每一代最好的總價格
    double genTotalFitBest[generation]; //紀錄每一代總體適應函數
    
	srand(time(NULL));
    goodTxt = fopen("./goodArray.txt", "r"); //讀入資料檔案
	while(!feof(goodTxt)) {	
		fscanf(goodTxt, "%lf,%lf,%lf,%lf,%lf,%lf,%lf", &id ,&price, &volume, &category, &food, &weight, &preference); //讀一行資料
		goodArray[sum][0] = id; //將產品代號塞入陣列
		goodArray[sum][1] = price; //將單價塞入陣列
		goodArray[sum][2] = volume; //將體積塞入陣列
		goodArray[sum][3] = category; //將種類塞入陣列
		goodArray[sum][4] = food; //將葷素塞入陣列
		goodArray[sum][5] = weight; //將重量塞入陣列
		goodArray[sum][6] = preference; //將偏好塞入陣列
		sum++;
	}
    fclose(goodTxt); //關閉檔案

    /****** 統計分類的個別數量 這樣才可以產生亂數 *******/
    int categoryArray[categoryLen]; //宣告種類的陣列
	for (int i = 0; i < categoryLen; i++) {
        categoryArray[i] = 0;
    }		
	for (int i = 0; i < goodLength; i++){
		temp = goodArray[i][3];
		categoryArray[temp]++;
	}

    /****** 找出種類中最多的數量 *******/
	int categoryMax = 0;
	for (int i = 0; i < categoryLen; i++){
		if (categoryArray[i] > categoryMax){
            categoryMax = categoryArray[i]; //抓出數量最多的那個種類
        }
	}

    /****** 將資料轉換成以種類為主的三維陣列 *******/
	double*** categoryGoodArray = create_Array_3(categoryLen, categoryMax, columnLen);
	int num; //暫時計數變數
	for (int i = 0; i < categoryLen; i++){
		num = 0;
		for (int j = 0; j < goodLength; j++){
			if (goodArray[j][3] == i){
				for (int k = 0; k < columnLen; k++){
					categoryGoodArray[i][num][k] = goodArray[j][k]; //將產品資訊塞入陣列
				}
				num++;
			}
		}
	}

    /****** 產生染色體 populationCount x itemValues *******/
    double*** gaPopulationArray = create_Array_3(populationCount, itemValues, 8); //建立三維陣列，第一維-第幾個染色體，第二維-第幾個品項，第三維-產品資訊
    int nonRequireIndex;
    for (int i = 0; i < populationCount; i++){
        /***** 隨機抓出不重複的選擇性商品之種類 ****/
        double *nonRequireArray = create_Array_1(nonRequireValues); //要抓的選擇性商品編號
        int checkNonReq; //檢查值
        for (int z = 0; z < nonRequireValues; z++){
            temp = rand() % (categoryLen - requireLen) + requireLen; //隨機選擇選擇性商品種類的編號
            checkNonReq = 0; //檢查值歸零
            for (int u = 0; u < z; u++){
                if (temp == nonRequireArray[u]){
                    z--; //將迴圈重作的變數
                    checkNonReq = 1; //檢查值確認
                    break;
                }
            }
            if (checkNonReq == 1){
                continue;
            } else {
                nonRequireArray[z] = temp; //將不重複的類別編號放入陣列
            }
        }
        
        weightSum = 0; //將累計重量值歸零

        /***** 必要性商品產生 *****/
        for (int j = 0; j < requireLen; j++){
            temp = rand() % categoryArray[j]; //隨機挑選該類別別中的第幾個品項
            for (int k = 0; k < 7; k++){
                gaPopulationArray[i][j][k] = categoryGoodArray[j][temp][k]; //把該商品資訊放入陣列
            }
            weightSum += gaPopulationArray[i][j][5]; //重量
        }
        
        /***** 選擇性商品產生 *****/
        nonRequireIndex = requireLen; //從第requireLen值繼續放資料到染色體中
        for (int k = 0; k < nonRequireValues; k++){
            categoryNum = nonRequireArray[k]; //依序抓取nonRequireArray裡的商品類別
            temp = rand() % categoryArray[categoryNum]; //隨機挑選該類別別中的第幾個品項
            for (int z = 0; z < columnLen; z++){
                gaPopulationArray[i][nonRequireIndex][z] = categoryGoodArray[categoryNum][temp][z]; //把該商品資訊放入陣列
            }
            weightSum += gaPopulationArray[i][nonRequireIndex][5]; //重量
            nonRequireIndex++;
        }

        /****** 重量不符 所以拋棄 *******/
		if (weightSum > maxWeight){
            i--; //重作該i次
        }
	}

    /***** 印出初始族群 *****/
    // for(int i = 0; i < populationCount; i++){
    //     printf("======== Pop: %d ========\n", i);
    //     for (int j = 0; j < itemValues; j++){
    //         printf("%d: ", j);
    //         for (int k = 0; k <columnLen; k++){
    //             printf("%lf, ", gaPopulationArray[i][j][k]);
    //         }
    //         printf("\n");
    //     }
    // }

    /****** 計算適應值 *******/
	double priceSum, volumeSum, preferenceSum, preferenceSum2; // 累計價格，體積，偏好(計算適應值)，偏好(計算總和)
	double priceFitnessArray[populationCount*2]; //價格適應值(*2是因為合併後也需要用到後面陣列的空間)
	double volumeFitnessArray[populationCount*2]; //體積適應值 (*2是因為合併後也需要用到後面陣列的空間)
	double preferenceFitnessArray[populationCount*2]; //偏好適應值 (*2是因為合併後也需要用到後面陣列的空間)
	double totalFitnessArray[populationCount]; //總體適應值
    double priceTotalArray[populationCount*2]; //總價格(*2是因為合併後也需要用到後面陣列的空間)
    double volumeTotalArray[populationCount*2]; //總體積 (*2是因為合併後也需要用到後面陣列的空間)
	double preferenceTotalArray[populationCount*2]; //總偏好 (*2是因為合併後也需要用到後面陣列的空間)

    for (int i = 0; i < populationCount; i++){
		priceSum = 0; //累計價格歸零
        volumeSum = 0; //累計體積歸零
        preferenceSum2 = 0;
		preferenceSum = 1; //累計偏好值為一        

		for (int j = 0; j < itemValues; j++){
			priceSum += gaPopulationArray[i][j][1]; // 計算總價格
			volumeSum += gaPopulationArray[i][j][2]; //計算總體積
			preferenceSum *= calculation_preference(gaPopulationArray[i][j][6]); //計算偏好適應函數
            preferenceSum2 += gaPopulationArray[i][j][6]; //計算總偏好
		}

		/****** 計算價格適應值 *******/
		priceFitnessArray[i] = fabs(maxPrice - priceSum) / maxPrice;
		if ((maxPrice - priceSum) == 0)
			priceFitnessArray[i] += 1;
		else if ((maxPrice - priceSum) > 0)
			priceFitnessArray[i] += 2;
		else if ((maxPrice - priceSum) < 0)
			priceFitnessArray[i] += 3;
		/****** 計算體積適應值 *******/

		volumeFitnessArray[i] = fabs(maxVolume - volumeSum) / maxVolume;
		if ((volumeSum >= (maxVolume * 0.7)) && (volumeSum <= maxVolume)){
			if ((maxVolume - volumeSum) == 0)
				volumeFitnessArray[i] += 1;
			else 
				volumeFitnessArray[i] += 2;
		}
		else {
			volumeFitnessArray[i] += 3;
		}
		preferenceFitnessArray[i] = preferenceSum;
        priceTotalArray[i] = priceSum;
        volumeTotalArray[i] = volumeSum;
        preferenceTotalArray[i] = preferenceSum2;		
        /****** 計算總適應函數 *******/
		totalFitnessArray[i] = priceFitnessArray[i] * volumeFitnessArray[i] * preferenceFitnessArray[i];
	}

    /***** 印出所有染色體的適應函數值 *****/
    // for (int i = 0; i < populationCount; i++){
    //     printf("=====Chrom - %d=====\n", i);
    //     printf("Preference Fit: %lf\n", preferenceFitnessArray[i]);
    //     printf("Price Fit: %lf\n", priceFitnessArray[i]);
    //     printf("Volume Fit: %lf\n", volumeFitnessArray[i]);
    //     printf("Total Fit: %lf\n", totalFitnessArray[i]);
    // }

    /***** 演化開始 *****/
    while(iteration <= generation){
        /****** 開始交配前處理 Select (分數越小越好) *******/
        double*** gaSelectArray = create_Array_3(populationCount, itemValues, columnLen); //宣告選擇過後的陣列
        int cutR, cutR1 ,cutR2;
        for (int i = 0; i < populationCount; i++){
            cutR = create_cut_rand(0, (populationCount - 1)); //取得不重複的隨機亂數
            cutR1 = cutR / 100; //取得第一個染色體位置
            cutR2 = cutR % 100; //取得第二個染色體位置
            if (totalFitnessArray[cutR1] < totalFitnessArray[cutR2]){
                for (int j = 0; j < itemValues; j++)
                    for (int k = 0; k < columnLen; k++)
                        gaSelectArray[i][j][k] = gaPopulationArray[cutR1][j][k];
            }
            else if (totalFitnessArray[cutR1] > totalFitnessArray[cutR2]){
                for (int j = 0; j < itemValues; j++)
                    for (int k = 0; k<columnLen; k++)
                        gaSelectArray[i][j][k] = gaPopulationArray[cutR2][j][k];
            }
            else{
                for (int j = 0; j < itemValues; j++)
                    for (int k = 0; k < columnLen; k++)
                        gaSelectArray[i][j][k] = gaPopulationArray[cutR1][j][k];
            }
        }

        /***** 印出選擇後的染色體 *****/
        // for(int i = 0; i < populationCount; i++){
        //     printf("======== select after: %d ========\n", i);
        //     for (int j = 0; j < itemValues; j++){
        //         printf("%d: ", j);
        //         for (int k = 0; k <columnLen; k++){
        //             printf("%lf, ", gaSelectArray[i][j][k]);
        //         }
        //         printf("\n");
        //     }
        // }

        /****** 開始交配 *******/
        double*** crossOverArray = create_Array_3(populationCount, itemValues, columnLen);
        int* randArray = create_randArray(populationCount); // 0~29
        for (int i = 0 ; i < populationCount; i += 2){
            crossRand = (double)rand() / (RAND_MAX + 1.0); 
            if (crossRand <= crossRate){
                cutR = create_cut_rand(1, 21);
                cutR1 = cutR / 100; //取得交配第一節點
                cutR2 = cutR % 100; //取得交配第二節點
                for (int j = 0; j < itemValues; j++){
                    for (int k = 0; k < columnLen ; k++){
                        if ((j > cutR1) && (j < cutR2)){
                            crossOverArray[i + 1][j][k] = gaSelectArray[randArray[i]][j][k];
                            crossOverArray[i][j][k] = gaSelectArray[randArray[i + 1]][j][k];
                        } else {
                            crossOverArray[i][j][k] = gaSelectArray[randArray[i]][j][k];
                            crossOverArray[i + 1][j][k] = gaSelectArray[randArray[i + 1]][j][k];
                        }
                    }
                }
            }
        }

        /***** 印出交配後的染色體 *****/
        // for(int i = 0; i < populationCount; i++){
        //     printf("======== cross after: %d ========\n", i);
        //     for (int j = 0; j < itemValues; j++){
        //         printf("%d: ", j);
        //         for (int k = 0; k <columnLen; k++){
        //             printf("%lf, ", crossOverArray[i][j][k]);
        //         }
        //         printf("\n");
        //     }
        // }

        /****** 開始除錯(過濾重複) *******/
        for (int i = 0; i < populationCount; i++){
            for (int j = 0; j < itemValues; j++){
                for (int k = 0; k < j ; k++){
                    if (crossOverArray[i][j][3] == crossOverArray[i][k][3]){
                        check = 1; //檢查值先給1，準備無限迴圈
                        checkSum = 0; //檢查總值歸零
                        while (check){
                            categoryR = rand() % categoryLen; //隨機挑選一個種類
                            for(int z = 0; z < itemValues; z++){
                                /* 挑出之種類開始與所有染色體的種類做比對 */
                                if(categoryR != crossOverArray[i][z][3]) {
                                    checkSum++; //若不相等就+1
                                } else {
                                    checkSum = 0; //若相等就歸零，重新再隨機挑一種種類
                                    break; //跳出此迴圈
                                }
                            }
                            if (checkSum == itemValues) {
                                check = 0; //如果不相等累加到與itemValues相同，就跳出無限迴圈
                            }
                        }
                        temp = rand() % categoryArray[categoryR]; // 該種類的第亂數個
                        for (int l = 0; l < columnLen; l++){
                            crossOverArray[i][j][l] = categoryGoodArray[categoryR][temp][l]; 
                        }
                        break;
                    }
                }
            }
        }

        /***** 印出交配除錯後的染色體 *****/
        // for(int i = 0; i < populationCount; i++){
        //     printf("======== cross after2: %d ========\n", i);
        //     for (int j = 0; j < itemValues; j++){
        //         printf("%d: ", j);
        //         for (int k = 0; k <columnLen; k++){
        //             printf("%lf, ", crossOverArray[i][j][k]);
        //         }
        //         printf("\n");
        //     }
        // }

        /****** 開始突變 *******/
        for (int i = 0; i < populationCount; i++){
            mutationRand = (double)rand() / (RAND_MAX + 1.0); //取得亂數隨機值
            if (mutationRand <= mutationRate){
                mutationRand = rand() % itemValues; //隨機取得索引值
                mutationCategory = (int)crossOverArray[i][mutationRand][3]; //取得要突變的種類
                mutationID = crossOverArray[i][mutationRand][0]; //取得要突變的產品代號
                check = 1;
                while(check){
                    temp = rand() % categoryArray[mutationCategory]; //隨機取的該種類的某品項
                    if (mutationID != categoryGoodArray[mutationCategory][temp][0]){
                        for (int k = 0; k < columnLen; k++){
                            crossOverArray[i][mutationRand][k] = categoryGoodArray[mutationCategory][temp][k];
                        }
                        check = 0;
                        break;
                    } else {
                        continue;
                    }
                }
            }
        }
        
        /***** 印出突變後的染色體 *****/
        // for(int i = 0; i < populationCount; i++){
        //     printf("======== mutation: %d ========\n", i);
        //     for (int j = 0; j < itemValues; j++){
        //         printf("%d: ", j);
        //         for (int k = 0; k <columnLen; k++){
        //             printf("%lf, ", crossOverArray[i][j][k]);
        //         }
        //         printf("\n");
        //     }
        // }

        /****** 將父親與孩子合併到同一個陣列 *******/
        double*** gaMergeArray = create_Array_3(populationCount*2, itemValues, columnLen); // 父母與孩子
        for (int i = 0; i < populationCount*2; i++){
            for (int j = 0; j < itemValues; j++){
                for (int k = 0; k < columnLen; k++){
                    if (i < populationCount){
                        gaMergeArray[i][j][k] = gaPopulationArray[i][j][k]; //將父母染色體放入陣列
                    } else {
                        gaMergeArray[i][j][k] = crossOverArray[i-populationCount][j][k]; //將孩子染色體放入陣列
                    }
                }
            }
        }

        /***** 印出合併後的染色體 *****/
        // for(int i = 0; i < populationCount*2; i++){
        //     printf("======== merge: %d ========\n", i);
        //     for (int j = 0; j < itemValues; j++){
        //         printf("%d: ", j);
        //         for (int k = 0; k <columnLen; k++){
        //             printf("%lf, ", gaMergeArray[i][j][k]);
        //         }
        //         printf("\n");
        //     }
        // }

        /****** 統計分數 *******/
        double gaScoreArray[populationCount*2];  // 統計父母與孩子的分數
        for (int i = 0; i < populationCount*2; i++){
            priceSum = 0; //累計價格歸零
            volumeSum = 0; //累計體積歸零
            preferenceSum2 = 0;
            preferenceSum = 1; //累計偏好值為一
            for (int j = 0; j < itemValues; j++){
                priceSum += gaMergeArray[i][j][1]; //價錢
                volumeSum += gaMergeArray[i][j][2]; //體積
                preferenceSum *= calculation_preference(gaMergeArray[i][j][6]);
                preferenceSum2 += gaMergeArray[i][j][6]; //計算總偏好
            }
            /****** 計算價格適應值 *******/
            priceFitnessArray[i] = fabs(maxPrice - priceSum) / maxPrice;
            if ((maxPrice - priceSum) == 0)
                priceFitnessArray[i] += 1;
            else if ((maxPrice - priceSum) > 0)
                priceFitnessArray[i] += 2;
            else if ((maxPrice - priceSum) < 0)
                priceFitnessArray[i] += 3;
            /****** 計算體積適應值 *******/
            volumeFitnessArray[i] = fabs(maxVolume - volumeSum) / maxVolume;
            if ((volumeSum >= (maxVolume * 0.7)) && (volumeSum <= maxVolume)){
                if ((maxVolume - volumeSum) == 0)
                    volumeFitnessArray[i] += 1;
                else 
                    volumeFitnessArray[i] += 2;
            } else {
                volumeFitnessArray[i] += 3;
            }
            preferenceFitnessArray[i] = preferenceSum;
            priceTotalArray[i] = priceSum;
            volumeTotalArray[i] = volumeSum;
            preferenceTotalArray[i] = preferenceSum2;
            /****** 計算偏好適應值 *******/
            gaScoreArray[i] = priceFitnessArray[i] * volumeFitnessArray[i] * preferenceFitnessArray[i];
        }

        /***** 印出所有染色體的適應函數值 *****/
        // for (int i = 0; i < populationCount*2; i++){
        //     printf("=====Chrom - %d=====\n", i);
        //     printf("Preference Fit: %lf\n", preferenceFitnessArray[i]);
        //     printf("Price Fit: %lf\n", priceFitnessArray[i]);
        //     printf("Volume Fit: %lf\n", volumeFitnessArray[i]);
        //     printf("Total Fit: %lf\n", gaScoreArray[i]);
        // }
        // printf("=========================================\n");

        /****** 將父母與孩子總計populationCount*2條染色體進行排序 *******/
        double scoreTemp = 0;
        for (int i = 0; i < (populationCount*2); i++){
            for (int j = 0; j < i; j++){
                if (gaScoreArray[j] > gaScoreArray[i]){
                    scoreTemp = gaScoreArray[j]; //將總適應函數較小的染色體先給變數
                    gaScoreArray[j] = gaScoreArray[i]; 
                    gaScoreArray[i] = scoreTemp;
                    for (int k = 0 ; k < itemValues; k++){
                        for (int l = 0; l < columnLen; l++){
                            /* 品項資料交換 */
                            scoreTemp = gaMergeArray[j][k][l];
                            gaMergeArray[j][k][l] = gaMergeArray[i][k][l];
                            gaMergeArray[i][k][l] = scoreTemp;

                            /* 總價格交換 */
                            scoreTemp = priceTotalArray[j];
                            priceTotalArray[j] = priceTotalArray[i];
                            priceTotalArray[i] = scoreTemp;

                            /* 總體機交換 */
                            scoreTemp = volumeTotalArray[j];
                            volumeTotalArray[j] = volumeTotalArray[i];
                            volumeTotalArray[i] = scoreTemp;

                            /* 總偏好交換 */
                            scoreTemp = preferenceTotalArray[j];
                            preferenceTotalArray[j] = preferenceTotalArray[i];
                            preferenceTotalArray[i] = scoreTemp;
                        }
                    }
                }
            }
        }

        /***** 印出排序的父母與孩子分數 *****/
        // for (int i = 0; i < populationCount*2; i++){
        //     printf("=====Chrom - %d=====\n", i);
        //     printf("Preference Fit: %lf\n", preferenceFitnessArray[i]);
        //     printf("Price Fit: %lf\n", priceFitnessArray[i]);
        //     printf("Volume Fit: %lf\n", volumeFitnessArray[i]);
        //     printf("Total Fit: %lf\n", gaScoreArray[i]);
        // }

        /****** 從父母與後代中挑選出好的populationCount個 *******/
        num = 0;
        for (int i = 0; i < populationCount*2; i++){
            weightSum = 0;
            for (int j = 0; j < itemValues; j++){
                weightSum += gaMergeArray[i][j][5]; //重量
            }
            /****** 重量不符所以拋棄 *******/
            if (weightSum <= maxWeight){
                for (int k = 0 ; k < itemValues; k++){
                    for (int l = 0; l < columnLen; l++){
                        gaPopulationArray[num][k][l] = gaMergeArray[i][k][l];
                    }
                }
                num++;
            }
            if (num >= populationCount){
                break;
            }
        }

        /***** 印出下一代的染色體 *****/
        // for(int i = 0; i < populationCount; i++){
        //     printf("======== New pop: %d ========\n", i);
        //     for (int j = 0; j < itemValues; j++){
        //         printf("%d: ", j);
        //         for (int k = 0; k <columnLen; k++){
        //             printf("%lf, ", gaPopulationArray[i][j][k]);
        //         }
        //         printf("\n");
        //     }
        // }

        /******* 開始釋放陣列記憶體 ******/
        // realloc(gaSelectArray, 0);
        // realloc(crossOverArray, 0);
        // realloc(randArray, 0);
        // realloc(gaMergeArray, 0);
        // realloc(weightSum, 0);
        // realloc(gaScoreArray, 0);
        // free(gaSelectArray);
        // free(crossOverArray);
        // free(randArray);
        // free(gaMergeArray);
        // free(gaScoreArray);
        
        genPreferenceBest[iteration] = preferenceTotalArray[0]; //紀錄每一代最好的偏好值
        genVolumeBest[iteration] = volumeTotalArray[0]; //紀錄每一代最好的體積率
        genPriceBest[iteration] = priceTotalArray[0]; //紀錄每一代最好的總價格
        genTotalFitBest[iteration] = gaScoreArray[0]; //紀錄每一代總體適應函數
        
        printf("========== Generation %d =========\n", iteration);
        
        for (int j = 0; j < itemValues; j++) {
            for (int k = 0; k <columnLen; k++){
                printf("%lf, ", gaPopulationArray[0][j][k]);
            }
             printf("\n");
        }
        printf("Total Fit: %lf\n", genTotalFitBest[iteration]);
        printf("Total Price: %lf\n", genPriceBest[iteration]);
        printf("Total Volume: %lf\n", genVolumeBest[iteration]);
        printf("Total Preference: %lf\n", genPreferenceBest[iteration]);

        // printf("Total Fit: %lf\n", gaScoreArray[0]);
        // printf("Total Price: %lf\n", priceTotalArray[0]);
        // printf("Total Volume: %lf\n", volumeTotalArray[0]);
        // printf("Total Preference: %lf\n", preferenceTotalArray[0]);
        iteration++;
    }
    finish = clock(); 
    printf("Total time: %f seconds\n", ((double)(finish - start)) / CLOCKS_PER_SEC); 

    resultTxt = fopen("GA_result.txt", "w");
    if(resultTxt == NULL){
        printf("Can't create file.\n");
    }   

    for (int j = 0; j < itemValues; j++) {
        for (int k = 0; k <columnLen; k++){
            if ( k == (columnLen - 1)) {
                fprintf(resultTxt, "%lf", gaPopulationArray[0][j][k]);
            } else {
                fprintf(resultTxt, "%lf,", gaPopulationArray[0][j][k]);
            }
        }
        fprintf(resultTxt, "\n");
    }
    fclose(resultTxt);
    return 0;
}