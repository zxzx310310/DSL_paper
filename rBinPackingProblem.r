#----�ϥΪ̻ݿ�J���Ѽ�(���])----
args <- commandArgs(TRUE)
dietHabit <- as.character(args[1]) #������
if(dietHabit == "����") {
  userItemValues <- as.integer(args[2]) #�ϥΪ̻ݭn���ƶq
  maxPrice <- as.integer(unlist(strsplit(as.character(args[3]),split="-",fixed=T))[2]) #�̤j���B
  exceptBrandList <- as.character(args[4]) #�N�n�簣���~�P
  userPreference <- as.integer(c(args[5], args[6], args[7], args[8], args[9],
                                 args[10], args[11], args[12], args[13], args[14],
                                 args[15], args[16], args[17], args[18], args[19], args[20])) #�������@��
} else if(dietHabit == "����"){
  userItemValues <- as.integer(args[2]) #�ϥΪ̻ݭn���ƶq
  maxPrice <- as.integer(unlist(strsplit(as.character(args[3]),split="-",fixed=T))[2]) #�̤j���B
  exceptBrandList <- as.character(args[4]) #�N�n�簣���~�P
  userPreference <- as.integer(c(args[5], args[6], args[7], args[8], args[9],
                                 args[10], args[11], args[12], args[13], args[14],
                                 args[15], args[16])) #���������@��
}

#write.table(x = c(dietHabit, userItemValues, maxPrice, exceptBrandList, userPreference), file = "test.txt", sep = ",")

#----��ƪ�l��(��Ʈw)----
pkgs = "RMySQL"
pkgs = pkgs[!( pkgs %in% installed.packages()[,"Package"] )]
if(length(pkgs)) install.packages(pkgs) #�T�{�O�_�����M��, �Y�L�N�w��
library(RMySQL)

mydb = dbConnect(MySQL(), user="root", password="", dbname="rpreferdatabase", host='127.0.0.1')
dbSendQuery(mydb,"SET NAMES big5")
result <- dbSendQuery(mydb, 'SELECT * FROM classicbase;')

sourceData <- fetch(result, n = -1)

sourceData <- sourceData[c(-1, -13)] #���������n��������

names(sourceData)[1] <- "���~�N��"
names(sourceData)[2] <- "�~�W"
names(sourceData)[3] <- "���"
names(sourceData)[4] <- "��n"
names(sourceData)[5] <- "�t�P"
names(sourceData)[6] <- "��"
names(sourceData)[7] <- "�e"
names(sourceData)[8] <- "��"
names(sourceData)[9] <- "����"
names(sourceData)[10] <- "����"
names(sourceData)[11] <- "���q" #���s�R�W���W��

sourceData$'���' <- as.integer(sourceData$'���')
sourceData$'��n' <- as.numeric(sourceData$'��n')
sourceData$'�t�P' <- as.factor(sourceData$'�t�P')
sourceData$'����' <- as.factor(sourceData$'����')
sourceData$'����' <- as.factor(sourceData$'����')
sourceData$'���q' <- as.numeric(sourceData$'���q')

goodData <- sourceData #�N��l��ƽƻs�@��
goodData <- cbind(goodData, "Selected" = 0, "Preference" = 1) #�s�W�Q������


#----���ҰѼƳ]�w----
maxVolume <- 47*32*39 #�̤j�c�l��n
maxWeight <- 16000 #�̤j���q
alpha <- 0.936 #��n�P���]�l
popAmount <- 30 #�H�f�ƶq
crossRate <- 0.7 #��t�v
mutationRate <- 0.01 #���ܲv
eliteValues <- 1 #�׭^�ƶq
maxGen <- 500 #�@�N����

#----Function----
#���n�ȻP���O�X��
preference_match <- function(good_data, require_goods, non_require_goods, user_preference) {
  total_list <- as.character(c(require_goods, non_require_goods)) #�N���ݩʰӫ~�P��ܩʰӫ~���O�X��
  good_preference <- data.frame(category = total_list, preference = c(sample(1, length(require_goods), replace = TRUE), user_preference)) #�N�ӫ~���O�M���n�ȦX�֬�DF���A

  for (i in 1:dim(good_preference)[1]) {
    # good_data[good_data$����==good_preference$category[i],]$Preference <- as.numeric(good_preference$preference[i])^2
    good_data[good_data$����==good_preference$category[i],]$Preference <- as.numeric(good_preference$preference[i])
  }

  return(good_data)
}


#�簣�~�P����k
except_brand <- function(good_data, except_brand_list) {
  #good_data: ��l�ӫ~��ƶ�
  #except_brand_list: �簣�~�P���W��

  for (i in 1:length(except_brand_list)) {
    good_data <- good_data[good_data$'�t�P'!=except_brand_list[i],] #�N�n�簣���t�P����
  }
  return(good_data)
}


#�����������
diet_select <- function(good_data, diet_habit_list) {
  #good_data: ��l�ӫ~��ƶ�
  #diet_habit_list: �����ί��������

  if(diet_habit_list=="����") {
    good_data <- good_data[good_data$'����'==diet_habit_list,] #�p�G�O�����N�N�ݩ���������~�z��X��
    good_data$'����' <- factor(good_data$����)
  }

  return(good_data)
}


#��l�H�f��k3(�L����, ��ܩʰӫ~���Ѽ�, ���q���w�ʱ���)
initial_pop <- function(good_data, require_goods, non_require_goods, non_require_values, limit_weight) {
  #good_data: ��l�ӫ~��ƶ�
  #require_goods: ���n�ʪ��ӫ~�M��
  #non_require_goods: �����n�ʪ��ӫ~�M��
  #non_require_values: �����n�ʪ��ӫ~�ƶq
  #limit_weight: �̤j���q����

  temp_good <- good_data #���N��l��ƼȮɵ��t�~�@���ܼƨϥ�

  for (i in 1:length(require_goods)) {
    get_index <- sample(which(temp_good$'����'==require_goods[i] & temp_good$'Selected'!=1), 1) #�H����X�ŦX������Selected��줣����1���C
    temp_good$'Selected'[get_index] <- 1 #�N�Q��ܪ����אּ1
  }

  for (i in 1:non_require_values) {
    category_goods <- sample(non_require_goods, 1) #�H���D���ܩʰӫ~�����O
    get_index <- sample(which(temp_good$'����'==category_goods & temp_good$'Selected'!=1), 1) #�H����X�ŦX������Selected��줣����1���C
    temp_good$'Selected'[get_index] <- 1 #�N�Q��ܪ����אּ1
  }

  selected_good <- temp_good[temp_good$'Selected'==1,] #�N�Q��ܪ��ӫ~��J�s�ܼƤ�
  sum_weight <- sum(selected_good$'���q') #�p�⦹��]���`���q

  while (sum_weight > limit_weight) {
    #���q���w�ʱ���
    #���}�l�P�O�Ĥ@�����ͪ���l�H�f�`���q���S���W�L�w�����̤j��, �Y���N�~���H����ܰӫ~�X�Ӱ���l�H�f, �����`���q�S���j��̤j��

    print(paste("���q�W�L:", sum_weight, ">", limit_weight))
    temp_good <- good_data #���s���@����l���, �]selected��즳�Q�ק�L

    for (i in 1:length(require_goods)) {
      get_index <- sample(which(temp_good$'����'==require_goods[i] & temp_good$'Selected'!=1), 1) #�H����X�ŦX������Selected��줣����1���C
      while (dim(temp_good[temp_good$'����'==require_goods[i],])[1]==1) {
        #�Y�Ӻ������ӫ~�ƶq����1, �h���s�H����ܨ�L���O
        get_index <- sample(which(temp_good$'����'==require_goods[i] & temp_good$'Selected'!=1), 1) #�H����X�ŦX������Selected��줣����1���C
      }
      temp_good$'Selected'[get_index] <- 1 #�N�Q��ܪ����אּ1
    }

    for (i in 1:non_require_values) {
      category_goods <- sample(non_require_goods, 1) #�H���D���ܩʰӫ~�����O
      while (dim(temp_good[temp_good$'����'==category_goods,])[1]==1) {
        #�p�G���o�쪺��m�Ӱӫ~�u���@�ӥB�w�g�Q���, �S���ӫ~�i���, �A���s�D��ظ�ӫ~
        remove_index <- which(non_require_goods==category_goods)
        category_goods <- sample(non_require_goods[-remove_index], 1) #�H���D���ܩʰӫ~�����O
      }
      get_index <- sample(which(temp_good$'����'==category_goods & temp_good$'Selected'!=1), 1) #�H����X�ŦX������Selected��줣����1���C
      temp_good$'Selected'[get_index] <- 1 #�N�Q��ܪ����אּ1
    }

    selected_good <- temp_good[temp_good$'Selected'==1,] #�N�Q��ܪ��ӫ~���s��J�ܼƤ�
    sum_weight <- sum(selected_good$'���q') #�p�⦹��]���`���q
  }

  return(selected_good) #�^�ǵ��G
}


#�s�X�V����
create_chromosome <- function(gene_list) {
  #gene_list: �Q��ܥX����]�M��
  for(i in 1:length(gene_list)) {
    chromosome <- as.vector(gene_list[[i]][[1]]$'���~�N��')
    gene_list[[i]]["chromosome"] <- list(chromosome)
  }
  return(gene_list)
}


#�p���`���q
total_weight <- function(gene_list) {
  #gene_list: �Q��ܥX����]�M��
  for(i in 1:length(gene_list)) {
    sum_weight <- sum(gene_list[[i]][[1]]$'���q')
    gene_list[[i]]["totalWeight"] <- list(sum_weight)
  }
  return(gene_list)
}


#���n���A���פ�k(�w�[�J�g�@��)
fitness_preference <- function(gene_list, require_goods, non_require_values) {
  #�Q��ܥX����]�M��
  print("�}�l�p�ⰾ�n�A�����...")
  for(i in 1:length(gene_list)) {
    reuslt <- 1
    for (k in 1:sum(length(require_goods), non_require_values)) {
      temp_preferencd <- 1+as.numeric((geneList[[i]][[1]]$'Preference'[k])^2 - 1) / sum(1:non_require_values) #���n���p�⤽��
      reuslt <- reuslt*temp_preferencd
    }
    gene_list[[i]]["fitPreference"] <- list(reuslt)
    print(paste("���n�A�����:", reuslt))
    print(paste("========��", i, "�Ӱ�]========"))
  }
  return(gene_list)
}


#��n���A���פ�k(�w�[�J�g�@��)
fitness_volume <- function(gene_list, bin_volume, volume_alpha) {
  #gene_list: �Q��ܥX����]�M��
  #bin_volume: �c�l�����n
  #volume_alpha: �c�l����n�P���]�l
  print("�}�l�p����n�A�����...")
  for (i in 1:length(gene_list)) {
    limit_volume <- bin_volume*volume_alpha #��n�P�P���]�l�ۭ�
    sum_volume <- sum(gene_list[[i]][[1]]$'��n') #�N�̤j������n��h�C�Ӱ�]���`��n
    reuslt <- (limit_volume-sum_volume)/limit_volume #�N��n�A���׺�X
    if (reuslt==0 || reuslt>0 && reuslt<=0.1) {
      reuslt <- reuslt + 1 #�Y�A���׵���0�Τj��0�äp�󵥩�0.1�N�����g�@��1, e.g. (49795.2-27749.25)/49795.2=0.4427324, �U����0��ܻ���t�Z�V�p
    } else if (reuslt>0.1 && reuslt<=0.5) {
      reuslt <- reuslt + 2 #�Y�A���פj��0.1�äp�󵥩�0.5�N�����g�@��2
    } else {
      reuslt <- reuslt + 3 #�ѤU���G�N�����g�@��3
    }
    gene_list[[i]]["fitVolume"] <- reuslt
    print(paste("��n�A�����:", reuslt))
    print(paste("========��", i, "�Ӱ�]========"))
  }
  return(gene_list)
}


#���檺�A���פ�k(�w�[�J�g�@��)
fitness_price <- function(gene_list, limit_price) {
  #gene_list: �Q��ܥX����]�M��
  #limit_price: ����̰�����
  print("�}�l�p�����A�����...")
  for (i in 1:length(gene_list)) {
    sum_price <- sum(gene_list[[i]][[1]]$'���') #�N�̤j������B��h�C�Ӱ�]���`���B
    reuslt <- (limit_price-sum_price)/limit_price #�N����A���׺�X
    if (reuslt==0 || reuslt>0 && reuslt<=0.1) {
      reuslt <- reuslt + 1 #�Y�A���׵���0�Τj��0�äp�󵥩�0.1�N�����g�@��1, e.g. (1200-1123)/1200=0.06416667, �U����0��ܻ���t�Z�V�p
    } else if (reuslt>0.1 && reuslt<=0.5) {
      reuslt <- reuslt + 2 #�Y�A���פj��0.1�äp�󵥩�0.5�N�����g�@��2
    } else {
      reuslt <- reuslt + 3 #�ѤU���G�N�����g�@��3
    }
    gene_list[[i]]["fitPrice"] <- reuslt
    print(paste("����A�����:", reuslt))
    print(paste("========��", i, "�Ӱ�]========"))
  }
  return(gene_list)
}


#�`�骺�A���פ�k
fitness_total <- function(gene_list) {
  #gene_list: �Q��ܥX����]�M��

  print("�}�l�p���`��A�����...")
  sum_fit <- unlist(lapply(fitnessPriceAfter, function(x) x$fitVolume*x$fitPrice))

  for (i in 1:length(gene_list)) {
    sum_fit <- gene_list[[i]]$'fitVolume'*gene_list[[i]]$'fitPrice'*gene_list[[i]]$'fitPreference'

    gene_list[[i]]["totalFit"] <- sum_fit
    print(paste("�`��A�����:", sum_fit))
    print(paste("========��", i, "�Ӱ�]========"))
  }
  return(gene_list)
}


#��t(���I��t)-�ݦҼ{�A����ƭ�(�]�t�g�@��)�B��t�v�M���q����
cross_over <- function(gene_list, require_goods, non_require_values, cross_rate, limit_weight) {

  for (i in 1:length(gene_list)) {
    #��������t���A, 0��ܥ���t, 1��ܤw��t
    gene_list[[i]]["crossState"] <- 0
  }
  get_chrom_length <- length(require_goods)+non_require_values #���o�V�������
  print("�}�l��t")

  for(i in 1:c(length(gene_list)/2)){
    get_cross_state <- unlist(lapply(gene_list, function(x) x$crossState)) #���w�ثe��t���A
    rnd_cross_rate <- round(runif(n = 1, min = 0, max = 1),3) #���Ͷü�
    get_index <- as.vector(sample(which(get_cross_state!=1),2)) #����n�Q��t����]

    loop_value <- 0

    if(rnd_cross_rate<=cross_rate) {
      #�üƤp�󵥩��t�v, �h�i���t

      divide_index <- sort(as.vector(sample(get_chrom_length, 2))) #�H����ܤ��Φa��(�����I��t)

      tempChrom_A <- gene_list[[get_index[1]]] #���N�V���鵹�Ȯ��ܼ�A
      tempChrom_B <- gene_list[[get_index[2]]] #���N�V���鵹�Ȯ��ܼ�B
      tempChrom_A$'chromosome'[(divide_index[1]+1):divide_index[2]] <- gene_list[[get_index[2]]]$'chromosome'[(divide_index[1]+1):divide_index[2]] #�}�l�i���t, �N�ĤG�Ӱ�]���Ϊ��V���鵹�Ĥ@�Ӱ�]
      tempChrom_B$'chromosome'[(divide_index[1]+1):divide_index[2]] <- gene_list[[get_index[1]]]$'chromosome'[(divide_index[1]+1):divide_index[2]] #�}�l�i���t, �N�Ĥ@�Ӱ�]���Ϊ��V���鵹�ĤG�Ӱ�]
      tempChrom_A[[1]][(divide_index[1]+1):divide_index[2],] <- gene_list[[get_index[2]]][[1]][(divide_index[1]+1):divide_index[2],] #�}�l�i���t, �N�ĤG�Ӱ�]���Ϊ��ӫ~���Ĥ@�Ӱ�]
      tempChrom_B[[1]][(divide_index[1]+1):divide_index[2],] <- gene_list[[get_index[1]]][[1]][(divide_index[1]+1):divide_index[2],] #�}�l�i���t, �N�Ĥ@�Ӱ�]���Ϊ��ӫ~���ĤG�Ӱ�]
      tempChrom_A$'totalWeight' <- sum(tempChrom_A[[1]]$'���q') #���s�p���`���q
      tempChrom_B$'totalWeight' <- sum(tempChrom_B[[1]]$'���q') #���s�p���`���q

      while (tempChrom_A$'totalWeight' > limit_weight || tempChrom_B$'totalWeight' > limit_weight && length(tempChrom_A$chromosome)!=length(unique(tempChrom_A$chromosome))) {
        loop_value <- loop_value+1

        print(paste("���q�W�L:", tempChrom_A$'totalWeight', "��", tempChrom_B$'totalWeight', ">", limit_weight, "�� �����ƭ�"))

        get_index <- as.vector(sample(which(get_cross_state!=1),2)) #���s����n�Q��t����]
        divide_index <- sort(as.vector(sample(get_chrom_length, 2))) #���s�H����ܤ��Φa��(�����I��t)

        tempChrom_A <- gene_list[[get_index[1]]] #���N�V���鵹�Ȯ��ܼ�A
        tempChrom_B <- gene_list[[get_index[2]]] #���N�V���鵹�Ȯ��ܼ�B
        tempChrom_A$'chromosome'[(divide_index[1]+1):divide_index[2]] <- gene_list[[get_index[2]]]$'chromosome'[(divide_index[1]+1):divide_index[2]] #�}�l�i���t, �N�ĤG�Ӱ�]���Ϊ��V���鵹�Ĥ@�Ӱ�]
        tempChrom_B$'chromosome'[(divide_index[1]+1):divide_index[2]] <- gene_list[[get_index[1]]]$'chromosome'[(divide_index[1]+1):divide_index[2]] #�}�l�i���t, �N�Ĥ@�Ӱ�]���Ϊ��V���鵹�ĤG�Ӱ�]
        tempChrom_A[[1]][(divide_index[1]+1):divide_index[2],] <- gene_list[[get_index[2]]][[1]][(divide_index[1]+1):divide_index[2],] #�}�l�i���t, �N�ĤG�Ӱ�]���Ϊ��ӫ~���Ĥ@�Ӱ�]
        tempChrom_B[[1]][(divide_index[1]+1):divide_index[2],] <- gene_list[[get_index[1]]][[1]][(divide_index[1]+1):divide_index[2],] #�}�l�i���t, �N�Ĥ@�Ӱ�]���Ϊ��ӫ~���ĤG�Ӱ�]
        tempChrom_A$'totalWeight' <- sum(tempChrom_A[[1]]$'���q') #���s�p���`���q
        tempChrom_B$'totalWeight' <- sum(tempChrom_B[[1]]$'���q') #���s�p���`���q

        # if(loop_value > 500) {
        #   print("�w���L�a��, ���i���t")
        #   break
        # }
        temp_log <- as.numeric(table(tempChrom_A$'chromosome'==tempChrom_B$'chromosome')["TRUE"])

        if(temp_log == get_chrom_length) {
          print("���X")
          break
        }
      }

      tempChrom_A$'crossState' <- 1
      tempChrom_B$'crossState' <- 1
      gene_list[[get_index[1]]] <- tempChrom_A
      gene_list[[get_index[2]]] <- tempChrom_B

      print("��t�o!")
      print(paste("========��", i, "��========"))
    } else {
      #�üƤj���t�v, �h���i���t
      gene_list[[get_index[1]]]$'crossState' <- 1
      gene_list[[get_index[2]]]$'crossState' <- 1
      print("�S��t!")
      print(paste("========��", i, "��========"))
    }
  }
  return(gene_list)
}


#���ܤ�k, �[�J���q����
mutation_FN <- function(good_data, gene_list, mutation_rate, require_goods, non_require_values, soure_data, limit_weight) {
  #good_data: �ӫ~��ƶ�
  #gene_list: �w��t�L����]�H�f�s
  #mutation_rate: ��t�v
  #require_goods: ���n�ʰӫ~�M��
  #non_require_values: ��ܩʰӫ~�ƶq
  #soure_data: ��l���(���g�L�ק諸)

  get_chrom_length <- length(require_goods)+non_require_values #��]�V���骺����
  temp_list <- gene_list #���N��Ƶ��t�~�@���ܼ�
  print("�}�l����")

  for(i in 1:length(gene_list)) {
    mutation_index <- as.numeric(sample(get_chrom_length, 1)) #�H�����o�n���ܪ���m
    rnd_mutation_rate <- runif(n = 1, min = 0, max = 1) #���Ͷü�
    print(paste("�üƬ��ܲv:", rnd_mutation_rate)) #��ܶüƪ����ܲv

    if(rnd_mutation_rate <= mutation_rate){
      mutation_category <- as.character(temp_list[[i]][[1]][mutation_index,]$'����') #���o��]���n�Q���ܪ��V����ӫ~����
      mutation_list <- soure_data[soure_data$'����'==mutation_category,] #���o��l��Ƥ��ŦX�n�Q���ܪ��ӫ~����
      temp_good_data <- good_data[good_data$'����'==mutation_category, ] #���o��ƶ��Ӻ��������

      repeat {
        #���ƿz��ӫ~, ����S���P�쥻��]�ۦP���ӫ~
        rnd_mutation_value <- sample(nrow(mutation_list), 1) #�H�����o�Ӻ������ӫ~

        comput_weight_list <- temp_list[[i]][[1]] #�ȮɱN�B�z������Ƶ��t�@�ܼ�
        comput_weight_list[mutation_index,] <- mutation_list[rnd_mutation_value,] #�N���o����ܪ���m�i�����
        sum_weight <- sum(comput_weight_list$'���q') #�p����ܫ��`���q

        if(temp_list[[i]][[1]][mutation_index,]$'���~�N��'!=mutation_list[rnd_mutation_value,]$'���~�N��' && sum_weight < limit_weight && dim(temp_good_data)[1]!=1){
          #��ӫ~��̤��P��, �ì��ܫ᪺���q���i�j��̤j���q����, �B�Ӭ��ܦ�m���ӫ~�����ƶq������1, �h���X, ��ܤw����D���ƪ��ӫ~, �B�`���q�S���W�L����, �θӰӫ~�����b�簣�~�P��ƶq������1
          break
        }
        print(paste("�ӫ~���ƥB���q�W�L:", sum_weight, "<", limit_weight))
        mutation_index <- as.numeric(sample(get_chrom_length, 1)) #�H�����o�n���ܪ���m
        mutation_category <- as.character(temp_list[[i]][[1]][mutation_index,]$'����') #���o��]���n�Q���ܪ��V����ӫ~����
        mutation_list <- soure_data[soure_data$'����'==mutation_category,] #���o��l��Ƥ��ŦX�n�Q���ܪ��ӫ~����
        temp_good_data <- good_data[good_data$'����'==mutation_category, ] #���o��ƶ��Ӻ��������
      }
      temp_list[[i]][[1]][mutation_index,] <- mutation_list[rnd_mutation_value,] #�N�ӫ~�i���ܲ�
      temp_list[[i]]$'chromosome'[mutation_index] <- as.character(mutation_list[rnd_mutation_value,]$'���~�N��') #�N��]�i���ܲ�
      temp_list[[i]]$'totalWeight' <- sum_weight

      print("��������!")
    } else {
      print(paste(rnd_mutation_rate, ">=", mutation_rate)) #��ܥثe���ܲv
    }
    print(paste("========��", i, "��========"))
  }
  return(temp_list)
}


#���N�P�U�N�X�֪���k, �ìD��X�s���ڸs(�ĥε׭^�F��)
new_population <- function(first_gene, second_gene, elite_values, pop_amount) {
  #first_gene: ���N��]
  #second_gene: �U�@�N��]
  #elite_values: �׭^�ƶq
  #pop_amount: �ڸs�j�p

  new_pop <- list()
  new_pop <- first_gene #�N���N��]��J�s���ܼ�
  new_pop <- append(new_pop, second_gene) #�N�U�N��]�[�J�ܼ�

  for (i in 1:length(new_pop)) {
    #���͵׭^���A
    new_pop[[i]]["elite"] <- 0 #�N�׭^���A�ҳ]�w��0, 0��ܤ��O�׭^��; 1��ܳQ�D�אּ�׭^��
  }

  result_pop <- list() #��l�̲׵��G�ڸs�M��

  last_list <- head(order(unlist(lapply(new_pop, function(x) x$totalFit)), decreasing = FALSE), pop_amount) #���opopAmount(�ƶq)���ڸs
  for (z in 1:length(elite_values)) {
    #�N�̦n���A����Ƴ]�w����^��, �é�J�s���s��
    new_pop[[last_list[z]]]$'elite' <- 1
    result_pop[[z]] <- new_pop[[last_list[z]]] #�N�׭^����]��J�s���ڸs��
    print(paste("�׭^�s�X:", unlist(result_pop[[z]]$'chromosome')))
    print(paste("�׭^���`��A�����:", result_pop[[z]]$'totalFit'))
  }

  for (k in (length(elite_values)+1):pop_amount) {
    #�N�ѤU����]�[�J��ڸs
    result_pop[[k]] <- new_pop[[last_list[k]]]
  }

  return(result_pop)
}


#----����----
#��������k
goodData <- diet_select(good_data = goodData, diet_habit_list = dietHabit)
level <- levels(goodData$'����')
level <- level[order(nchar(level), level)]
#levels(goodData$'����') <- level[order(nchar(level), level)]
#goodData$'����' <- factor(goodData$'����', levels = level)
requiredList <- level[order(nchar(level), level)][1:6]
#requiredList <- level[order(nchar(level), level)][1:6]
if (dietHabit == "����") {
  nonRequiredList <- c("�V���P����", "�ѱ�", "�R�w��", "���Y_��", "���Y_��", "���Y_��ۣ", "���Y_����_�h�J��", "���Y_�ѵ�_�h�J��", "����_�T��_�a�x��", "����_���~_�h�J��", "�w��_�a�x��", "�氮_��G���a", "�氮_�զX�]", "�氮_Ĭ����", "�氮_�v����", "�氮_����p�I")
} else {
  nonRequiredList <- c("�V���P����", "�ѱ�", "�R�w��", "���Y_��", "���Y_��ۣ", "���Y_�ѵ�_�h�J��", "����_�T��_�a�x��", "����_���~_�h�J��", "�氮_��G���a", "�氮_�զX�]", "�氮_�v����", "�氮_����p�I")
}
#nonRequiredList <- level[order(nchar(level), level)][-1:-length(requiredList)]
nonRequiredValues <-  length(nonRequiredList) #��ܩʰӫ~���ƶq
#userPreference <- sample(c(1:length(nonRequiredList)), length(nonRequiredList), replace = FALSE)

goodData <- preference_match(good_data = goodData, require_goods = requiredList, non_require_goods = nonRequiredList, user_preference = userPreference)

#�簣�����Q�n���~�P
goodData <- except_brand(good_data = goodData, except_brand_list = exceptBrandList)

#���ͪ�l�f(���popAmount�ƶq)
geneList <- list()
for (i in 1:popAmount) {
  gene <- initial_pop(good_data = goodData, require_goods = requiredList, non_require_goods = nonRequiredList, non_require_values = nonRequiredValues, limit_weight = maxWeight)
  geneList[[i]] <- list(gene)
}

#�s�X�V����
geneList <- create_chromosome(gene_list = geneList)

#�p��C�Ӱ�]�`���q
geneList <- total_weight(gene_list = geneList)

#�p�ⰾ�n�A����(�ثe�ȭp���`���n��)
fitnessPreference <- list()
fitnessPreference <- fitness_preference(gene_list = geneList, require_goods = requiredList, non_require_values =  nonRequiredValues)

#�p����n�A����
fitnessVolumeAfter <- list()
fitnessVolumeAfter <- fitness_volume(gene_list = fitnessPreference, bin_volume = maxVolume, volume_alpha = alpha)

#�p�����A����
fitnessPriceAfter <- list()
fitnessPriceAfter <- fitness_price(gene_list = fitnessVolumeAfter, limit_price = maxPrice)

#�p���`��A����
fitnessTotalAfter <- list()
fitnessTotalAfter <- fitness_total(gene_list = fitnessPriceAfter)

#�}�l�i���t
crossAfter <- list()
crossAfter <- cross_over(gene_list = fitnessTotalAfter, require_goods = requiredList, non_require_values = nonRequiredValues, cross_rate = crossRate, limit_weight = maxWeight)

#�}�l�i�����
mutationAfter <- list()
mutationAfter <- mutation_FN(good_data = goodData, gene_list = crossAfter, mutation_rate = mutationRate, require_goods = requiredList, non_require_values = nonRequiredValues, soure_data = goodData, limit_weight = maxWeight)

#�p��s�@�N��]���������n���A�����
mutationAfter <- fitness_volume(gene_list = mutationAfter, bin_volume = maxVolume, volume_alpha = alpha)
mutationAfter <- fitness_price(gene_list = mutationAfter, limit_price = maxPrice)
mutationAfter <- fitness_total(gene_list = mutationAfter)


#�즹��m�w�g���Ĥ@�N�P�ĤG�N����]
#��l��]��fitnessPriceAfter
#�ĤG�N��]��mutationAfter

#�X�֦��N�P�U�@�N��], �ñĥε׭^�F���M���X�s���ڸs
newPopulation <- new_population(first_gene = fitnessTotalAfter, second_gene = mutationAfter, elite_values = eliteValues, pop_amount = popAmount)


gen_values_best <- vector() #�����̦n����]�`��A�����
gen_values_loss <- vector() #�����̮t����]�`��A�����
for (i in 1:maxGen) {
  #�}�l�i���]�t��
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
  print(paste("============��", i, "�N============"))
}

nowDateTime <- unlist(strsplit(as.character(Sys.time()), split = " ")) #���Τ���P�ɶ�
resultDF <- newPopulation[[1]][[1]][-12] #���������n���
resultDF <- cbind(resultDF, ����W�O = nowDateTime[1], �ɶ��W�O = nowDateTime[2]) #�W�[�ɶ����W�O

mydb = dbConnect(MySQL(), user="root", password="", dbname="rpreferdatabase", host='127.0.0.1')
dbSendQuery(mydb,"SET NAMES big5")
dbWriteTable(mydb, name = "history", value = resultDF, append = TRUE, field.types=list(���~�N�� = "varchar(100)", �~�W = "varchar(100)", ��� = "int(100)", ��n = "double(10,2)", �t�P = "varchar(100)", �� = "double(10,2)", �e = "double(10,2)", �� = "double(10,2)", ���� = "	varchar(100)", ���� = "	varchar(100)", ���q = "int(100)", Preference = "int(100)", ����W�O = "varchar(100)", �ɶ��W�O = "varchar(100)"), row.names = FALSE) #��Ʈw�s�X�г]�wbig5_chinese_ci
dbDisconnect(mydb)

#Rscript rBinPackingProblem.r "����" 22 1300-1599 "�j�P" 11 2 8 13 5 16 1 9 10 3 6 7 14 4 12 1

#write.csv(x = newPopulation[[1]][[1]][-12], file = "solution.csv", row.names = FALSE)