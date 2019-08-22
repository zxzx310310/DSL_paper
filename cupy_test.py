# -*- coding: utf-8 -*-
import numpy as np
import cupy as cp

#numX = 10
#numY = 20
numX = cp.array([1, 1, 1, 1, 1, 1, 10, 11, 16, 12, 1, 2, 3, 4, 5, 6, 9, 8, 7, 13, 14, 15], dtype = 'float64')
numY = cp.array([20, 45], dtype = 'float64')

kernel = cp.ElementwiseKernel(
        'int64 x, int64 y', 'int64 z',
        '''int max = 16;
        z = max;''', 'my_kernel')

kernel2 = cp.ElementwiseKernel(
        'float64 x, float64 y', 'float64 result',
        '''double temp = 0;
        for(int i = 0; i < 20; i = i + 1){
          temp = temp + x;
        }
        result = temp + y;
        ''', 'my_kernel2')

kernel3 = cp.ElementwiseKernel(
        'float64 input', 'float64 res',
        '''double sum = 0;
           sum = input[_ind.size()-i+1] + 1;
           res = sum;
        ''', 'my_kernel3')

x = cp.arange(10, dtype=np.float32)
y = cp.arange(5, dtype=np.float32)
y2 = cp.arange(10, dtype=np.float32)

add_reverse = cp.ElementwiseKernel(
     'raw T x, raw TT y', 'T z',
     '''
     z = y[0];
     ''', 'add_reverse')

while(1):
    #kernel(numX, numY)
    x = kernel2(numX, numY)

        