# -*- coding: utf-8 -*-
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
from numba import cuda
from numba import jit

#---計算時間---
#startTime = time.time()

#---GPU訊息---
#dev = drv.Device(0)
#print(dev.name())
#dev.MAX_THREADS_PER_BLOCK
#dev.MAX_BLOCK_DIM_X
#dev.MAX_BLOCK_DIM_X
#dev.MAX_BLOCK_DIM_Z
#dev.TOTAL_CONSTANT_MEMORY
#dev.MAX_PITCH
#dev.CLOCK_RATE
#
##---範例1---
#test = SourceModule("""
#__global__ void add(int *c){
#    int idx = threadIdx.x * threadIdx.y;
#    *c = idx;
#}
#""")
#
#c = np.array([0])
#c = c.astype(np.int32)
#testMod = test.get_function('add')
#testMod(drv.Out(c), block=(3,3,1), grid=(1,1))



#@vectorize(['int64(int64, int64)'], target='cuda')
#def Add(a, b):
#    return a + b
#
## Initialize arrays
#while 1:
#    N = 100000
#    A = np.ones(N, dtype=np.int64)
#    B = np.ones(A.shape, dtype=A.dtype)
#    C = np.empty_like(A, dtype=A.dtype)
#    # Add arrays on GPU
#    C = Add(A, B)

#a = np.arange(5)
#@guvectorize([(int64[:], int64, int64[:])], '(n),()->(n)')
#def g(x, y, res):
#    for i in range(x.shape[0]):
#        res[i] = x[i] + y
#while 1:
#    a = np.arange(5)
#    g(a, 2)

x = np.arange(100).reshape(10, 10)

@jit(nopython=True) # Set "nopython" mode for best performance, equivalent to @njit
def go_fast(a): # Function is compiled to machine code when called the first time
    trace = 0
    for i in range(a.shape[0]):   # Numba likes loops
        trace += np.tanh(a[i, i]) # Numba likes NumPy functions
    return a + trace              # Numba likes NumPy broadcasting
while 1:
    x = np.arange(100).reshape(10, 10)
    print(go_fast(x))

#while 1:
#    prng = curand.PRNG(rndtype=curand.PRNG.XORWOW)
#    rand = np.empty(100000)
#    prng.uniform(rand)
#    print(rand[:10])

