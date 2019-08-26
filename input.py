#!/usr/bin/env python
# -*- coding: utf-8 -*-
# @Date    : 2019-08-24 18:47:17
# @Author  : Huiwen Deng 
import pandas as pd 
import numpy as np 
import sys 

# 输入一个数/字符串
s=input()
print(s)

# 一行输入多个，空格间隔
arr=list(map(str,sys.stdin.readline().strip().split()))
# 或者
s=input()
s=[i for i in s.split()]
print(arr)
print(s)

# 两行输入：第一行输入数组长度，第二行输入数组
s=input()
while s!="":
	length=int(s)
	arr=[int(i) for i in input().split()]
	print(length,arr)
	break

# 输入多行
res=[]
s= sys.stdin.readline().strip("\n")
while s != "":     
	res.append(s)     
	s= sys.stdin.readline().strip("\n")      
print (res) 

# input多行输入
n=int(input()) # 输入行数
res=[]
for  i in range(0,n):
	read_line=list(map(int,input().split())) # 注意转换数据类型
	res.append(read_line)
print(res)

# 输出
sys.stdout.write(res)