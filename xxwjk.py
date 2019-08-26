#!/usr/bin/env python
# -*- coding: utf-8 -*-
# @Date    : 2019-08-20 13:15:02
# @Author  : Huiwen Deng 
import numpy as np 
import pandas as pd 
from sklearn.linear_model import LogisticRegression
df=pd.read_csv('C:/Users/yirav/Desktop/data1.csv')
# 替换缺失值
df=df.fillna({'x1':-999,'x2':-999})


tmp=df.groupby('cust_id')['max_ovd_days'].max()
df.drop('max_ovd_days',axis=1,inplace=True)
tmp.columns={'cust_id','max_ovd_days'}
df=df.merge(tmp,on='cust_id',how='left')
def label(x):
	if x>30:
		return 1
	elif x==0:
		return 0

df['label']=df['max_ovd_days'].map(lambda x:label(x))
df.dropna(inplace=True)
df.drop_duplicates('cust_id',keep='first',inplace=True)

y = df['label'] 
model = LogisticRegression()
X=[x1,x2]
model.fit(X, y)
