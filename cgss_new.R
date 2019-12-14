setwd("C:\\Users\\yirav\\Documents\\jupyter notebook\\data result")

library(tidyverse)
library(readstata13)
library(reshape2)
library(mlogit)
library(nnet)# dummy variable
library(AER) # 
library(quantreg) # ��λ���ع�
library(psych)# ������ͳ��

# CGSS
data15=read.dta13('cgss2015.dta')
data13=read.dta13('cgss2013.dta')
data12=read.dta13('cgss2012.dta')
data10=read.dta13('cgss2010.dta')
cpi=read.csv('cpi_2009.csv')
p_city_pro=read.csv('p_city_pro.csv')
exp_edu_pro=read.csv('expend_edu_pro.csv')
exp_farm_pro=read.csv('expend_farm_pro.csv')
third_pro=read.csv('third_pro.csv')
gdp=read.csv('��ʡ�˾�GDP.csv')
egdp=read.csv('exp_GDP_pro.csv')
# ��λ��Ϊ���кͷǹ���
# ְҵ���ݹ����Ϊ4�࣬��������
# ������ò��Ϊ��Ա�ͷǵ�Ա
# ����1��ũ��0(ԭ����2)
data15=data15%>%rename(gender=a2,birthy_c=a301,birthy_f=a89a,edu_c=a7a,edu_f=a89b,income=a8a,
                       job_f=fisco88)%>%mutate(urban=ifelse(s1==2,0,s1),party_c=ifelse(a10==4,1,0),
                                               party_f=ifelse(a89c==4,1,0),year=2015,
                                               job_c=ifelse(is.na(a59disco88),a60disco88,a59disco88),
                                               isei_c=ifelse(is.na(isei_c1),isei_c2,isei_c1),
                                               com_c=ifelse((a59j%in%c(1,6)|((a59j==2)&(a59k==1)))|((a59j==3)&(a59k==1))|((a59j==4)&(a59k==1)),1,
                                                            ifelse(((a59j%in%c(2,3,4))&(a59k!=1))|(a59j==5),0,NA)),
                                               com_f=ifelse((a89g%in%c(1,6)|((a89g==2)&(a89h==1)))|((a89g==3)&(a89h==1))|((a89g==4)&(a89h==1)),1,
                                                            ifelse(((a89g%in%c(2,3,4))&(a89h!=1))|(a89g==5),0,NA)))
data10=data10%>%rename(gender=a2,birthy_c=a3a,birthy_f=a89a,edu_c=a7a,edu_f=a89b,income=a8a,
                       job_f=fisco88)%>%mutate(urban=ifelse(s5==2,0,s5),party_c=ifelse(a10==4,1,0),
                                               party_f=ifelse(a89c==4,1,0),year=2010,
                                               job_c=ifelse(is.na(risco881),risco882,risco881),
                                               isei_c=ifelse(is.na(isei_c1),isei_c2,isei_c1),
                                               com_c=ifelse((a59j%in%c(1,6)|((a59j==2)&(a59k==1)))|((a59j==3)&(a59k==1))|((a59j==4)&(a59k==1)),1,
                                                            ifelse(((a59j%in%c(2,3,4))&(a59k!=1))|(a59j==5),0,NA)),
                                               com_f=ifelse((a89g%in%c(1,6)|((a89g==2)&(a89h==1)))|((a89g==3)&(a89h==1))|((a89g==4)&(a89h==1)),1,
                                                            ifelse(((a89g%in%c(2,3,4))&(a89h!=1))|(a89g==5),0,NA)))
# ��ί��1-0����ί��2-1
data13=data13%>%rename(gender=a2,birthy_c=a3a,birthy_f=a89a,edu_c=a7a,edu_f=a89b,income=a8a,
                       job_f=iscodad)%>%mutate(urban=ifelse(vilorngh==1,0,1),party_c=ifelse(a10==4,1,0),
                                              party_f=ifelse(a89c==4,1,0),year=2013,
                                              job_c=ifelse(is.na(iscorp1),iscorp2,iscorp1),
                                              isei_c=ifelse(is.na(isei_c1),isei_c2,isei_c1),
                                              com_c=ifelse((a59j%in%c(1,6)|((a59j==2)&(a59k==1)))|((a59j==3)&(a59k==1))|((a59j==4)&(a59k==1)),1,
                                                           ifelse(((a59j%in%c(2,3,4))&(a59k!=1))|(a59j==5),0,NA)),
                                              com_f=ifelse((a89g%in%c(1,6)|((a89g==2)&(a89h==1)))|((a89g==3)&(a89h==1))|((a89g==4)&(a89h==1)),1,
                                                           ifelse(((a89g%in%c(2,3,4))&(a89h!=1))|(a89g==5),0,NA)))
data12=data12%>%rename(gender=a2,birthy_c=a3a,birthy_f=a89a,edu_c=a7a,edu_f=a89b,income=a8a,
                       job_f=iscodad)%>%mutate(urban=ifelse(vilorngh==1,0,1),party_c=ifelse(a10==4,1,0),
                                               party_f=ifelse(a89c==4,1,0),year=2012,
                                               job_c=ifelse(is.na(iscorp1),iscorp2,iscorp1),
                                               isei_c=ifelse(is.na(isei_c1),isei_c2,isei_c1),
                                               com_c=ifelse((a59j%in%c(1,6)|((a59j==2)&(a59k==1)))|((a59j==3)&(a59k==1))|((a59j==4)&(a59k==1)),1,
                                                            ifelse(((a59j%in%c(2,3,4))&(a59k!=1))|(a59j==5),0,NA)),
                                               com_f=ifelse((a89g%in%c(1,6)|((a89g==2)&(a89h==1)))|((a89g==3)&(a89h==1))|((a89g==4)&(a89h==1)),1,
                                                            ifelse(((a89g%in%c(2,3,4))&(a89h!=1))|(a89g==5),0,NA)))

res15=data15[c('s41','urban','gender','birthy_c','birthy_f','edu_c','edu_f','com_c','com_f','job_c','job_f','isei_c','isei_f','party_c','party_f','income','year')]
res13=data13[c('s41','urban','gender','birthy_c','birthy_f','edu_c','edu_f','com_c','com_f','job_c','job_f','isei_c','isei_f','party_c','party_f','income','year')]
res12=data12[c('s41','urban','gender','birthy_c','birthy_f','edu_c','edu_f','com_c','com_f','job_c','job_f','isei_c','isei_f','party_c','party_f','income','year')]
res10=data10[c('s41','urban','gender','birthy_c','birthy_f','edu_c','edu_f','com_c','com_f','job_c','job_f','isei_c','isei_f','party_c','party_f','income','year')]

total=rbind(res15,res13,res12,res10)
total$pro[total$s41==1]<-31
total$pro[total$s41==2]<-53
total$pro[total$s41==3]<-15
total$pro[total$s41==4]<-11
total$pro[total$s41==5]<-22
total$pro[total$s41==6]<-51
total$pro[total$s41==7]<-12
total$pro[total$s41==8]<-64
total$pro[total$s41==9]<-34
total$pro[total$s41==10]<-37
total$pro[total$s41==11]<-14
total$pro[total$s41==12]<-44
total$pro[total$s41==13]<-45
total$pro[total$s41==14]<-65
total$pro[total$s41==15]<-32
total$pro[total$s41==16]<-36
total$pro[total$s41==17]<-13
total$pro[total$s41==18]<-41
total$pro[total$s41==19]<-33
total$pro[total$s41==20]<-46
total$pro[total$s41==21]<-42
total$pro[total$s41==22]<-43
total$pro[total$s41==23]<-62
total$pro[total$s41==24]<-35
total$pro[total$s41==25]<-54
total$pro[total$s41==26]<-52
total$pro[total$s41==27]<-21
total$pro[total$s41==28]<-50
total$pro[total$s41==29]<-61
total$pro[total$s41==30]<-63
total$pro[total$s41==31]<-23
total<-total%>%mutate(eduy_c=ifelse(edu_c%in%c(1,2),0,ifelse(edu_c==3,6,ifelse(edu_c==4,9,
                      ifelse(edu_c%in%c(5,6,7,8),12,ifelse(edu_c%in%c(9,10),15,ifelse(edu_c%in%c(11,12),16,ifelse(edu_c==13,19,NA))))))),
                     eduy_f=ifelse(edu_f%in%c(1,2),0,ifelse(edu_f==3,6,ifelse(edu_f==4,9,
                     ifelse(edu_f%in%c(5,6,7,8),12,ifelse(edu_f%in%c(9,10),15,ifelse(edu_f%in%c(11,12),16,ifelse(edu_f==13,19,NA))))))))
total$dis[total$pro%in%c(11,12,13,21,31,32,33,35,37,44,46)]<-0
total$dis[total$pro%in%c(14,22,23,34,36,41,42,43)]<-1
total$dis[total$pro%in%c(15,45,50,51,52,53,54,61,62,63,64,65)]<-2
total$gender=ifelse(total$gender==2,0,1)
# ����CPI
total<-merge(total,cpi,by.x='pro',by.y = 'No',all.x = TRUE)
# ���Ӻ�۱���
total<-merge(total,p_city_pro,by = 'pro',all.x = TRUE)
total<-merge(total,exp_edu_pro,by = 'pro',all.x = TRUE)
total<-merge(total,exp_farm_pro,by = 'pro',all.x = TRUE)
total<-merge(total,third_pro,by = 'pro',all.x = TRUE)
total<-merge(total,gdp,by = 'pro',all.x = TRUE)
total<-merge(total,egdp,by = 'pro',all.x = TRUE)
# �����۸����Ӻ�۱���
res15_1<-total%>%filter(year==2015)%>%mutate(inc_cpi=income/X2015,p=p2015,e=e2015,f=f2015,t=t2015,pgdp=pgdp2015,eg=eg2015)
res13_1<-total%>%filter(year==2013)%>%mutate(inc_cpi=income/X2013,p=p2013,e=e2013,f=f2013,t=t2013,pgdp=pgdp2015,eg=eg2013)
res12_1<-total%>%filter(year==2012)%>%mutate(inc_cpi=income/X2012,p=p2012,e=e2012,f=f2012,t=t2012,pgdp=pgdp2015,eg=eg2012)
res10_1<-total%>%filter(year==2010)%>%mutate(inc_cpi=income/X2010,p=p2010,e=e2010,f=f2010,t=t2010,pgdp=pgdp2015,eg=eg2010)
# ɸѡ�����Ӵ�20-45������35-65���������0
res15_1<-res15_1%>%mutate(age_c=year-birthy_c,age_c2=age_c^2,age_f=year-birthy_f,age_f2=age_f^2)%>%filter((between(age_c,20,45))&(between(age_f,35,65))&(income>0))%>%mutate(linc=log(inc_cpi))
res13_1<-res13_1%>%mutate(age_c=year-birthy_c,age_c2=age_c^2,age_f=year-birthy_f,age_f2=age_f^2)%>%filter((between(age_c,20,45))&(between(age_f,35,65))&(income>0))%>%mutate(linc=log(inc_cpi))
res12_1<-res12_1%>%mutate(age_c=year-birthy_c,age_c2=age_c^2,age_f=year-birthy_f,age_f2=age_f^2)%>%filter((between(age_c,20,45))&(between(age_f,35,65))&(income>0))%>%mutate(linc=log(inc_cpi))
res10_1<-res10_1%>%mutate(age_c=year-birthy_c,age_c2=age_c^2,age_f=year-birthy_f,age_f2=age_f^2)%>%filter((between(age_c,20,45))&(between(age_f,35,65))&(income>0))%>%mutate(linc=log(inc_cpi))
# �������0���޳�ǰ��1%
res15_11<-res15_1%>%filter(urban==1,(linc > quantile(linc, 0.01)) & (linc < quantile(linc, 0.99)))
res15_10<-res15_1%>%filter(urban==0,(linc > quantile(linc, 0.01)) & (linc < quantile(linc, 0.99)))
res13_11<-res13_1%>%filter(urban==1,(linc > quantile(linc, 0.01)) & (linc < quantile(linc, 0.99)))
res13_10<-res13_1%>%filter(urban==0,(linc > quantile(linc, 0.01)) & (linc < quantile(linc, 0.99)))
res12_11<-res12_1%>%filter(urban==1,(linc > quantile(linc, 0.01)) & (linc < quantile(linc, 0.99)))
res12_10<-res12_1%>%filter(urban==0,(linc > quantile(linc, 0.01)) & (linc < quantile(linc, 0.99)))
res10_11<-res10_1%>%filter(urban==1,(linc > quantile(linc, 0.01)) & (linc < quantile(linc, 0.99)))
res10_10<-res10_1%>%filter(urban==0,(linc > quantile(linc, 0.01)) & (linc < quantile(linc, 0.99)))
total1<-rbind(res10_11,res10_10,res12_11,res12_10,res13_11,res13_10,res15_11,res15_10)%>%mutate(cohort=ifelse(between(birthy_f,1941,1950),0,
                                                                                                              ifelse(between(birthy_f,1951,1960),1,
                                                                                                                     ifelse(between(birthy_f,1961,1970),2,
                                                                                                                            ifelse(between(birthy_f,1971,1980),3,
                                                                                                                                   ifelse(between(birthy_f,1981,1990),4,ifelse(between(birthy_f,1991,2000),5,6)))))))
# ���򡢵�����ݡ��������
dum_dis<-class.ind(total1$dis)
colnames(dum_dis)<-c('d0','d1','d2')
dum_y<-class.ind(total1$year)
colnames(dum_y)<-c('y0','y2','y3','y5')
dum_c<-class.ind(total1$cohort)
colnames(dum_c)<-c('c0','c1','c2','c3')
total2<-cbind(total1,dum_dis,dum_y,dum_c)
total3<-total2[c('urban','gender','age_c','age_c2','age_f','age_f2',
                 'eduy_c','eduy_f','isei_c','isei_f','party_c','income','linc',
                 'd1','d2','y2','y3','y5','c1','c2','c3','p','e','f','t','pgdp','eg','year')]
total3<-na.omit(total3)

# ����������CFPS
raw_data16=read.csv('model2016.csv')
raw_data14=read.csv('model2014.csv')
raw_data12=read.csv('model2012.csv')
raw_data10=read.csv('model2010.csv')
result=rbind(raw_data16,raw_data14,raw_data12,raw_data10)
result<-result%>%mutate(age2=age^2,lincome=log(income))
result$dis[result$pro%in%c(11,12,13,21,31,32,33,35,37,44,46)]<-0
result$dis[result$pro%in%c(14,22,23,34,36,41,42,43)]<-1
result$dis[result$pro%in%c(15,45,50,51,52,53,54,61,62,63,64,65)]<-2
raw10_11<-result%>%filter(year==2010)%>%filter(urban==1,(lincome > quantile(lincome, 0.01)) & (lincome < quantile(lincome, 0.99)))
raw10_10<-result%>%filter(year==2010)%>%filter(urban==0,(lincome > quantile(lincome, 0.01)) & (lincome < quantile(lincome, 0.99)))
raw12_11<-result%>%filter(year==2012)%>%filter(urban==1,(lincome > quantile(lincome, 0.01)) & (lincome < quantile(lincome, 0.99)))
raw12_10<-result%>%filter(year==2012)%>%filter(urban==0,(lincome > quantile(lincome, 0.01)) & (lincome < quantile(lincome, 0.99)))
raw14_11<-result%>%filter(year==2014)%>%filter(urban==1,(lincome > quantile(lincome, 0.01)) & (lincome < quantile(lincome, 0.99)))
raw14_10<-result%>%filter(year==2014)%>%filter(urban==0,(lincome > quantile(lincome, 0.01)) & (lincome < quantile(lincome, 0.99)))
raw16_11<-result%>%filter(year==2016)%>%filter(urban==1,(lincome > quantile(lincome, 0.01)) & (lincome < quantile(lincome, 0.99)))
raw16_10<-result%>%filter(year==2016)%>%filter(urban==0,(lincome > quantile(lincome, 0.01)) & (lincome < quantile(lincome, 0.99)))
result<-rbind(raw10_10,raw10_11,raw12_10,raw12_11,
              raw14_10,raw14_11,raw16_10,raw16_11)%>%mutate(birthy=year-age,cohort=ifelse(between(birthy,1941,1950),0,
                                                                           ifelse(between(birthy,1951,1960),1,
                                                                                  ifelse(between(birthy,1961,1970),2,
                                                                                         ifelse(between(birthy,1971,1980),3,
                                                                                                ifelse(between(birthy,1981,1990),4,ifelse(between(birthy,1991,2000),5,6)))))))
dum_disf<-class.ind(result$dis)
colnames(dum_disf)<-c('d0','d1','d2')
dum_cohort<-class.ind(result$cohort)
colnames(dum_cohort)<-c('c0','c1','c2','c3')
result<-cbind(result,dum_disf,dum_cohort)

# TS2SLS��һ�׶� ���Ƹ�������
m1<-lm(lincome~age+age2+d1+d2+c1+c2+c3+job+eduy,data=result)
summary(m1)

linc_f<-predict(m1,newdata = data.frame(d1=total3$d1,d2=total3$d2,eduy=total3$eduy_f,
                                        age=total3$age_f,age2=total3$age_f2,job=total3$isei_f,
                                        c1=total3$c1,c2=total3$c2,c3=total3$c3))
mdata<-cbind(total3,linc_f)

lm1=lm(linc~linc_f+age_c+age_c2+age_f+age_f2+gender,data=mdata)
summary(lm1)
# 2010��
#ȫ����
lm10=lm(linc~linc_f+age_c+age_c2+age_f+age_f2+gender,data=mdata%>%filter(year==2010))
summary(lm10)
#����
lm10_1=lm(linc~linc_f+age_c+age_c2+age_f+age_f2+gender,data=mdata%>%filter(year==2010,urban==1))
summary(lm10_1)
#ũ��
lm10_0=lm(linc~linc_f+age_c+age_c2+age_f+age_f2+gender,data=mdata%>%filter(year==2010,urban==0))
summary(lm10_0)
# 2012��
#ȫ����
lm12=lm(linc~linc_f+age_c+age_c2+age_f+age_f2+gender,data=mdata%>%filter(year==2012))
summary(lm12)
#����
lm12_1=lm(linc~linc_f+age_c+age_c2+age_f+age_f2+gender,data=mdata%>%filter(year==2012,urban==1))
summary(lm12_1)
#ũ��
lm12_0=lm(linc~linc_f+age_c+age_c2+age_f+age_f2+gender,data=mdata%>%filter(year==2012,urban==0))
summary(lm12_0)
# 2013��
#ȫ����
lm13=lm(linc~linc_f+age_c+age_c2+age_f+age_f2+gender,data=mdata%>%filter(year==2013))
summary(lm13)
#����
lm13_1=lm(linc~linc_f+age_c+age_c2+age_f+age_f2+gender,data=mdata%>%filter(year==2013,urban==1))
summary(lm13_1)
#ũ��
lm13_0=lm(linc~linc_f+age_c+age_c2+age_f+age_f2+gender,data=mdata%>%filter(year==2013,urban==0))
summary(lm13_0)
# 2015��
#ȫ����
lm15=lm(linc~linc_f+age_c+age_c2+age_f+age_f2+gender,data=mdata%>%filter(year==2015))
summary(lm15)
#����
lm15_1=lm(linc~linc_f+age_c+age_c2+age_f+age_f2+gender,data=mdata%>%filter(year==2015,urban==1))
summary(lm15_1)
#ũ��
lm15_0=lm(linc~linc_f+age_c+age_c2+age_f+age_f2+gender,data=mdata%>%filter(year==2015,urban==0))
summary(lm15_0)

# ��λ���ع�
# 2010
fit10 = rq(linc~linc_f+age_c+age_c2+age_f+age_f2, tau= c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), data = mdata%>%filter(year==2010))         
summary(fit10, se = "boot")
# 2012
fit12 = rq(linc~linc_f+age_c+age_c2+age_f+age_f2, tau= c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), data = mdata%>%filter(year==2012))         
summary(fit12, se = "boot")
# 2013
fit13 = rq(linc~linc_f+age_c+age_c2+age_f+age_f2, tau= c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), data = mdata%>%filter(year==2013))         
summary(fit13, se = "boot")
# 2015
fit15 = rq(linc~linc_f+age_c+age_c2+age_f+age_f2, tau= c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), data = mdata%>%filter(year==2015))         
summary(fit15, se = "boot")
# ȫ����
fit1 = rq(linc~linc_f+age_c+age_c2+age_f+age_f2, tau= c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), data = mdata)         
summary(fit1, se = "boot")

# �������أ���Ͻ������ݿ�����ݣ�2010��Ϊ��׼��
# ����Ƿ���Ҫ���ӵ����������������������������Ҫ�������н�ЧӦ�ļ�����
cl1<-lm(linc~linc_f*p+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
# cl1<-lm(linc~linc_f*p+age_c+age_c2+age_f+age_f2,data=mdata)
summary(cl1)
cl2<-lm(linc~linc_f*e+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(cl2)
cl3<-lm(linc~linc_f*f+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(cl3)
cl4<-lm(linc~linc_f*t+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(cl4)
cl5<-lm(linc~linc_f*log(pgdp)+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(cl5)
cl6<-lm(linc~linc_f*eg+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(cl6)

# ��λ���ع飬��Ϊ������ݻᱨ���������죬���Բ��������
# ��ʱֻ���������ļ���
r1<-rq(linc~linc_f*p+age_c+age_c2+age_f+age_f2+factor(gender)+factor(urban)+y2+y3+y5,tau= c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),data=mdata)
summary(r1, se = "boot")
r2<-rq(linc~linc_f*log(pgdp)+age_c+age_c2+age_f+age_f2+factor(gender)+factor(urban)+y2+y3+y5,tau= c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),data=mdata)
summary(r2, se = "boot")
r3<-rq(linc~linc_f*t+age_c+age_c2+age_f+age_f2+factor(gender)+factor(urban)+y2+y3+y5,tau= c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),data=mdata)
summary(r3, se = "boot")

# ���н�ĵ���ģ��
# ע���Ա�������������н���������ڱ�����Ҫ���Ļ�
mdata<-mdata%>%mutate(slinc=scale(linc),slinc_f=scale(linc_f),seduy_c=scale(eduy_c),seduy_f=scale(eduy_f),
                      sisei_c=scale(isei_c),sisei_f=scale(isei_f),sp=scale(p),st=scale(t),spgdp=scale(log(pgdp)))
# ������
# �Ӵ��ܽ������
k1=lm(seduy_c~slinc_f+sp*slinc_f+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k1)
k2=lm(slinc~slinc_f+sp*slinc_f+seduy_c*sp+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k2)
# �Ӵ�ְҵ
k1=lm(sisei_c~slinc_f+sp*slinc_f+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k1)
k2=lm(slinc~slinc_f+sp*slinc_f+sisei_c*sp+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k2)
# �����ܽ���
k1=lm(seduy_f~slinc_f+sp*slinc_f+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k1)
k2=lm(slinc~slinc_f+sp*slinc_f+seduy_f*sp+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k2)
# ����ְҵ
k1=lm(sisei_f~slinc_f+sp*slinc_f+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k1)
k2=lm(slinc~slinc_f+sp*slinc_f+sisei_f*sp+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k2)

# ��ҵ�ṹ
# �Ӵ��ܽ������
k1=lm(seduy_c~slinc_f+st*slinc_f+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k1)
k2=lm(slinc~slinc_f+st*slinc_f+seduy_c*st+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k2)
# �Ӵ�ְҵ
k1=lm(sisei_c~slinc_f+st*slinc_f+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k1)
k2=lm(slinc~slinc_f+st*slinc_f+sisei_c*st+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k2)
# �����ܽ���
k1=lm(seduy_f~slinc_f+st*slinc_f+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k1)
k2=lm(slinc~slinc_f+st*slinc_f+seduy_f*st+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k2)
# ����ְҵ
k1=lm(sisei_f~slinc_f+st*slinc_f+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k1)
k2=lm(slinc~slinc_f+st*slinc_f+sisei_f*st+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k2)

# �˾�GDP
# �Ӵ��ܽ���
k1=lm(seduy_c~slinc_f+spgdp*slinc_f+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k1)
k2=lm(slinc~slinc_f+spgdp*slinc_f+seduy_c*spgdp+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k2)
# �Ӵ�ְҵ
k1=lm(sisei_c~slinc_f+spgdp*slinc_f+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k1)
k2=lm(slinc~slinc_f+spgdp*slinc_f+sisei_c*spgdp+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k2)
# �����ܽ���
k1=lm(seduy_f~slinc_f+spgdp*slinc_f+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k1)
k2=lm(slinc~slinc_f+spgdp*slinc_f+seduy_f*spgdp+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k2)
# ����ְҵ
k1=lm(sisei_f~slinc_f+spgdp*slinc_f+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k1)
k2=lm(slinc~slinc_f+spgdp*slinc_f+sisei_f*spgdp+age_c+age_c2+age_f+age_f2+gender+urban+y2+y3+y5,data=mdata)
summary(k2)

# ����ʱ����ɷ�
x<-scale(total3[c('party_c','isei_c')])
pc<-princomp(x, cor = T)
summary(pc, loadings = T)
pc_data<-predict(pc)
# ��ʯͼ
fa.parallel(x,fa='pc',n.iter = 100,show.legend = F,main = 'Scree plot with parallel analysis')