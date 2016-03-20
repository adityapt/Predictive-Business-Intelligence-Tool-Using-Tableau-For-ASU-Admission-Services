#_________________________________________________________________________________________________
# Change the working directory to ur dataset directory and run the entire code. QA has to be done
#_________________________________________________________________________________________________
setwd("~/capstone")
mod = read.csv("modifiedcsv392016.csv")
str(mod)
summary(mod)
provinceLevels = as.data.frame(sort(levels(mod$Fnoprovince)))
write.csv(provinceLevels, "provinceLevels.csv")
library(sqldf)
#_________________________________________________________________________________________________
# creating a new dataset by grouping provinces
# https://en.wikipedia.org/wiki/List_of_cities_in_China
# Dataset from cities in china extracted from this website
#_________________________________________________________________________________________________

new = sqldf("select a.*, 
      case				
      when Fnoprovince	=	''	then	'Not Provided'
      when Fnoprovince	=	'-'	then	'No Province'
      when Fnoprovince	=	'-----'	then	'No Province'
      when Fnoprovince	=	'1215 e vista del apt2034 tempe'	then	'arizona'
      when Fnoprovince	=	'18 mondamin ct,  somervill ma'	then	'massachusets'
      when Fnoprovince	=	'200231'	then	'No Province'
      when Fnoprovince	=	'61'	then	'No Province'
      when Fnoprovince	=	'汿西'	then	'No Province'
      when Fnoprovince	=	'湿忿翁'	then	'No Province'
      when Fnoprovince	=	'airai'	then	'palau'
      when Fnoprovince	=	'alberta'	then	'canada'
      when Fnoprovince	=	'an hui'	then	'anhui'
      when Fnoprovince	=	'anhui'	then	'anhui'
      when Fnoprovince	=	'anshan'	then	'liaoning'
      when Fnoprovince	=	'anyang'	then	'henan'
      when Fnoprovince	=	'asia'	then	'asia'
      when Fnoprovince	=	'auburn'	then	'alabama'
      when Fnoprovince	=	'az'	then	'arizona'
      when Fnoprovince	=	'b.c.'	then	'british columbia'
      when Fnoprovince	=	'baiyin'	then	'baiyin'
      when Fnoprovince	=	'bangkok'	then	'bangkok'
      when Fnoprovince	=	'baoding'	then	'baoding'
      when Fnoprovince	=	'baoji'	then	'baoji'
      when Fnoprovince	=	'baoshan'	then	'baoshan'
      when Fnoprovince	=	'baotou'	then	'baotou'
      when Fnoprovince	=	'barcelona'	then	'barcelona'
      when Fnoprovince	=	'bazhong'	then	'bazhong'
      when Fnoprovince	=	'be'	then	'be'
      when Fnoprovince	=	'beiijng'	then	'beijing'
      when Fnoprovince	=	'beiing'	then	'beijing'
      when Fnoprovince	=	'beijiao shunde'	then	'guangdong'
      when Fnoprovince	=	'beijing'	then	'beijing'
      when Fnoprovince	=	'beijing,'	then	'beijing'
      when Fnoprovince	=	'beijinng'	then	'beijing'
      when Fnoprovince	=	'beijjing'	then	'beijing'
      when Fnoprovince	=	'beijng'	then	'beijing'
      when Fnoprovince	=	'beilun ningbo'	then	'zhejiang'
      when Fnoprovince	=	'bejing'	then	'beijing'
      when Fnoprovince	=	'bengbu'	then	'anhui'
      when Fnoprovince	=	'binzhou'	then	'shandong'
      when Fnoprovince	=	'bj'	then	'beijing'
      when Fnoprovince	=	'bozhou'	then	'anhui'
      when Fnoprovince	=	'bozou'	then	'anhui'
      when Fnoprovince	=	'british columbia'	then	'british columbia'
      when Fnoprovince	=	'budapest'	then	'budapest'
      when Fnoprovince	=	'burnaby'	then	'british columbia'
      when Fnoprovince	=	'ca'	then	'california'
      when Fnoprovince	=	'california'	then	'california'
      when Fnoprovince	=	'capital federal'	then	'capital federal'
      when Fnoprovince	=	'changchun'	then	'jilin'
      when Fnoprovince	=	'changde'	then	'hunan'
      when Fnoprovince	=	'changping'	then	'guangdong'
      when Fnoprovince	=	'changsha'	then	'hunan'
      when Fnoprovince	=	'changshu'	then	'jiangsu'
      when Fnoprovince	=	'changzhi'	then	'shanxi'
      when Fnoprovince	=	'changzhou'	then	'jiangsu'
      when Fnoprovince	=	'changzhou jiangsu'	then	'jiangsu'
      when Fnoprovince	=	'changzhou, jiangsu'	then	'jiangsu'
      when Fnoprovince	=	'chaoyang'	then	'beijing'
      when Fnoprovince	=	'chaozhou'	then	'guangdong'
      when Fnoprovince	=	'chengde'	then	'hebei'
      when Fnoprovince	=	'chengdu'	then	'sichuan'
      when Fnoprovince	=	'china'	then	'china'
      when Fnoprovince	=	'china jiangsu nanjing'	then	'jiangsu'
      when Fnoprovince	=	'china/beijing'	then	'beijing'
      when Fnoprovince	=	'chizhou'	then	'anhui'
      when Fnoprovince	=	'chong qing'	then	'municipal'
      when Fnoprovince	=	'chongging'	then	'municipal'
      when Fnoprovince	=	'chongqing'	then	'municipal'
      when Fnoprovince	=	'chuzhou'	then	'anhui'
      when Fnoprovince	=	'cixi'	then	'zhejiang'
      when Fnoprovince	=	'dalian'	then	'liaoning'
      when Fnoprovince	=	'dandong'	then	'liaoning'
      when Fnoprovince	=	'daqing'	then	'heilongjiang'
      when Fnoprovince	=	'datong'	then	'shanxi'
      when Fnoprovince	=	'daxing'	then	'beijing'
      when Fnoprovince	=	'de zhou'	then	'shandong'
      when Fnoprovince	=	'dengzhou'	then	'henan'
      when Fnoprovince	=	'dongguan'	then	'guangdong'
      when Fnoprovince	=	'dongtai'	then	'jiangsu'
      when Fnoprovince	=	'dongyang'	then	'zhejiang'
      when Fnoprovince	=	'dongying'	then	'shandong'
      when Fnoprovince	=	'dubai'	then	'dubai'
      when Fnoprovince	=	'dujiangyan'	then	'sichuan'
      when Fnoprovince	=	'迿宁翁'	then	'No Province'
      when Fnoprovince	=	'erd'	then	'budapest'
      when Fnoprovince	=	'essex'	then	'massachusets'
      when Fnoprovince	=	'fenghua'	then	'zhejiang'
      when Fnoprovince	=	'fengtai dist. beijing'	then	'beijing'
      when Fnoprovince	=	'fl'	then	'florida'
      when Fnoprovince	=	'florida'	then	'florida'
      when Fnoprovince	=	'foreign country'	then	'No Province'
      when Fnoprovince	=	'foshan'	then	'guangdong'
      when Fnoprovince	=	'foshanshi'	then	'guangdong'
      when Fnoprovince	=	'fredericton'	then	'canada'
      when Fnoprovince	=	'fttgvv'	then	'No Province'
      when Fnoprovince	=	'fu jian'	then	'sichuan'
      when Fnoprovince	=	'fujian'	then	'sichuan'
      when Fnoprovince	=	'fujian,'	then	'sichuan'
      when Fnoprovince	=	'fujiang'	then	'sichuan'
      when Fnoprovince	=	'fushun'	then	'liaoning'
      when Fnoprovince	=	'fuyang'	then	'anhui'
      when Fnoprovince	=	'fuzhou'	then	'fujian'
      when Fnoprovince	=	'ga'	then	'georgia'
      when Fnoprovince	=	'gan su'	then	'gansu'
      when Fnoprovince	=	'gansu'	then	'gansu'
      when Fnoprovince	=	'gansu provin'	then	'gansu'
      when Fnoprovince	=	'ganzhou'	then	'jiangxi'
      when Fnoprovince	=	'gaobeidian'	then	'hebei'
      when Fnoprovince	=	'gaocheng'	then	'hebei'
      when Fnoprovince	=	'gd'	then	'guangdong'
      when Fnoprovince	=	'ge jiu'	then	'yunnan'
      when Fnoprovince	=	'guandong'	then	'guangdong'
      when Fnoprovince	=	'guang dong'	then	'guangdong'
      when Fnoprovince	=	'guang xi'	then	'guangxi'
      when Fnoprovince	=	'guangdon'	then	'guangdong'
      when Fnoprovince	=	'guangdong'	then	'guangdong'
      when Fnoprovince	=	'guangdongsheng'	then	'guangdong'
      when Fnoprovince	=	'guanghan'	then	'sichuan'
      when Fnoprovince	=	'guangodong'	then	'guangdong'
      when Fnoprovince	=	'guangong'	then	'guangdong'
      when Fnoprovince	=	'guangxi'	then	'guangxi'
      when Fnoprovince	=	'guangxi zhuang autono'	then	'guangxi'
      when Fnoprovince	=	'guangyuan'	then	'sichuan'
      when Fnoprovince	=	'guangzhou'	then	'guangdong'
      when Fnoprovince	=	'guanxi'	then	'guangxi'
      when Fnoprovince	=	'guilin'	then	'guangxi'
      when Fnoprovince	=	'guiyang'	then	'guizhou'
      when Fnoprovince	=	'guizhou'	then	'guizhou'
      when Fnoprovince	=	'hai dian dist.'	then	'beijing'
      when Fnoprovince	=	'haidian'	then	'beijing'
      when Fnoprovince	=	'haidian beijing'	then	'beijing'
      when Fnoprovince	=	'haidianqu'	then	'beijing'
      when Fnoprovince	=	'haikou'	then	'hainan'
      when Fnoprovince	=	'haimen'	then	'jiangsu'
      when Fnoprovince	=	'hainan'	then	'hainan'
      when Fnoprovince	=	'haining'	then	'zhejiang'
      when Fnoprovince	=	'handan'	then	'hebei'
      when Fnoprovince	=	'hangzhou'	then	'zhejiang'
      when Fnoprovince	=	'harbin'	then	'heilongjiang'
      when Fnoprovince	=	'he bei'	then	'hebei'
      when Fnoprovince	=	'he nan'	then	'henan'
      when Fnoprovince	=	'hebei'	then	'hebei'
      when Fnoprovince	=	'hefe'	then	'anhui'
      when Fnoprovince	=	'hefei'	then	'anhui'
      when Fnoprovince	=	'hegei'	then	'anhui'
      when Fnoprovince	=	'heibei'	then	'hebei'
      when Fnoprovince	=	'heihongjiang'	then	'heilongjiang'
      when Fnoprovince	=	'heilingjiang'	then	'heilongjiang'
      when Fnoprovince	=	'heilongjiang'	then	'heilongjiang'
      when Fnoprovince	=	'heilongjiang,'	then	'heilongjiang'
      when Fnoprovince	=	'henan'	then	'henan'
      when Fnoprovince	=	'henana'	then	'henan'
      when Fnoprovince	=	'heshan'	then	'guangdong'
      when Fnoprovince	=	'hohhot'	then	'inner mongolia'
      when Fnoprovince	=	'hong kong'	then	'autonomous'
      when Fnoprovince	=	'hong kou'	then	'shanghai'
      when Fnoprovince	=	'hongkong'	then	'hongkong'
      when Fnoprovince	=	'houma'	then	'shanxi'
      when Fnoprovince	=	'hu bei'	then	'hubei'
      when Fnoprovince	=	'huaibei'	then	'anhui'
      when Fnoprovince	=	'huan rro'	then	'huan rro'
      when Fnoprovince	=	'hubei'	then	'hubei'
      when Fnoprovince	=	'huhhot'	then	'inner mongolia'
      when Fnoprovince	=	'huizhou'	then	'guangdong'
      when Fnoprovince	=	'hunan'	then	'hunan'
      when Fnoprovince	=	'hunan, pr.'	then	'hunan'
      when Fnoprovince	=	'huzhou'	then	'zhejiang'
      when Fnoprovince	=	'inner mongolia'	then	'inner mongolia'
      when Fnoprovince	=	'inner mongolia autonomous reg'	then	'inner mongolia'
      when Fnoprovince	=	'innermongolia'	then	'inner mongolia'
      when Fnoprovince	=	'jangxi'	then	'jangxi'
      when Fnoprovince	=	'ji lin'	then	'jilin'
      when Fnoprovince	=	'jiande'	then	'zhejiang'
      when Fnoprovince	=	'jiang  xi'	then	'jangxi'
      when Fnoprovince	=	'jiang su'	then	'jiang su'
      when Fnoprovince	=	'jiang xi'	then	'jiang su'
      when Fnoprovince	=	'jiangmen'	then	'guangdong'
      when Fnoprovince	=	'jiangshu'	then	'jiangsu'
      when Fnoprovince	=	'jiangsu'	then	'jiangsu'
      when Fnoprovince	=	'jiangxi'	then	'jiangxi'
      when Fnoprovince	=	'jiangyin'	then	'jiangsu'
      when Fnoprovince	=	'jiangyou'	then	'sichuan'
      when Fnoprovince	=	'jiaozuo'	then	'henan'
      when Fnoprovince	=	'jiaxing'	then	'zhejiang'
      when Fnoprovince	=	'jieyang'	then	'guangdong'
      when Fnoprovince	=	'jilin'	then	'jilin'
      when Fnoprovince	=	'jinan'	then	'shandong'
      when Fnoprovince	=	'jingjiang'	then	'jiangsu'
      when Fnoprovince	=	'jingmen'	then	'hubei'
      when Fnoprovince	=	'jingzhou'	then	'hubei'
      when Fnoprovince	=	'jinhua'	then	'zhejiang'
      when Fnoprovince	=	'jinjiang'	then	'fujian'
      when Fnoprovince	=	'jinshan'	then	'shanghai'
      when Fnoprovince	=	'jintan'	then	'jiangsu'
      when Fnoprovince	=	'jinzhong'	then	'shanxi'
      when Fnoprovince	=	'jinzhou'	then	'hebei'
      when Fnoprovince	=	'jiujiang'	then	'jiangxi'
      when Fnoprovince	=	'jixi'	then	'heilongjiang'
      when Fnoprovince	=	'jlin'	then	'jilin'
      when Fnoprovince	=	'kaifeng'	then	'henan'
      when Fnoprovince	=	'kampala'	then	'uganda'
      when Fnoprovince	=	'kanto'	then	'japan'
      when Fnoprovince	=	'karamay'	then	'xinjiang'
      when Fnoprovince	=	'koror'	then	'koror'
      when Fnoprovince	=	'kuitun'	then	'xinjiang'
      when Fnoprovince	=	'kunming'	then	'yunnan'
      when Fnoprovince	=	'kunshan'	then	'jiangsu'
      when Fnoprovince	=	'langfang'	then	'hebei'
      when Fnoprovince	=	'lanzhou'	then	'gansu'
      when Fnoprovince	=	'leshan'	then	'sichuan'
      when Fnoprovince	=	'lianyungang'	then	'jiangsu'
      when Fnoprovince	=	'liao ning'	then	'liaoning'
      when Fnoprovince	=	'liaoling'	then	'liaoning'
      when Fnoprovince	=	'liaoning'	then	'liaoning'
      when Fnoprovince	=	'linan'	then	'zhejiang'
      when Fnoprovince	=	'linyi'	then	'shandong'
      when Fnoprovince	=	'lisbon'	then	'portugal'
      when Fnoprovince	=	'lishui'	then	'zhejiang'
      when Fnoprovince	=	'liyang'	then	'jiangsu'
      when Fnoprovince	=	'ln'	then	'london'
      when Fnoprovince	=	'london'	then	'london'
      when Fnoprovince	=	'longhuaxinqu'	then	'guangdong'
      when Fnoprovince	=	'longkou'	then	'shandong'
      when Fnoprovince	=	'longyan'	then	'fujian'
      when Fnoprovince	=	'longyou'	then	'zhejiang'
      when Fnoprovince	=	'loudi'	then	'hunan'
      when Fnoprovince	=	'lu an'	then	'anhui'
      when Fnoprovince	=	'lufeng'	then	'guangdong'
      when Fnoprovince	=	'luoyang'	then	'henan'
      when Fnoprovince	=	'luwan'	then	'shanghai'
      when Fnoprovince	=	'luzhou'	then	'sichuan'
      when Fnoprovince	=	'macau'	then	'autonomous'
      when Fnoprovince	=	'madaba-manja'	then	'jordon'
      when Fnoprovince	=	'meishan'	then	'sichuan'
      when Fnoprovince	=	'mi'	then	'michigan'
      when Fnoprovince	=	'mianyang'	then	'sichuan'
      when Fnoprovince	=	'minhang'	then	'shanghai'
      when Fnoprovince	=	'minnesota'	then	'minnesota'
      when Fnoprovince	=	'miyagi'	then	'japan'
      when Fnoprovince	=	'mudanjiang'	then	'heilongjiang'
      when Fnoprovince	=	'n/a'	then	'No Province'
      when Fnoprovince	=	'nairobi'	then	'kenya'
      when Fnoprovince	=	'nanchang'	then	'jiangxi'
      when Fnoprovince	=	'nanchong'	then	'sichuan'
      when Fnoprovince	=	'nanjing'	then	'jiangsu'
      when Fnoprovince	=	'nanning'	then	'guangxi'
      when Fnoprovince	=	'nantong'	then	'jiangsu'
      when Fnoprovince	=	'nassau'	then	'bahamas'
      when Fnoprovince	=	'nebraska'	then	'nebraska'
      when Fnoprovince	=	'nei menggu zizhiqu'	then	'inner mongolia'
      when Fnoprovince	=	'nei mongol'	then	'inner mongolia'
      when Fnoprovince	=	'neimenggu'	then	'inner mongolia'
      when Fnoprovince	=	'neimonggol'	then	'inner mongolia'
      when Fnoprovince	=	'new jersey'	then	'new jersey'
      when Fnoprovince	=	'new mexico'	then	'new mexico'
      when Fnoprovince	=	'ningbo'	then	'zhejiang'
      when Fnoprovince	=	'ningde'	then	'fujian'
      when Fnoprovince	=	'ningixia'	then	'ningxia'
      when Fnoprovince	=	'ningxia'	then	'ningxia'
      when Fnoprovince	=	'ningxia hui autonomous region'	then	'ningxia'
      when Fnoprovince	=	'no'	then	'No Province'
      when Fnoprovince	=	'none'	then	'No Province'
      when Fnoprovince	=	'none of usa'	then	'No Province'
      when Fnoprovince	=	'north yorkshire'	then	'north yorkshire'
      when Fnoprovince	=	'not applicable'	then	'No Province'
      when Fnoprovince	=	'not applicaple'	then	'No Province'
      when Fnoprovince	=	'not listed or not applicable'	then	'No Province'
      when Fnoprovince	=	'nsw'	then	'new southwales'
      when Fnoprovince	=	'nv'	then	'nevada'
      when Fnoprovince	=	'on'	then	'ontario'
      when Fnoprovince	=	'ontario'	then	'ontario'
      when Fnoprovince	=	'outside us'	then	'No Province'
      when Fnoprovince	=	'pan jin'	then	'liaoning'
      when Fnoprovince	=	'panama'	then	'panama'
      when Fnoprovince	=	'panjin'	then	'liaoning'
      when Fnoprovince	=	'peking'	then	'peking'
      when Fnoprovince	=	'pest'	then	'budapest'
      when Fnoprovince	=	'pingdingshan'	then	'henan'
      when Fnoprovince	=	'pittsfield'	then	'pittsfield'
      when Fnoprovince	=	'pudong'	then	'shanghai'
      when Fnoprovince	=	'puyang'	then	'henan'
      when Fnoprovince	=	'qianjiang'	then	'hubei'
      when Fnoprovince	=	'qiba'	then	'gansu'
      when Fnoprovince	=	'qihe county'	then	'shandong'
      when Fnoprovince	=	'qing hai'	then	'qinghai'
      when Fnoprovince	=	'qingdao'	then	'shandong'
      when Fnoprovince	=	'qinghai'	then	'qinghai'
      when Fnoprovince	=	'qingyang'	then	'gansu'
      when Fnoprovince	=	'qinhuangdao'	then	'hebei'
      when Fnoprovince	=	'quanzhou'	then	'fujian'
      when Fnoprovince	=	'qujing'	then	'yunnan'
      when Fnoprovince	=	'richmond'	then	'richmond'
      when Fnoprovince	=	'rizhao'	then	'shandong'
      when Fnoprovince	=	'ruian'	then	'zhejiang'
      when Fnoprovince	=	'ruzhou'	then	'henan'
      when Fnoprovince	=	'santai'	then	'sichuan'
      when Fnoprovince	=	'saraburi'	then	'thailand'
      when Fnoprovince	=	'sc'	then	'No Province'
      when Fnoprovince	=	'sd'	then	'No Province'
      when Fnoprovince	=	'select state code..'	then	'No Province'
      when Fnoprovince	=	'select state code...'	then	'No Province'
      when Fnoprovince	=	'select..'	then	'No Province'
      when Fnoprovince	=	'sh'	then	'No Province'
      when Fnoprovince	=	'shaanx'	then	'shaanxi'
      when Fnoprovince	=	'shanxi'	then	'shanxi'
      when Fnoprovince	=	'shanxi,'	then	'shanxi'
      when Fnoprovince	=	'shaghai'	then	'shanghai'
      when Fnoprovince	=	'shan dong'	then	'shandong'
      when Fnoprovince	=	'shan xi'	then	'shanxi'
      when Fnoprovince	=	'shan xi sheng'	then	'shanxi'
      when Fnoprovince	=	'shananxi'	then	'shanxi'
      when Fnoprovince	=	'shandong'	then	'shandong'
      when Fnoprovince	=	'shang hai'	then	'shanghai'
      when Fnoprovince	=	'shandong'	then	'shandong'
      when Fnoprovince	=	'shanghai'	then	'shanghai'
      when Fnoprovince	=	'shanghai municipality'	then	'shanghai'
      when Fnoprovince	=	'shanghai,'	then	'shanghai'
      when Fnoprovince	=	'shanghia'	then	'shanghai'
      when Fnoprovince	=	'shangrao'	then	'jiangxi'
      when Fnoprovince	=	'shannxi'	then	'shanxi'
      when Fnoprovince	=	'shantou'	then	'guangdong'
      when Fnoprovince	=	'shanxi'	then	'shanxi'
      when Fnoprovince	=	'shanxi provice'	then	'shanxi'
      when Fnoprovince	=	'shaoshan'	then	'hunan'
      when Fnoprovince	=	'shaoxing'	then	'zhejiang'
      when Fnoprovince	=	'shenyang'	then	'liaoning'
      when Fnoprovince	=	'shenzhen'	then	'guangdong'
      when Fnoprovince	=	'shenzhencity'	then	'guangdong'
      when Fnoprovince	=	'shenzheng'	then	'guangdong'
      when Fnoprovince	=	'shihezi'	then	'xinjiang'
      when Fnoprovince	=	'shijiazhuang'	then	'hebei'
      when Fnoprovince	=	'shiyan'	then	'hubei'
      when Fnoprovince	=	'shizuoka'	then	'japan'
      when Fnoprovince	=	'shjiazhuang'	then	'hebei'
      when Fnoprovince	=	'shouguang'	then	'shandong'
      when Fnoprovince	=	'shunde'	then	'guangdong'
      when Fnoprovince	=	'si chuan'	then	'sichuan'
      when Fnoprovince	=	'sichaun'	then	'sichuan'
      when Fnoprovince	=	'sichua'	then	'sichuan'
      when Fnoprovince	=	'sichuan'	then	'sichuan'
      when Fnoprovince	=	'sichuang'	then	'sichuan'
      when Fnoprovince	=	'sicuan'	then	'sichuan'
      when Fnoprovince	=	'singapore'	then	'singapore'
      when Fnoprovince	=	'singpoare'	then	'singapore'
      when Fnoprovince	=	'sn'	then	'No Province'
      when Fnoprovince	=	'suqian'	then	'jiangsu'
      when Fnoprovince	=	'suzhou'	then	'anhui'
      when Fnoprovince	=	'tai yuan'	then	'shanxi'
      when Fnoprovince	=	'taichung'	then	'taiwan'
      when Fnoprovince	=	'taiyuan'	then	'shanxi'
      when Fnoprovince	=	'taizhou'	then	'jiangsu'
      when Fnoprovince	=	'tangshan'	then	'hebei'
      when Fnoprovince	=	'tempe'	then	'arizona'
      when Fnoprovince	=	'tengzhou'	then	'shandong'
      when Fnoprovince	=	'tian jin'	then	'municipal'
      when Fnoprovince	=	'tianjian'	then	'municipal'
      when Fnoprovince	=	'tianjin'	then	'municipal'
      when Fnoprovince	=	'tianjin,china'	then	'municipal'
      when Fnoprovince	=	'tiayuan'	then	'shanxi'
      when Fnoprovince	=	'tieling'	then	'liaoning'
      when Fnoprovince	=	'tongcheng'	then	'anhui'
      when Fnoprovince	=	'tongchuan'	then	'shaanxi'
      when Fnoprovince	=	'tongling'	then	'anhui'
      when Fnoprovince	=	'tucson'	then	'arizona'
      when Fnoprovince	=	'uganda'	then	'uganda'
      when Fnoprovince	=	'ulanqab'	then	'inner mongolia'
      when Fnoprovince	=	'urumchi'	then	'xinjiang'
      when Fnoprovince	=	'urumqi'	then	'xinjiang'
      when Fnoprovince	=	'victoria'	then	'victoria'
      when Fnoprovince	=	'wa'	then	'washington'
      when Fnoprovince	=	'washington'	then	'washington'
      when Fnoprovince	=	'weifang'	then	'shandong'
      when Fnoprovince	=	'weihai'	then	'shandong'
      when Fnoprovince	=	'wenling'	then	'zhejiang'
      when Fnoprovince	=	'wenzhou'	then	'zhejiang'
      when Fnoprovince	=	'wi'	then	'wisconsin'
      when Fnoprovince	=	'wuhan'	then	'hubei'
      when Fnoprovince	=	'wuhan hubei'	then	'hubei'
      when Fnoprovince	=	'wuhou sichuan'	then	'sichuan'
      when Fnoprovince	=	'wuhu'	then	'anhui'
      when Fnoprovince	=	'wuwei'	then	'gansu'
      when Fnoprovince	=	'wuxi'	then	'jiangsu'
      when Fnoprovince	=	'wwww'	then	'No Province'
      when Fnoprovince	=	'xi an'	then	'shaanxi'
      when Fnoprovince	=	'xiamen'	then	'fujian'
      when Fnoprovince	=	'xian'	then	'shaanxi'
      when Fnoprovince	=	'xiangfan'	then	'hunan'
      when Fnoprovince	=	'xiangtan'	then	'hunan'
      when Fnoprovince	=	'xiangyang'	then	'hubei'
      when Fnoprovince	=	'xianyang'	then	'shaanxi'
      when Fnoprovince	=	'xiaogan'	then	'hubei'
      when Fnoprovince	=	'xin jiang'	then	'xinjiang'
      when Fnoprovince	=	'xinchang'	then	'zhejiang'
      when Fnoprovince	=	'xingjiang'	then	'xinjiang'
      when Fnoprovince	=	'xining'	then	'qinghai'
      when Fnoprovince	=	'xinjiang'	then	'xinjiang'
      when Fnoprovince	=	'xinjiang uygur'	then	'xinjiang'
      when Fnoprovince	=	'xinjiang uygur autonomous reg.'	then	'xinjiang'
      when Fnoprovince	=	'xinjiang uygur autonomous regi'	then	'xinjiang'
      when Fnoprovince	=	'xintai'	then	'shandong'
      when Fnoprovince	=	'xinxiang'	then	'henan'
      when Fnoprovince	=	'xinyang'	then	'henan'
      when Fnoprovince	=	'xinyu jiangxi'	then	'jiangxi'
      when Fnoprovince	=	'xinzhou'	then	'shanxi'
      when Fnoprovince	=	'xishuangbanna'	then	'yunnan'
      when Fnoprovince	=	'xuancheng'	then	'anhui'
      when Fnoprovince	=	'xuchang'	then	'henan'
      when Fnoprovince	=	'xucheng'	then	'henan'
      when Fnoprovince	=	'xuzhou'	then	'jiangsu'
      when Fnoprovince	=	'xx'	then	'No Province'
      when Fnoprovince	=	'yancheng'	then	'jiangsu'
      when Fnoprovince	=	'yangquan'	then	'shanxi'
      when Fnoprovince	=	'yangzhong'	then	'jiangsu'
      when Fnoprovince	=	'yangzhou'	then	'jiangsu'
      when Fnoprovince	=	'yantai'	then	'shandong'
      when Fnoprovince	=	'yibin'	then	'sichuan'
      when Fnoprovince	=	'yichang'	then	'hubei'
      when Fnoprovince	=	'yichun'	then	'heilongjiang'
      when Fnoprovince	=	'yima'	then	'henan'
      when Fnoprovince	=	'yin chuan'	then	'ningxia'
      when Fnoprovince	=	'yinchuan'	then	'ningxia'
      when Fnoprovince	=	'yingkou'	then	'liaoning'
      when Fnoprovince	=	'yiwu'	then	'zhejiang'
      when Fnoprovince	=	'yixing'	then	'jiangsu'
      when Fnoprovince	=	'yiyang'	then	'hunan'
      when Fnoprovince	=	'yongkang'	then	'zhejiang'
      when Fnoprovince	=	'yubei'	then	'chongqing'
      when Fnoprovince	=	'yueyang'	then	'hunan'
      when Fnoprovince	=	'yunan'	then	'guangdong'
      when Fnoprovince	=	'yuncheng'	then	'shanxi'
      when Fnoprovince	=	'yunnan'	then	'yunnan'
      when Fnoprovince	=	'yuyao'	then	'zhejiang'
      when Fnoprovince	=	'yuzhou'	then	'henan'
      when Fnoprovince	=	'zaozhaung'	then	'shandong'
      when Fnoprovince	=	'zaozhuang'	then	'shandong'
      when Fnoprovince	=	'zhabei'	then	'shanghai'
      when Fnoprovince	=	'zhangzhou'	then	'fujian'
      when Fnoprovince	=	'zhanjiang'	then	'guangdong'
      when Fnoprovince	=	'zhaodong'	then	'heilongjiang'
      when Fnoprovince	=	'zhe jiang'	then	'zhejiang'
      when Fnoprovince	=	'zhejiang'	then	'zhejiang'
      when Fnoprovince	=	'zhejiangsheng'	then	'zhejiang'
      when Fnoprovince	=	'zhengjiang'	then	'jiangsu'
      when Fnoprovince	=	'zhengzhou'	then	'henan'
      when Fnoprovince	=	'zhenjiang'	then	'jiangsu'
      when Fnoprovince	=	'zhonglu chaoyang'	then	'beijing'
      when Fnoprovince	=	'zhonglu choayang'	then	'beijing'
      when Fnoprovince	=	'zhongwei'	then	'ningxia'
      when Fnoprovince	=	'zhoushan'	then	'zhejiang'
      when Fnoprovince	=	'zhuhai'	then	'guangdong'
      when Fnoprovince	=	'zhuji'	then	'zhejiang'
      when Fnoprovince	=	'zibo'	then	'shandong'
      when Fnoprovince	=	'zigong'	then	'sichuan'
      when Fnoprovince	=	'zunyi'	then	'guizhou'
      else 'shaanxi'
  end as provincedata
from mod as a")
new$Fnoprovince = as.factor(new$Fnoprovince)
new$provincedata = as.factor(new$provincedata)

levels(new$provincedata)
levels(new$Fnoprovince)
length(levels(new$provincedata))
#87
length(levels(new$Fnoprovince))
#456

names(new)
write.csv(new[-1], "modifiedcsv3102016.csv")
#___________________________________________________________________________________________________
#  market analysis made easy
#___________________________________________________________________________________________________

mod = read.csv("modifiedcsv3102016.csv")
library(sqldf)
frequencyapplyadmitcount = sqldf("select a.*, (admitted*100/applied)||'%' as applyadmitperc,(enrolled*100/admitted)||'%' as enrolladmitperc, (enrolled*100/applied)||'%' as enrollapplyperc from (select ApplicationYear,count(AdmittedFlag) as applied,sum(Admit) as admitted, sum(Enrolled) as enrolled from mod group by ApplicationYear) as a group by a.ApplicationYear") 

#   ApplicationYear applied admitted enrolled applyadmitperc enrolladmitperc enrollapplyperc
#1            2009     857      436      123            50%             28%             14%
#2            2010    3253     1297      452            39%             34%             13%
#3            2011    3862     1701      655            44%             38%             16%
#4            2012    4931     2227      804            45%             36%             16%
#5            2013    7246     3080     1128            42%             36%             15%
#6            2014    7337     3338     1316            45%             39%             17%
#7            2015    5626     2243      856            39%             38%             15%

frequencyapplyadmitcount = sqldf("select a.*, (admitted*100/applied)||'%' as applyadmitperc,(enrolled*100/admitted)||'%' as enrolladmitperc, (enrolled*100/applied)||'%' as enrollapplyperc from (select ApplicationYear,count(AdmittedFlag) as applied,sum(Admit) as admitted, sum(Enrolled) as enrolled from mod where ApplicationComplete = 1 group by ApplicationYear) as a group by a.ApplicationYear") 
#     ApplicationYear applied admitted enrolled applyadmitperc enrolladmitperc enrollapplyperc
#1            2009     492      436      123            88%             28%             25%
#2            2010    1713     1297      452            75%             34%             26%
#3            2011    2280     1701      655            74%             38%             28%
#4            2012    3454     2227      804            64%             36%             23%
#5            2013    4684     3080     1128            65%             36%             24%
#6            2014    4349     3338     1316            76%             39%             30%
#7            2015    3764     2243      856            59%             38%             22%


#__________________________________________________________________________________________________

# 3/14/2016 data analysis

#__________________________________________________________________________________________________

sort(names(mod))

#whereever the admit date is null we have to replace it with the application date

mod$AdmitDate[which(mod$AdmitDate[which(mod$Admit == 0)]!= '')]
#2194

length(which(mod$AdmitDate[which(mod$Admit == 0)] == ''))
#16596

length(mod$AdmitDate[which(mod$Admit == 0)])
#18790

# we have observed that there are 18790 people who dont have an admit and out of that 2194 students who 
# didnt get an admit have an admit date !lol

#whereever the admit date is null (for rejects) we have to replace it with the application date
#and where ever the admit date is provided for not admitted students - consider that as the reject date

length(which(mod$AdmitDate== ''))
#16598

length(which(mod$Admit == 0))

names(mod)

#18790

table(mod$AdmitTypeAdj,mod$AdmitTypeAdjDescr)

sort(unique(mod$monthYear[which(mod$ApplicationYear == 2015)]))
levels(mod$NSCENROLLMENTTYPE)

names(mod)

#________________________________________________________________________________________________
#3172016 class of data column
#________________________________________________________________________________________________
mod = read.csv("modifiedcsv3152016.csv")
names(mod)
class(names(mod))
a = list()
for ( i in 1: length(names(mod)))
{
    a[i] = class(mod[,names(mod)[i]])
}
b = cbind(names(mod),unlist(a))
print(b)



