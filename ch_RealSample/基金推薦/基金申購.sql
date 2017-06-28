---------------------------------- 基金申購 --------------------------------------------------

use project2017
exec source.dbo.up_droptable 'project2017.dbo.基金推薦_申購明細'

-- 申購歷史資料(申購登錄年月) 199403 到 201512 -- 共680,244 憑證
select max(申購登錄年月) from db_wm.dbo.v_基金申購扣款追蹤h
select min(申購登錄年月) from db_wm.dbo.v_基金申購扣款追蹤h
select count(distinct 憑證) from db_wm.dbo.v_基金申購扣款追蹤h

-- 申購資料h + now

select convert(varchar(4),申購登錄年月,112) yyyy,
	身分證字號,
	憑證,
	DB分類,
	基金中文簡稱1 [基金中文名稱],
	投資型態,
	自然人身分,
	count(*) 購買次數
into #基金申購_扣款明細
from db_wm.dbo.v_基金申購扣款追蹤h
group by 身分證字號,憑證,基金中文簡稱1,DB分類,投資型態,convert(varchar(4),申購登錄年月,112),自然人身分


select convert(varchar(4),申購登錄年月,112) yyyy,
	身分證字號,
	憑證,
	DB分類,
	基金中文簡稱1 [基金中文名稱],
	投資型態,
	自然人身分,
	count(*) 購買次數
into #基金申購_扣款明細2
from db_wm.dbo.v_基金申購扣款追蹤
group by 身分證字號,憑證,基金中文簡稱1,DB分類,投資型態,convert(varchar(4),申購登錄年月,112),自然人身分

select top 10 * from #基金申購_扣款明細 where 憑證= '126V0111500160' -- 同一筆資料若有持續扣款會出現在h 和 現在 
select top 10 * from #基金申購_扣款明細2 where 憑證= '126V0111500160'

select a.yyyy 申購登錄年,
	 a.身分證字號,
	a.基金中文名稱,
	a.憑證,
	a.DB分類,
	a.投資型態,
	a.自然人身分,
	sum(a.購買次數) 購買次數 
into 基金推薦_申購明細
from 
(select  * from #基金申購_扣款明細
union 
select * from #基金申購_扣款明細2) a
group by yyyy,身分證字號,投資型態,基金中文名稱,憑證,DB分類,自然人身分

------------------ view -------------------------------------

alter view  v_基金推薦_申購明細  as 
select  a.*,b.idn from 基金推薦_申購明細 a
	left join source.dbo.idn b
		on a.身分證字號=b.身分證字號


------------------- 基金申購(自然人,>=2015年申購登錄年 ) -------------------------------

select distinct 身分證字號 from 基金推薦_申購明細 -- 112,144人有交易紀錄

select count(distinct 身分證字號) from 基金推薦_申購明細 where 申購登錄年>='2015' --45,324
select count(distinct 憑證) from 基金推薦_申購明細 where 申購登錄年>='2015' --45,324

-- 自然人申購登錄年 >= 2015 
-- 214,528 
select * 
into 基金推薦_申購明細_2015
from 基金推薦_申購明細 where 申購登錄年>='2015' and [自然人身分] = 'a.自然人'
-- select 申購登錄年 , count( distinct 身分證字號) 用戶數
-- from 基金推薦_申購明細 
-- group by 申購登錄年

------------------ 基金特徵 整理 ------------------------------


declare @YYYYMMDD varchar(8)
set @YYYYMMDD= (select max(convert(varchar(8), 更新時間,112)) from  external.dbo.MMA基金基本資料_每週更新)

-- drop table  project2017.dbo.基金推薦_基金屬性

select  
	a.更新時間,
	convert(varchar(8),更新時間,112) yyyymmdd,
	a.基金代碼,
	b.國內外基金註記,
	a.[基金規模(台幣/億)],
	a.基金目前規模區間,
	a.基金成立時間,
	datediff(year,基金成立時間,更新時間) 基金成立幾年,
	b.基金公司代碼,
	b.計價幣別,
	a.基金經理人,
	b.區域別,
	a.基金投資產業分類1,
	a.基金投資產業分類2,
	a.基金投資產業分類3,
	b.AUM基金型態別,
	b.商品投資屬性,
	b.高收益債註記,
	b.保本型基金註記,
	a.淨值,
	a.Sharpe,
	a.Beta,
	a.[一個月累積報酬率(%)],
	a.[三個月累積報酬率(%)],
	a.[六個月累積報酬率(%)],
	a.[一年累積報酬率(%)],
	a.[三年累積報酬率(%)],
	a.[五年累積報酬率(%)],
	a.[自今年以來報酬率(%)],
	a.[自成立日起報酬率(%)],
	case when a.[嘉實資訊基金評等]=0 then NULL
	else a.[嘉實資訊基金評等] end as 基金評等
into project2017.dbo.基金推薦_基金屬性
from external.dbo.MMA基金基本資料_每週更新 a
	left join db_wm.dbo.v_fund b 
		on a.基金代碼 = b.基金代碼
where convert(varchar(8),更新時間,112) = @YYYYMMDD


select top 10 * from project2017.dbo.基金推薦_基金屬性 b 

--------------- 熱賣基金註記 ------------

select  distinct  基金中文名稱, '0' [熱賣基金註記]
into #temp_fund
from 基金推薦_申購明細_2015

delete from #temp_fund 
where 基金中文名稱 in (select top 20 基金中文名稱
from 基金推薦_申購明細_2015
group by 基金中文名稱
order by count(憑證) desc)

select top 20 基金中文名稱, 
	'1' [熱賣基金註記]
-- 	count(憑證) 憑證數
into #temp_hotfund
from 基金推薦_申購明細_2015
group by 基金中文名稱
order by count(憑證) desc 


select * 
into #基金推薦_熱賣基金註記
from #temp_fund
union all
select *,'1' 熱賣基金註記 from #temp_hotfund

select left(基金中文名稱,3) [基金代碼], 
	max(熱賣基金註記) 熱賣基金註記 
into 基金推薦_熱賣基金註記
from #基金推薦_熱賣基金註記1
group by left(基金中文名稱,3)
--- ----------------------------------- VIEW ----------------------------------------------------------------
alter view v_基金推薦_基金申購 as 
select a.*,b.*,
-- 基金評等級距
	case when b.基金評等 < 3 then 'a. [0.5,3)'	
		when b.基金評等 <= 3 then 'b.[3,5]'
		else 'c.ND' end as 基金評等級距,
-- 	 case when b.AUM基金型態別='E' then 'a.股票型'
-- 		  when b.AUM基金型態別='B' then 'b.債券型'
-- 		  when b.AUM基金型態別='M' then 'c.貨幣型'
-- 		  when b.AUM基金型態別 in ('O','W','F','FT','I') then 'd.其他型' else 'ND' end as 型態別,
	 case when b.AUM基金型態別='E' then 'a.股票型'
		  when b.AUM基金型態別='B' then 'b.債券型'
		  when b.AUM基金型態別='M' then 'c.貨幣型'
		  when b.AUM基金型態別='W' then 'd.平衡型'
		  when b.AUM基金型態別='F' then 'e.組合型'
		  when b.AUM基金型態別='FT' then 'f.期貨型'
		  when b.AUM基金型態別='I' then 'g.指數型'
		  when b.AUM基金型態別='O' then 'h.其他型' else 'ND' end as AUM型態別,
--- 成立時間級距
	case when b.基金成立幾年<1 then 'a.<1年'
		when b.基金成立幾年<2 then 'b.1(含)~2年' 
		when b.基金成立幾年<3 then 'c.2(含)~3年' 
		when b.基金成立幾年<5 then 'd.3(含)~5年' 
		when b.基金成立幾年<10 then 'e.5(含)~10年' 
		when b.基金成立幾年<15 then 'f.10(含)~15年' 
		when b.基金成立幾年<20 then 'g.15(含)~20年' 
		when b.基金成立幾年<30 then 'h.20(含)~30年' 
		when b.基金成立幾年<50 then 'i.30(含)~50年' 
		when b.基金成立幾年<100 then 'j.>50年(含)~' 
		else 'ND' end as [基金成立級距(年)],
-- 區域別
	case when b.區域別 = 'TW' then 'TW.台灣'
		when b.區域別 = 'ZA' then 'ZA.南非'
		when b.區域別 = 'BE' then 'BE.比利時'
		when b.區域別 = 'MY' then 'MY.馬來西亞'
		when b.區域別 = 'US' then 'US.美國'
		when b.區域別 = 'PH' then 'PH.菲律賓'
		when b.區域別 = 'HK' then 'HK.香港'
		when b.區域別 = 'AU' then 'AU.澳洲'
		when b.區域別 = 'JP' then 'JP.日本'
		when b.區域別 = 'KR' then 'KR.韓國'			
		when b.區域別 = 'IE' then 'IE.愛爾蘭'
		when b.區域別 = 'CA' then 'CA.加拿大'
		when b.區域別 = 'BR' then 'BR.巴西'
		when b.區域別 = 'IN' then 'IN.印度'
		when b.區域別 = 'CN' then 'CN.中國'
		when b.區域別 = 'GB' then 'GB.英國'
		when b.區域別 = 'DE' then 'DE.德國'
		when b.區域別 = 'ID' then 'ID.印尼'
		when b.區域別 = 'CH' then 'CH.瑞士'
		when b.區域別 = 'RU' then 'RU.俄羅斯'
		when b.區域別 = 'TH' then 'TH.泰國'
		when b.區域別 = 'IT' then 'IT.義大利'
		when b.區域別 = 'LU' then 'LU.全球型'
		when b.區域別 = 'SG' then 'SG.新加坡'
		when b.區域別 = 'VN' then 'VN.越南'
		when b.區域別 = 'FR' then 'FR.法國'
		else 'ND' end as 區域別1,
	d.熱賣基金註記,
	c.idn,
	e.cluster
from 基金推薦_申購明細_2015 a
	left join project2017.dbo.基金推薦_基金屬性 b 
		on left(a.基金中文名稱,3) = b.基金代碼
	left join source.dbo.idn c
		on a.身分證字號=c.身分證字號
	left join project2017.dbo.基金推薦_熱賣基金註記 d
		on left(a.基金中文名稱,3) = d.基金代碼
	left join project2017.dbo.基金推薦_基金分群 e
		on d.基金代碼 = e.基金代碼

select top 10 * from v_基金推薦_基金申購
--------- 熱賣基金profile --------

alter view v_基金推薦_熱賣申購基金 as
select 
	e.憑證數,
	b.*,
	case when b.區域別 = 'TW' then 'TW.台灣'
		when b.區域別 = 'ZA' then 'ZA.南非'
		when b.區域別 = 'BE' then 'BE.比利時'
		when b.區域別 = 'MY' then 'MY.馬來西亞'
		when b.區域別 = 'US' then 'US.美國'
		when b.區域別 = 'PH' then 'PH.菲律賓'
		when b.區域別 = 'HK' then 'HK.香港'
		when b.區域別 = 'AU' then 'AU.澳洲'
		when b.區域別 = 'JP' then 'JP.日本'
		when b.區域別 = 'KR' then 'KR.韓國'			
		when b.區域別 = 'IE' then 'IE.愛爾蘭'
		when b.區域別 = 'CA' then 'CA.加拿大'
		when b.區域別 = 'BR' then 'BR.巴西'
		when b.區域別 = 'IN' then 'IN.印度'
		when b.區域別 = 'CN' then 'CN.中國'
		when b.區域別 = 'GB' then 'GB.英國'
		when b.區域別 = 'DE' then 'DE.德國'
		when b.區域別 = 'ID' then 'ID.印尼'
		when b.區域別 = 'CH' then 'CH.瑞士'
		when b.區域別 = 'RU' then 'RU.俄羅斯'
		when b.區域別 = 'TH' then 'TH.泰國'
		when b.區域別 = 'IT' then 'IT.義大利'
		when b.區域別 = 'LU' then 'LU.全球型'
		when b.區域別 = 'SG' then 'SG.新加坡'
		when b.區域別 = 'VN' then 'VN.越南'
		when b.區域別 = 'FR' then 'FR.法國'
		else 'ND' end as 區域別1, 
	 case when b.AUM基金型態別='E' then 'a.股票型'
		  when b.AUM基金型態別='B' then 'b.債券型'
		  when b.AUM基金型態別='M' then 'c.貨幣型'
		  when b.AUM基金型態別='W' then 'd.平衡型'
		  when b.AUM基金型態別='F' then 'e.組合型'
		  when b.AUM基金型態別='FT' then 'f.期貨型'
		  when b.AUM基金型態別='I' then 'g.指數型'
		  when b.AUM基金型態別='O' then 'h.其他型' else 'ND' end as AUM型態別

from  基金推薦_熱賣基金註記 c 
	left join project2017.dbo.基金推薦_基金屬性 b 
		on c.基金代碼 = b.基金代碼
	left join (select top 20 基金中文名稱, count(憑證) 憑證數
			from 基金推薦_申購明細_2015
				group by 基金中文名稱
			order by count(憑證) desc) e 
		on c.基金代碼=left(e.基金中文名稱,3)
where 熱賣基金註記=1


 ---------------------------------------------- by 基金 view -----------------------------------------------------
-- select * from 基金推薦_基金屬性 
-- select distinct left(基金中文名稱,3) from 基金推薦_熱賣基金註記 
-- 
-- 
-- select left(基金中文名稱,3),count(*),max(熱賣基金註記) from 基金推薦_熱賣基金註記 
-- group by left(基金中文名稱,3)
alter view v_基金推薦_基金屬性 as 
select a.*,
	case when b.註記 is NULL then 0
		else b.熱賣基金註記 end as 熱賣基金註記,
	 case when a.AUM基金型態別='E' then 'a.股票型'
		  when a.AUM基金型態別='B' then 'b.債券型'
		  when a.AUM基金型態別='M' then 'c.貨幣型'
		  when a.AUM基金型態別='W' then 'd.平衡型'
		  when a.AUM基金型態別='F' then 'e.組合型'
		  when a.AUM基金型態別='FT' then 'f.期貨型'
		  when a.AUM基金型態別='I' then 'g.指數型'
		  when a.AUM基金型態別='O' then 'h.其他型' else 'ND' end as 投資型態別,
	c.cluster 
from 基金推薦_基金屬性 a
	left join 基金推薦_熱賣基金註記 b 
		on a.基金代碼= b.基金代碼
	left join 基金推薦_基金分群 c 
		on c.基金代碼= a.基金代碼

-- where 熱賣基金註記 =1

----------- 基金中文與代碼 ---
-- select 基金代碼 + '_'+基金中文名稱 [基金名稱] 
-- into 基金推薦_基金中文名稱
-- from db_wm.dbo.v_fund 

--------------------------------------------------------- 推薦屬性Table切分 ------------------------------------------------------------------------------------------------------
-- cluster1 : 國內股票型 (546)
select * from v_基金推薦_基金屬性
	where cluster = 1

-- cluster2 : 國外債券型 (1031)
select * from v_基金推薦_基金屬性
	where cluster = 2

-- cluster2 : 國外股票型 (1200)
select * from v_基金推薦_基金屬性
	where cluster = 3


---- 申購基金特徵 --
select b.cluster,a.身分證字號,基金中文名稱 from v_基金推薦_申購明細  a
	left join v_基金推薦_基金屬性 b on left(a.基金中文名稱,3) = b.基金代碼
where [申購登錄年] >= 2015

---------------------------------------------------------基金(申購)推薦清單 ------------------------------------------------------------------------------------------------------
---須執行完R code (基金推薦_Hybrid.r) ---
-- 
select top 10 * from 基金推薦_基金中文名稱
select * from dbo.基金推薦_物品混合式_全清單 a

select  a.id [身分證字號],
	b.基金名稱 [基金1] ,
	c.基金名稱 [基金2],
	d.基金名稱 [基金3],
	e.基金名稱 [基金4],
	f.基金名稱 [基金5]
into dbo.基金推薦_物品混合式_全清單1 
from dbo.基金推薦_物品混合式_全清單 a
	left join 基金推薦_基金中文名稱 b on left(b.基金名稱,3)=基金1 
	left join 基金推薦_基金中文名稱 c on left(c.基金名稱,3)=基金2 
	left join 基金推薦_基金中文名稱 d on left(d.基金名稱,3)=基金3 
	left join 基金推薦_基金中文名稱 e on left(e.基金名稱,3)=基金4 
	left join 基金推薦_基金中文名稱 f on left(f.基金名稱,3)=基金5 
--推薦基金數
select top 20 基金1,count(*) n 
into #recc_counts
from 基金推薦_物品混合式_全清單1 
group by 基金1
order by 2 desc

drop table #hot_counts
--熱賣基金
select top 100 基金中文名稱,count(憑證) n
into #hot_counts
from 基金推薦_申購明細_2015
group by 基金中文名稱
order by count(憑證) desc
--- 
select a.基金1 [推薦基金1],
	a.n [推薦次數],
	b.基金中文名稱 [熱賣基金],
	b.n [從2015起憑證數]
 from #recc_counts a
	left join #hot_counts  b on left(b.基金中文名稱,3) = left(a.基金1,3)

select top 10 * from project2017.dbo.基金推薦_物品混合式_全清單1
---------------------------------------------------------- 測試區 -------------------------------------------------------------------------------------------------------------- 

--group by cluster 

select 
	count(*) 基金數,
	sum(國內外基金註記) 國外基金數,
	avg(基金成立幾年) 平均成立幾年,
	sum(熱賣基金註記) 熱賣基金數,
	 sum(高收益債註記) 高收益債數, 
	avg(Sharpe) 平均Sharpe,
	avg(Beta) 平均Beta,
	avg([一個月累積報酬率(%)]) [平均一個月累積報酬率(%)],
	avg([三個月累積報酬率(%)]) [平均三個月累積報酬率(%)],
	avg([六個月累積報酬率(%)]) [平均六個月累積報酬率(%)],
	avg([一年累積報酬率(%)]) [平均一年累積報酬率(%)]
	 from v_基金推薦_基金屬性
group by 國內外基金註記 --cluster


select top 10 保本型基金註記,*from v_基金推薦_基金屬性



select * from v_基金推薦_熱賣申購基金
select top 10 * from v_基金推薦_基金申購
select top 10 區域別,* from db_wm.dbo.v_fund
select top 10 * from 基金推薦_熱賣基金註記
select top 10 * from 基金推薦_基金屬性
select top 10 * from v_基金推薦_基金申購 where 區域別= 'LU'
select * from v_基金推薦_熱賣申購基金
select  
--avg([五年累積報酬率(%)]) 
from  project2017.dbo.基金推薦_基金屬性 where [五年累積報酬率(%)] is not null

select count(*) n ,身分證字號,基金中文名稱 
into #temp
from v_基金推薦_申購明細 
where [申購登錄年] >= 2015
group by 身分證字號,基金中文名稱

select top 10 * from #temp
order by n desc
-- select top 100 convert(varchar(4),YYYYMMDD,112) yyymm,*
-- from 基金推薦_申購明細 where 憑證基金別= 'Y38'
