use project2017 
--------------------------庫存 -----------------------------------------
declare @YYYYMM varchar(6),@YYYY varchar(4)
set @YYYYMM=(select max(right(name,6)) from bank2017.dbo.sysobjects where xtype='V' and left(name,12)='v_CIFALL999_')
select @YYYYMM

exec source.dbo.up_droptable 'project2017.dbo.基金推薦_庫存明細'
exec source.dbo.up_droptable 'project2017.dbo.[基金推薦_持有基金種類(人)]'
exec source.dbo.up_droptable 'project2017.dbo.基金推薦_庫存基金持有統計'


select 身分證字號,
	憑證,
	[交易日(開始)],
	基金中文名稱,
	DB分類,
	自然人身分
into #基金推薦_庫存明細
from DB_WM.dbo.v_庫存基金存量分析
where  YYYYMM = @YYYYMM 


-- where  DB分類 = 'a.一般身分' and YYYYMM = @YYYYMM 
-- where DB分類 = 'a.一般身分' and 投資型態 = 'b.單筆申購' and YYYYMM = @YYYYMM

-- exec sp_rename 'project2017.dbo.基金推薦_庫存明細', 'project2017.dbo.基金推薦_庫存明細_自然人單筆申購'
-- sp_rename '基金推薦_庫存明細_自然人','基金推薦_庫存明細'
-- select top 10 * from  DB_WM.dbo.v_庫存基金存量分析 where DB分類= 'a.一般身分'
-- select count(*) from project2017.dbo.基金推薦_庫存明細 where [交易日(開始)] >'20151231'  --85463
-- select count(distinct 身分證字號)  from project2017.dbo.基金推薦_庫存明細 where [交易日(開始)] >'20151231' 
-- select top 10 * from #基金推薦_庫存明細
select 身分證字號,
	憑證,
	convert(varchar,[交易日(開始)],112) [交易日(開始)],
	convert(varchar(4),[交易日(開始)],112) [交易年(開始)],
	基金中文名稱,
	DB分類,
	自然人身分
into 基金推薦_庫存明細
from  #基金推薦_庫存明細



go
----------------------- view --------------------------------
alter view v_基金推薦_庫存明細 as 
select a.*,b.idn from 基金推薦_庫存明細 a
	left join source.dbo.idn b
		on a.身分證字號=b.身分證字號



select top 10 * from v_基金推薦_庫存明細 where DB分類= 'a.一般身分'

-- select top 10 * from v_基金推薦_庫存明細 where left([交易年(開始)],4) >= '2015'
-- select  distinct 身分證字號 from v_基金推薦_庫存明細 where [交易年(開始)] >= '2015'
------------------------------------------------------------------------



---排除基金交易年 < 2015 --
--- 持有的基金種類數 by 人 --

select a.身分證字號, a.基金中文名稱,count(*) 持有數  
into #fund_no -- 持有憑證數
from (select * from 基金推薦_庫存明細 where [交易年(開始)] >2015) a
group by a.身分證字號,a.基金中文名稱

--1960檔基金
select count(distinct left(基金中文名稱,3)) from #fund_no

-- 32570 user 庫存
select 身分證字號,
	count(*) 持有基金種類,
	sum(持有數) 持有憑證數
-- into [基金推薦_持有基金種類(人)]
from #fund_no
group by 身分證字號


select 基金中文名稱, count(*) 多少人持有
-- into 基金推薦_庫存基金持有統計
from #fund_no
group by 基金中文名稱 
order by 2 desc
 
-- select 持有基金種類,count(*) number from  [基金推薦_持有基金種類(人)] 
-- group by 持有基金種類
-- order by 1




------ 生成基金推薦清單後...執行.... -----

exec source.dbo.up_droptable 'project2017.dbo.基金推薦_全清單'

select 
	b.uid 身分證字號,
	c.持有基金種類,
	c.持有憑證數,
	b.推薦方法,
	b.item1 推薦基金1,
	b.item2 推薦基金2,
	b.item3 推薦基金3,
	b.item4 推薦基金4,
	b.item5 推薦基金5,
	b.item6 推薦基金6,
	b.item7 推薦基金7,
	b.item8 推薦基金8,
	b.item9 推薦基金9,
	b.item10 推薦基金10,
	b.item11 推薦基金11,
	b.item12 推薦基金12,
	b.item13 推薦基金13,
	b.item14 推薦基金14,
	b.item15 推薦基金15,
	b.item16 推薦基金16,
	b.item17 推薦基金17,
	b.item18 推薦基金18,
	b.item19 推薦基金19,
	b.item20 推薦基金20
into 基金推薦_全清單
from (
select 'a.熱門基金' 推薦方法, *  from dbo.基金推薦_熱門基金Top20 
union 
select 'b.用戶相似' 推薦方法 , * from 基金推薦_個人基金Top20 
) b
	left join [基金推薦_持有基金種類(人)] c 
		on b.uid = c.身分證字號


------ view ---- 

ALTER VIEW 
v_基金推薦_全清單
as 
SELECT 
	*,
	CASE WHEN 持有基金種類 <3 THEN 'a.持有1~3種基金'
		WHEN 持有基金種類<6 THEN 'b.持有4~5種基金'
		WHEN 持有基金種類<11 THEN 'c.持有6~10種基金'
		WHEN 持有基金種類<21 THEN 'd.持有11~20種基金'
		WHEN 持有基金種類>20 THEN 'e.持有>20種基金'
		ELSE 'f.異常' end as 基金種類級距,
	CASE WHEN 持有憑證數 <3 THEN 'a.持有1~3基金憑證'
		WHEN 持有憑證數<6 THEN 'b.持有4~5基金憑證'
		WHEN 持有憑證數<11 THEN 'c.持有6~10基金憑證'
		WHEN 持有憑證數<21 THEN 'd.持有11~20基金憑證'
		WHEN 持有憑證數>20 THEN 'd.持有>20基金憑證'
		ELSE 'f.異常' end as 基金憑證數級距
FROM 基金推薦_全清單



---------------------------- 測試區 (待刪除) ------------------------------------------------
select distinct 基金中文名稱 from v_基金推薦_申購明細 where 申購登錄年>= 2015
select top 10 * from v_基金推薦_申購明細 where 申購登錄年>= 2015
select count(distinct 憑證) from v_基金推薦_庫存明細 where [交易年(開始)] >=2015

select 推薦方法, count(*) n from v_基金推薦_全清單
group by 推薦方法
--2748 基金販售 --
select count(*) from external.dbo.MMA基金基本資料_每週更新
where convert(varchar(8),更新時間,112) = '20170322'
---
select top 100 * from v_基金推薦_全清單

select * from 基金推薦_庫存基金持有統計

select  count(*) n
from 
v_基金推薦_全清單

select 基金種類級距 , count(*) n
from 
v_基金推薦_全清單
group by 基金種類級距


select * 
-- into #temp
from dbo.基金推薦_基金相似度_C
where left(基金1,3) = '100'
order by 相似度 desc


select top 100 基金1,count(*) n 
-- into #temp2
from dbo.基金推薦_基金相似度_C
group by 基金1
order by n desc

select * from dbo.基金推薦_基金相似度_J 
where 
-- 基金1 in (select 基金1 from #temp2) 
left(基金1,3) = '213' order by 相似度 desc

select 基金1,基金2,max(相似度) sim from 基金推薦_基金相似度_C
where 基金1 in (select 基金1 from #temp2) 
group by 基金1,基金2
order by 基金1,sim desc
-- order by max(相似度)


-- jaccard 相似度 --

select * 
-- into #temp
from dbo.基金推薦_基金相似度_J
where left(基金1,3) = '65M'
order by 相似度 desc


