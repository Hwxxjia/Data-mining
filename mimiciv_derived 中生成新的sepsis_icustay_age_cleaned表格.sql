-- 保留 subject_id 唯一的记录（原始数据不修改）
DROP TABLE IF EXISTS sepsis_3_cleaned;
CREATE TEMPORARY TABLE sepsis_3_cleaned AS
SELECT s.*
FROM mimiciv_derived.sepsis_3 s
INNER JOIN (
    SELECT subject_id
    FROM mimiciv_derived.sepsis_3
    GROUP BY subject_id
    HAVING COUNT(*) = 1  -- 只保留出现一次的 subject_id
) unique_subjects 
    ON s.subject_id = unique_subjects.subject_id;
	
DROP TABLE IF EXISTS sepsis_icustay;
CREATE TEMPORARY TABLE sepsis_icustay AS
                             --将sepsis_3_cleaned（去重subject_id）与icustay_detail中的数据向右合并

	select 
	sepsis_3_cleaned.*,
	mimiciv_derived.icustay_detail.gender,
	mimiciv_derived.icustay_detail.dod,
	mimiciv_derived.icustay_detail.admittime,
	mimiciv_derived.icustay_detail.dischtime ,
    mimiciv_derived.icustay_detail.los_hospital ,
    mimiciv_derived.icustay_detail.admission_age ,
    mimiciv_derived.icustay_detail.hospital_expire_flag smallint,
    mimiciv_derived.icustay_detail.hospstay_seq ,
    mimiciv_derived.icustay_detail.first_hosp_stay ,
    mimiciv_derived.icustay_detail.icu_intime ,
    mimiciv_derived.icustay_detail.icu_outtime ,
    mimiciv_derived.icustay_detail.los_icu ,
    mimiciv_derived.icustay_detail.icustay_seq,
    mimiciv_derived.icustay_detail.first_icu_stay
	
     from mimiciv_derived.icustay_detail
     right join
     sepsis_3_cleaned
     on mimiciv_derived.icustay_detail.stay_id = sepsis_3_cleaned.stay_id;

DROP TABLE IF EXISTS sepsis_icustaycleaned;
CREATE TEMPORARY TABLE sepsis_icustaycleaned AS
	                                      --提取sepsis_icustay中ICU入住时间大于24小时的病例
select *
     from sepsis_icustay
     where sepsis_icustay.los_icu >= 1 ;
	 
CREATE TABLE IF NOT EXISTS mimiciv_derived.sepsis_icustay_age_cleaned	          
	AS select                                  --提取sepsis_icustaycleaned 中年龄大于18岁的病例	
	sepsis_icustaycleaned.subject_id, 
    sepsis_icustaycleaned.stay_id, 
    sepsis_icustaycleaned.antibiotic_time ,
    sepsis_icustaycleaned.culture_time ,
    sepsis_icustaycleaned.suspected_infection_time ,
    sepsis_icustaycleaned.sofa_time ,
    sepsis_icustaycleaned.sofa_score ,
    sepsis_icustaycleaned.respiration ,
    sepsis_icustaycleaned.coagulation ,
    sepsis_icustaycleaned.liver ,
    sepsis_icustaycleaned.cardiovascular ,
    sepsis_icustaycleaned.cns ,
    sepsis_icustaycleaned.renal ,
    sepsis_icustaycleaned.gender,
	sepsis_icustaycleaned.dod,
	sepsis_icustaycleaned.admittime,
	sepsis_icustaycleaned.dischtime ,
    sepsis_icustaycleaned.los_hospital ,
    sepsis_icustaycleaned.admission_age ,
    --sepsis_icustaycleaned.hospital_expire_flag ,
    sepsis_icustaycleaned.hospstay_seq ,
    sepsis_icustaycleaned.first_hosp_stay ,
    sepsis_icustaycleaned.icu_intime ,
    sepsis_icustaycleaned.icu_outtime ,
    sepsis_icustaycleaned.los_icu ,
    sepsis_icustaycleaned.icustay_seq,
    sepsis_icustaycleaned.first_icu_stay
	from sepsis_icustaycleaned
	where sepsis_icustaycleaned.admission_age  > 18;
	 





