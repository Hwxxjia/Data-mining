DROP TABLE IF EXISTS ab_tbl;
DROP TABLE IF EXISTS me;
DROP TABLE IF EXISTS me_then_ab;
DROP TABLE IF EXISTS ab_then_me;
DROP TABLE IF EXISTS suspicion_of_infection;

CREATE TEMPORARY TABLE ab_tbl AS 
  select
      abx.subject_id, abx.hadm_id, abx.stay_id
    , abx.antibiotic
    , abx.starttime AS antibiotic_time
    -- date is used to match microbiology cultures with only date available
    , cast(abx.starttime AS DATE) AS antibiotic_date
    , abx.stoptime AS antibiotic_stoptime
    -- create a unique identifier for each patient antibiotic
    , ROW_NUMBER() OVER
    (
      PARTITION BY subject_id
      ORDER BY starttime, stoptime, antibiotic
    ) AS ab_id
  from mimiciv_derived.antibiotic as abx; 
  
CREATE TEMPORARY TABLE me as
  select micro_specimen_id
    -- the following columns are identical for all rows of the same micro_specimen_id
    -- these aggregates simply collapse duplicates down to 1 row
    , MAX(subject_id) AS subject_id
    , MAX(hadm_id) AS hadm_id
    , CAST(MAX(chartdate) AS DATE) AS chartdate
    , MAX(charttime) AS charttime
    , MAX(spec_type_desc) AS spec_type_desc
    , max(case when org_name is not null and org_name != '' then 1 else 0 end) as PositiveCulture
  from mimiciv_hosp.microbiologyevents
  group by micro_specimen_id;

CREATE TEMPORARY TABLE me_then_ab AS 
  select
    ab_tbl.subject_id
    , ab_tbl.hadm_id
    , ab_tbl.stay_id
    , ab_tbl.ab_id
    
    , me72.micro_specimen_id
    , coalesce(me72.charttime, CAST(me72.chartdate AS DATE)) as last72_charttime
    , me72.positiveculture as last72_positiveculture
    , me72.spec_type_desc as last72_specimen

    -- we will use this partition to select the earliest culture before this abx
    -- this ensures each antibiotic is only matched to a single culture
    -- and consequently we have 1 row per antibiotic
    , ROW_NUMBER() OVER
    (
      PARTITION BY ab_tbl.subject_id, ab_tbl.ab_id
      ORDER BY me72.chartdate, me72.charttime NULLS LAST
    ) AS micro_seq
  from ab_tbl
  -- abx taken after culture, but no more than 72 hours after
  LEFT JOIN me me72
    on ab_tbl.subject_id = me72.subject_id
    and
    (
      (
      -- if charttime is available, use it
          me72.charttime is not null
      and ab_tbl.antibiotic_time > me72.charttime
      and ab_tbl.antibiotic_time <= me72.charttime + INTERVAL '72 hours'
      )
      OR
      (
      -- if charttime is not available, use chartdate
          me72.charttime is null
      and antibiotic_date >= me72.chartdate
      and antibiotic_date <= me72.chartdate + INTERVAL '3 days'
      )
    );

CREATE TEMPORARY TABLE ab_then_me AS
  select
      ab_tbl.subject_id
    , ab_tbl.hadm_id
    , ab_tbl.stay_id
    , ab_tbl.ab_id
    
    , me24.micro_specimen_id
    , COALESCE(me24.charttime, CAST(me24.chartdate AS DATE)) as next24_charttime
    , me24.positiveculture as next24_positiveculture
    , me24.spec_type_desc as next24_specimen

    -- we will use this partition to select the earliest culture before this abx
    -- this ensures each antibiotic is only matched to a single culture
    -- and consequently we have 1 row per antibiotic
    , ROW_NUMBER() OVER
    (
      PARTITION BY ab_tbl.subject_id, ab_tbl.ab_id
      ORDER BY me24.chartdate, me24.charttime NULLS LAST
    ) AS micro_seq
  from ab_tbl
  -- culture in subsequent 24 hours
  LEFT JOIN me me24
    on ab_tbl.subject_id = me24.subject_id
    and
    (
      (
          -- if charttime is available, use it
          me24.charttime is not null
      and ab_tbl.antibiotic_time >= me24.charttime - INTERVAL '24 hours'  
      and ab_tbl.antibiotic_time < me24.charttime
      )
      OR
      (
          -- if charttime is not available, use chartdate
          me24.charttime is null
      and ab_tbl.antibiotic_date >= me24.chartdate - INTERVAL '1 day'
      and ab_tbl.antibiotic_date <= me24.chartdate
      )
    );

CREATE TEMPORARY TABLE suspicion_of_infection AS
SELECT
ab_tbl.subject_id
, ab_tbl.stay_id
, ab_tbl.hadm_id
, ab_tbl.ab_id
, ab_tbl.antibiotic
, ab_tbl.antibiotic_time

, CASE
  WHEN last72_specimen IS NULL AND next24_specimen IS NULL
    THEN 0
  ELSE 1 
  END AS suspected_infection
-- time of suspected infection:
--    (1) the culture time (if before antibiotic)
--    (2) or the antibiotic time (if before culture)
, CASE
  WHEN last72_specimen IS NULL AND next24_specimen IS NULL
    THEN NULL
  ELSE COALESCE(last72_charttime, antibiotic_time)
  END AS suspected_infection_time

, COALESCE(last72_charttime, next24_charttime) AS culture_time

-- the specimen that was cultured
, COALESCE(last72_specimen, next24_specimen) AS specimen

-- whether the cultured specimen ended up being positive or not
, COALESCE(last72_positiveculture, next24_positiveculture) AS positive_culture

FROM ab_tbl
LEFT JOIN ab_then_me ab2me
    ON ab_tbl.subject_id = ab2me.subject_id
    AND ab_tbl.ab_id = ab2me.ab_id
    AND ab2me.micro_seq = 1
LEFT JOIN me_then_ab me2ab
    ON ab_tbl.subject_id = me2ab.subject_id
    AND ab_tbl.ab_id = me2ab.ab_id
    AND me2ab.micro_seq = 1
;

-- Creates a table with "onset" time of Sepsis-3 in the ICU.
-- That is, the earliest time at which a patient had SOFA >= 2 and suspicion of infection.
-- As many variables used in SOFA are only collected in the ICU, this query can only
-- define sepsis-3 onset within the ICU.

-- extract rows with SOFA >= 2
-- implicitly this assumes baseline SOFA was 0 before ICU admission.
WITH sofa AS
(
  SELECT stay_id
    , starttime, endtime
    , respiration_24hours as respiration
    , coagulation_24hours as coagulation
    , liver_24hours as liver
    , cardiovascular_24hours as cardiovascular
    , cns_24hours as cns
    , renal_24hours as renal
    , sofa_24hours as sofa_score
  FROM mimiciv_derived.sofa
  WHERE sofa_24hours >= 2
)
, s1 as
(
  SELECT 
    soi.subject_id
    , soi.stay_id
    -- suspicion columns
    , soi.ab_id
    , soi.antibiotic
    , soi.antibiotic_time
    , soi.culture_time
    , soi.suspected_infection
    , soi.suspected_infection_time
    , soi.specimen
    , soi.positive_culture
    -- sofa columns
    , starttime, endtime
    , respiration, coagulation, liver, cardiovascular, cns, renal
    , sofa_score
    -- All rows have an associated suspicion of infection event
    -- Therefore, Sepsis-3 is defined as SOFA >= 2.
    -- Implicitly, the baseline SOFA score is assumed to be zero, as we do not know
    -- if the patient has preexisting (acute or chronic) organ dysfunction 
    -- before the onset of infection.
    , sofa_score >= 2 and suspected_infection = 1 as sepsis3
    -- subselect to the earliest suspicion/antibiotic/SOFA row
    , ROW_NUMBER() OVER
    (
        PARTITION BY soi.stay_id
        ORDER BY suspected_infection_time, antibiotic_time, culture_time, endtime
    ) AS rn_sus
  FROM suspicion_of_infection as soi
  INNER JOIN sofa
    ON soi.stay_id = sofa.stay_id 
    AND sofa.endtime >= soi.suspected_infection_time - INTERVAL '48 hours'
    AND sofa.endtime <= soi.suspected_infection_time + INTERVAL '24 hours'
  -- only include in-ICU rows
  WHERE soi.stay_id is not null
)
SELECT 
subject_id, stay_id
-- note: there may be more than one antibiotic given at this time
, antibiotic_time
-- culture times may be dates, rather than times
, culture_time
, suspected_infection_time
-- endtime is latest time at which the SOFA score is valid
, endtime as sofa_time
, sofa_score
, respiration, coagulation, liver, cardiovascular, cns, renal
, sepsis3
FROM s1
WHERE rn_sus = 1
