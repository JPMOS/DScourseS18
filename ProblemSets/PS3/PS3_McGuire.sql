CREATE TABLE florida(policyID,statecode,county,eq_site_limit,hu_site_limit,fl_site_limit,fr_site_limit,tiv_2011,tiv_2012,eq_site_deductible,hu_site_deductible,fl_site_deductible,fr_site_deductible,point_latitude,point_longitude,line,construction,point_granularity)
 .import FL_insurance_sample.csv florida 
SELECT * FROM florida LIMIT 10;
SELECT DISTINCT country FROM florida;
SELECT AVG(tiv_2012 - tiv_2011) FROM florida;
SELECT construction, COUNT(*) FROM florida GROUP BY construction;
