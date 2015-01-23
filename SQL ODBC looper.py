
#####################################################################################################################
# Connects to Teradata server and loops through SQL queries which otherwise will run out of spool space
# Prints completion status 
# Requires pyodbc library
#####################################################################################################################

import pyodbc

cnx = pyodbc.connect("DRIVER={Teradata};DBCNAME=<Address of database>;DATABASE=<Name of database>;AUTHENTICATION=TD2;UID=<User ID>;PWD=<your password>",autocommit=True,ANSI=True)

cursor =cnx.cursor()


# !Manual #
# Enter month range and week range

mth_st = 1320
mth_end = 1392
wk_st = 5791
wk_end = 6060

# Start looping through the query

for i in range(mth_st,mth_end):
       
    cursor.execute("""
insert into  <your oap>.<tablename>  
SELECT
mth_id
,Case
            when <some identifier> = 'EMEA' and cntry.cntry_code in ('DE', 'AT', 'CH') then 'DEATCH'
            when <some identifier> = 'EMEA' and cntry.cntry_code in ('GB', 'IE') then 'UK'
            when <some identifier> = 'EMEA' and cntry.cntry_code not in ('DE', 'AT', 'CH', 'GB', 'IE') then 'CEMEA'
            when <some identifier> = 'NA' and cntry.cntry_code in 'US' then 'US'
            when <some identifier> = 'NA' and cntry.cntry_code in 'CA' then 'CA'
            when <some identifier> = 'LATAM' then 'LATAM'
            when <some identifier> = 'APAC' and cntry.cntry_code in ('AU', 'NZ') then 'AU'
            when <some identifier> = 'APAC' and cntry.cntry_code in ('C2', 'CN') then 'CN'
            when <some identifier> = 'APAC' and cntry.cntry_code in ('IN') then 'IN'
            when <some identifier> = 'APAC' and cntry.cntry_code in ('JP') then 'JP'
            when <some identifier> = 'APAC' and cntry.cntry_code in ('HK','TW','KR') then 'HKTWKR'
            when <some identifier> = 'APAC' and cntry.cntry_code not in ('AU', 'C2','CN','IN','JP','HK','TW','KR') then 'RoAPAC'
end as  Region
,sum( metric ) as metric

FROM

<dbc where base tables are stored>.<monthly data table> mth
join
<dbc where dimension tables are stored>.dim_cntry cntry
on rcvr_cntry_code = cntry.cntry_code
where  mth.mth_id = ?
GROUP BY 1,2
    """, i)
    print (" %d %% complete" % int((i-mth_st)*100/(mth_end-mth_st)))

print ("Monthly Data Extraction Complete")

# Start looping through the query

for i in range (wk_st,wk_end):
    cursor.execute("""
insert into <your oap>.<tablename>   
SELECT
wk_id
,Case
            when <some identifier> = 'EMEA' and cntry.cntry_code in ('DE', 'AT', 'CH') then 'DEATCH'
            when <some identifier> = 'EMEA' and cntry.cntry_code in ('GB', 'IE') then 'UK'
            when <some identifier> = 'EMEA' and cntry.cntry_code not in ('DE', 'AT', 'CH', 'GB', 'IE') then 'CEMEA'
            when <some identifier> = 'NA' and cntry.cntry_code in 'US' then 'US'
            when <some identifier> = 'NA' and cntry.cntry_code in 'CA' then 'CA'
            when <some identifier> = 'LATAM' then 'LATAM'
            when <some identifier> = 'APAC' and cntry.cntry_code in ('AU', 'NZ') then 'AU'
            when <some identifier> = 'APAC' and cntry.cntry_code in ('C2', 'CN') then 'CN'
            when <some identifier> = 'APAC' and cntry.cntry_code in ('IN') then 'IN'
            when <some identifier> = 'APAC' and cntry.cntry_code in ('JP') then 'JP'
            when <some identifier> = 'APAC' and cntry.cntry_code in ('HK','TW','KR') then 'HKTWKR'
            when <some identifier> = 'APAC' and cntry.cntry_code not in ('AU', 'C2','CN','IN','JP','HK','TW','KR') then 'RoAPAC'
end as  Region
,sum( metric ) as metric

FROM

<dbc where base tables are stored>.<weekly data> wk
join
<dbc where dimension tables are stored>.dim_cntry cntry
on rcvr_cntry_code = cntry.cntry_code
where  wk.wk_id = ?
GROUP BY 1,2
""", i)
    print ("%d %% complete" % int((i-wk_st)*100/(wk_end-wk_st)))
    
print ("Weekly Data Extraction complete")

raw_input()
