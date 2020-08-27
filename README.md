# DatacleaningSOM20

Script only works with JMNCA Data from Somalia, 2020.
In order to support the cleaning of the data.
Checks for dublicates, outliers, sensitive data, "others" answers by means of package cleaninginspectoR ("OutliersEtc" is the xlsx-export of those results called).
Spelling checks in character variables and MAIN PART consists of finding inconsistencies in the survey answers ("inconsistencies" is the xlsx-export of those results called)
Main export is one xlsx-file combining all found issues sorted by uuid (called "Out_Inc"). In order to prioritize cleaning, count of possible errors per inconstistency xlsx-file "inconCount" can be exported.
