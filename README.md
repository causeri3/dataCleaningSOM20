# DatacleaningSOM20

For REACH Initiative, Joint Multi Cluster Needs Asessment (JMCNA) report, 2020 Somalia:

In order to support the cleaning of the data.
- Checks for dublicates, outliers, sensitive data, "others" answers by means of package cleaninginspectoR
  -> xlsx-file "OutliersEtc"
- Spelling checks in character variables 
- Main part consists of finding inconsistencies in the survey answers 
  ->xlsx-file "inconsistencies"

Main export is one xlsx-file combining all found issues sorted by uuid 
  -> xlsx-file "Out_Inc"
In order to prioritize call backs, count of possible errors per inconstistency can be exported
  -> xlsx-file "inconCount"
