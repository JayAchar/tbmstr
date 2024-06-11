updated_descriptions <- list(
  list(
    table_name = "adherence",
    variable = "doses",
    description = "Number of dosese taken in a given month of a drug"
  ),
  list(
    table_name = "adherence",
    variable = "drug",
    description = "Regimen drug"
  ),
  list(
    table_name = "adherence",
    variable = "month",
    description = "Treatment month"
  ),
  list(
    table_name = "adherence",
    variable = "fkey",
    description = "FOREIGN KEY - baseline globalrecordid"
  ),
  list(
    table_name = "adherence",
    variable = "globalrecordid",
    description = "Non-unique identifier"
  ),
  list(
    table_name = "adherence",
    variable = "trtendat",
    description = "Date of last dose of study regimen"
  ),
  list(
    table_name = "adherence",
    variable = "uniquerowid",
    description = "Non-unique identifier"
  ),
  list(
    table_name = "adherence",
    variable = "trtstsdrugdat",
    description = "Start date of specified regimen drug"
  ),
  list(
    table_name = "adverse",
    variable = "aecomment",
    description = "Study team free text comment"
  ),
  list(
    table_name = "adverse",
    variable = "aenddt",
    description = "Date of AE episode resolution"
  ),
  list(
    table_name = "adverse",
    variable = "aeonsetdt",
    description = "Date of AE episode initiation"
  ),
  list(
    table_name = "adverse",
    variable = "aeoutcome",
    description = "Recorded outcome of AE episode"
  ),
  list(
    table_name = "adverse",
    variable = "aeterm",
    description = "Body system effected by AE"
  ),
  list(
    table_name = "adverse",
    variable = "episode",
    description = "AE episode number for a given participant"
  ),
  list(
    table_name = "adverse",
    variable = "globalrecordid",
    description = "FOREIGN KEY - baseline globalrecordid"
  ),
  list(
    table_name = "adverse",
    variable = "changeId",
    description = "FOREIGN KEY - change changeId"
  ),
  list(
    table_name = "adverse",
    variable = "otherae",
    description = "Free text to describe other AE types"
  ),
  list(
    table_name = "adverse",
    variable = "sae",
    description = "Was the AE episode a Serious Adverse Event (SAE)"
  ),
  list(
    table_name = "adverse",
    variable = "saetype",
    description = "Category of SAE"
  ),
  list(
    table_name = "adverse",
    variable = "severity",
    description = "AE severity grade"
  ),
  list(
    table_name = "change",
    variable = "aeaction",
    description = "Response to referenced AE"
  ),
  list(
    table_name = "change",
    variable = "aedrug",
    description = "Drug prescription that was effected"
  ),
  list(
    table_name = "change",
    variable = "changeId",
    description = "FOREIGN KEY adverse changeId"
  ),
  list(
    table_name = "change",
    variable = "episode",
    description = "Drug change episode number for a given participant"
  ),
  list(
    table_name = "change",
    variable = "fkey",
    description = "FOREIGN KEY baseline globalrecordid"
  ),
  list(
    table_name = "change",
    variable = "globalrecordid",
    description = "Non-unique identifier"
  ),
  list(
    table_name = "change",
    variable = "uniquerowid",
    description = "PRIMARY KEY"
  ),
  list(
    table_name = "dst",
    variable = "datedst",
    description = "Date of specimen submission for DST"
  ),
  list(
    table_name = "dst",
    variable = "dstbdq1",
    description = "Phenotypic DST for bedaquiline"
  ),
  list(
    table_name = "dst",
    variable = "dstcfz",
    description = "Phenotypic DST for clofazimine"
  ),
  list(
    table_name = "dst",
    variable = "dstdlm1",
    description = "Phenotypic DST for delamanid"
  ),
  list(
    table_name = "dst",
    variable = "dsth",
    description = "Phenotypic DST for isoniazid"
  ),
  list(
    table_name = "dst",
    variable = "dstr",
    description = "Phenotypic DST for rifampicin"
  ),
  list(
    table_name = "dst",
    variable = "dstlfx",
    description = "Phenotypic DST for levofloxacin"
  ),
  list(
    table_name = "dst",
    variable = "dstmfx",
    description = "Phenotypic DST for moxifloxacin"
  ),
  list(
    table_name = "dst",
    variable = "dstlzd1",
    description = "Phenotypic DST for linezolid"
  ),
  list(
    table_name = "dst",
    variable = "fkey",
    description = "FOREIGN KEY baseline globalrecordid"
  ),
  list(
    table_name = "dst",
    variable = "uniquerowid",
    description = "PRIMARY KEY"
  ),
  list(
    table_name = "dst",
    variable = "globalrecordid",
    description = "Non-unique identifier"
  ),
  list(
    table_name = "dst",
    variable = "monthdst",
    description = "Treatment month when DST performed"
  ),
  list(
    table_name = "monitor",
    variable = "covid19fu",
    description = "Repeat COVID-19 test results"
  ),
  list(
    table_name = "monitor",
    variable = "hbfu",
    description = "Follow-up haemoglobin result"
  ),
  list(
    table_name = "monitor",
    variable = "neutfu",
    description = "Follow-up neutrophil count"
  ),
  list(
    table_name = "monitor",
    variable = "wbcfu",
    description = "Follow-up leukocyte count"
  ),
  list(
    table_name = "monitor",
    variable = "monthcl",
    description = "Treatment month for follow-up testing"
  ),
  list(
    table_name = "monitor",
    variable = "platfu",
    description = "Follow-up platelet count"
  ),
  list(
    table_name = "monitor",
    variable = "potasfu",
    description = "Follow-up serum potassium result"
  ),
  list(
    table_name = "monitor",
    variable = "pregfu",
    description = "Follow-up pregnancy test result"
  ),
  list(
    table_name = "monitor",
    variable = "qtcffu",
    description = "Follow-up corrected QT measurement"
  ),
  list(
    table_name = "monitor",
    variable = "uniquerowid",
    description = "PRIMARY KEY"
  ),
  list(
    table_name = "monitor",
    variable = "wtfu",
    description = "Follow-up weight measurement"
  ),
  list(
    table_name = "monitor",
    variable = "xrayfu",
    description = "Follow-up x-ray result"
  ),
  list(
    table_name = "monitor",
    variable = "creatfu",
    description = "Follow-up serum creatinine measurement"
  ),
  list(
    table_name = "monitor",
    variable = "globalrecordid",
    description = "Non-unique identifier"
  ),
  list(
    table_name = "monitor",
    variable = "fkey",
    description = "FOREIGN KEY baseline globalrecordid"
  ),
  list(
    table_name = "monitor",
    variable = "cldat",
    description = "Date of follow-up testing"
  ),
  list(
    table_name = "myco",
    variable = "datespecimen",
    description = "Date of specimen submission"
  ),
  list(
    table_name = "myco",
    variable = "fkey",
    description = "FOREIGN KEY baseline globalrecordid"
  ),
  list(
    table_name = "myco",
    variable = "globalrecordid",
    description = "Non-unique identifier"
  ),
  list(
    table_name = "myco",
    variable = "month0",
    description = "Treatment month when speciment submitted"
  ),
  list(
    table_name = "myco",
    variable = "result",
    description = "Test result"
  ),
  list(
    table_name = "myco",
    variable = "specimen",
    description = "Type of speciment tested"
  ),
  list(
    table_name = "myco",
    variable = "test_type",
    description = "Type of mycobacterial test performed"
  ),
  list(
    table_name = "myco",
    variable = "uniquerowid",
    description = "PRIMARY KEY"
  ),
  list(
    table_name = "myco",
    variable = "xtype",
    description = "Type of Xpert test performed"
  ),
  list(
    table_name = "baseline",
    variable = "altval",
    description = "ALT value"
  ),
  list(
    table_name = "baseline",
    variable = "art",
    description = "On ART?"
  ),
  list(
    table_name = "baseline",
    variable = "artreg",
    description = "ART regimen"
  ),
  list(
    table_name = "baseline",
    variable = "astval",
    description = "AST value"
  ),
  list(
    table_name = "baseline",
    variable = "blrval",
    description = "Bilirubin value"
  ),
  list(
    table_name = "baseline",
    variable = "cause1",
    description = "Cause of death"
  ),
  list(
    table_name = "baseline",
    variable = "cause2",
    description = "Cause of death"
  ),
  list(
    table_name = "baseline",
    variable = "cause3",
    description = "Cause of death"
  ),
  list(
    table_name = "baseline",
    variable = "cav",
    description = "Were cavities identified on the baseline chest x-ray?"
  ),
  list(
    table_name = "baseline",
    variable = "caval",
    description = "Ionised calcium value"
  ),
  list(
    table_name = "baseline",
    variable = "cd4",
    description = "CD4 count"
  ),
  list(
    table_name = "baseline",
    variable = "cpt",
    description = "Co-trimoxazole therapy"
  ),
  list(
    table_name = "baseline",
    variable = "creatval",
    description = "Baseline serum creatinine value"
  ),
  list(
    table_name = "baseline",
    variable = "diabtype",
    description = "Type of diabetes"
  ),
  list(
    table_name = "baseline",
    variable = "diagnos10",
    description = "Other diagnoses"
  ),
  list(
    table_name = "baseline",
    variable = "diagnos9",
    description = "Other diagnoses"
  ),
  list(
    table_name = "baseline",
    variable = "diagnos8",
    description = "Other diagnoses"
  ),
  list(
    table_name = "baseline",
    variable = "evldat3",
    description = "Date of 3-month post-treatment completion follow-up
    evaluation"
  ),
  list(
    table_name = "baseline",
    variable = "evldat6",
    description = "Date of 6-month post-treatment completion follow-up
    evaluation"
  ),
  list(
    table_name = "baseline",
    variable = "evldat9",
    description = "Date of 9-month post-treatment completion follow-up
    evaluation"
  ),
  list(
    table_name = "baseline",
    variable = "evldat12",
    description = "Date of 12-month post-treatment completion follow-up
    evaluation"
  ),
  list(
    table_name = "baseline",
    variable = "extrot",
    description = "Other extra-pulmonary - free text"
  ),
  list(
    table_name = "baseline",
    variable = "fkey",
    description = "Non-unique identifier"
  ),
  list(
    table_name = "baseline",
    variable = "fucom",
    description = "Follow-up free text comments"
  ),
  list(
    table_name = "baseline",
    variable = "gest",
    description = "Gestational weeks"
  ),
  list(
    table_name = "baseline",
    variable = "hbval",
    description = "Baseline haemoglobin value"
  ),
  list(
    table_name = "baseline",
    variable = "heartype",
    description = "Free text description of heard disease"
  ),
  list(
    table_name = "baseline",
    variable = "kval",
    description = "Baseline potassium value"
  ),
  list(
    table_name = "baseline",
    variable = "kidneytype",
    description = "Severity grade"
  ),
  list(
    table_name = "baseline",
    variable = "livertype",
    description = "Type of viral hepatitis"
  ),
  list(
    table_name = "baseline",
    variable = "mgval",
    description = "Baseline magnesium value"
  ),
  list(
    table_name = "baseline",
    variable = "na",
    description = "Sodium decreased"
  ),
  list(
    table_name = "baseline",
    variable = "naval",
    description = "Baseline serum sodium value"
  ),
  list(
    table_name = "baseline",
    variable = "neuval",
    description = "Baseline neutrophil count"
  ),
  list(
    table_name = "baseline",
    variable = "nofu",
    description = "Did the participant complete post-treatment follow-up"
  ),
  list(
    table_name = "baseline",
    variable = "pack",
    description = "How many packs per day does the participant smoke"
  ),
  list(
    table_name = "baseline",
    variable = "platval",
    description = "Baseline platelet count"
  ),
  list(
    table_name = "baseline",
    variable = "prtclvioldat",
    description = "Date of protocol violation"
  ),
  list(
    table_name = "baseline",
    variable = "prvoutyear",
    description = "Outcome month and year of most recent previous TB treatment
    episode"
  ),
  list(
    table_name = "baseline",
    variable = "prvtrtyear",
    description = "initiation month and year of most recent previous TB
    treatment episode"
  ),
  list(
    table_name = "baseline",
    variable = "reltype3",
    description = "Recurrence type after 3 months of follow-up"
  ),
  list(
    table_name = "baseline",
    variable = "reltype6",
    description = "Recurrence type after 6 months of follow-up"
  ),
  list(
    table_name = "baseline",
    variable = "reltype9",
    description = "Recurrence type after 9 months of follow-up"
  ),
  list(
    table_name = "baseline",
    variable = "reltype12",
    description = "Recurrence type after 12 months of follow-up"
  ),
  list(
    table_name = "baseline",
    variable = "smdur",
    description = "Smoking duration in years"
  ),
  list(
    table_name = "baseline",
    variable = "stat3",
    description = "Study status after 3 months follow-up"
  ),
  list(
    table_name = "baseline",
    variable = "stat6",
    description = "Study status after 6 months follow-up"
  ),
  list(
    table_name = "baseline",
    variable = "stat9",
    description = "Study status after 9 months follow-up"
  ),
  list(
    table_name = "baseline",
    variable = "stat12",
    description = "Study status after 12 months follow-up"
  ),
  list(
    table_name = "baseline",
    variable = "trtcom",
    description = "Free text treatment comments"
  ),
  list(
    table_name = "baseline",
    variable = "trtendat",
    description = "Date of last dose of study regimen"
  )
)
