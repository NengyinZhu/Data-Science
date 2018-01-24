#install.packages("RODBC")
library("RODBC", lib.loc="~/R/win-library/3.2")
# connect to SQL Server
dbConnect<-odbcDriverConnect('driver={SQL Server};server=<>\\<>,1433;database=<>;username=;password=') 
#####

#diagnosisCode
diagnosisCode=c(346.7,347.73,348.8,349.81)

# disease
for (dCode in diagnosisCode){
  data_disease<-sqlQuery(dbConnect,paste("select  ENCOUNTER_ID,DIAGNOSIS_CODE,DIAGNOSIS_DESCRIPTION from D_DIAGNOSIS hd, F_DIAGNOSIS hf where hd.DIAGNOSIS_ID=hf.DIAGNOSIS_ID and hd.DIAGNOSIS_CODE='",dCode,"';",sep=""));
  write.csv(data_disease,file=paste('.\\disease-',dCode,'.csv',sep=''),row.names = FALSE)
}

# patient
for (dCode in diagnosisCode){
  data_patient<-sqlQuery(dbConnect,paste("select encounter_id,e.patient_id,patient_sk,race,AGE_IN_YEARS,gender, marital_status,patient_type_desc,ADMITTED_DT_TM from d_patient p, d_patient_type pt , f_encounter e
 where e.patient_id=p.patient_id and e.patient_type_id=pt.patient_type_id and ENCOUNTER_ID in(select  ENCOUNTER_ID from D_DIAGNOSIS hd, F_DIAGNOSIS hf where hd.DIAGNOSIS_ID=hf.DIAGNOSIS_ID and hd.DIAGNOSIS_CODE='",dCode,"');",sep=""))
  write.csv(data_patient,file=paste('.\\patient-',dCode,'.csv'),row.names = FALSE)                       
}

# medication
for (dCode in diagnosisCode){
  data_medication<-sqlQuery(dbConnect,paste("select encounter_id,ndc_code, brand_name, generic_name, PRODUCT_STRENGTH_DESCRIPTION, route_description, dose_form_description,TOTAL_DISPENSED_DOSES from f_medication mf, d_medication md
 where mf.medication_id=md.medication_id and encounter_id in(select  ENCOUNTER_ID from D_DIAGNOSIS hd, F_DIAGNOSIS hf where hd.DIAGNOSIS_ID=hf.DIAGNOSIS_ID and hd.DIAGNOSIS_CODE='",dCode,"');",sep=""))
  write.csv(data_medication,file=paste('.\\medication-',dCode,'.csv'),row.names = FALSE) 
}