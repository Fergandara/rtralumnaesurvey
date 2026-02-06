#' @export
agency<-function(alum, year){
  len<-as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum, n4_c1!="")%>%count(n4_c1!="No, never"))[2,2]
  vara1 <-as.numeric(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c1!="No, never")%>%count(n4_c2_a=="Yes"))[2,2])
  vara2 <-as.numeric(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c1!="No, never")%>%count(n4_c2_b=="Yes"))[2,2])
  vara3 <-as.numeric(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c1!="No, never")%>%count(n4_c2_c=="Yes"))[2,2])
  vara4 <-as.numeric(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c1!="No, never")%>%count(n4_c2_d=="Yes"))[2,2])
  vara5 <-as.numeric(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c1!="No, never")%>%count(n4_c2_e=="Yes"))[2,2])
  vara6 <-as.numeric(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c1!="No, never")%>%count(n4_c2_f=="Yes"))[2,2])
  vara7 <-as.numeric(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c1!="No, never")%>%count(n4_c2_g=="Yes"))[2,2])
  vara8 <-as.numeric(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c1!="No, never")%>%count(n4_c2_h=="Yes"))[2,2])
  vara9 <-as.numeric(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c1!="No, never")%>%count(n4_c2_i=="Yes"))[2,2])
  vara10 <-as.numeric(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c1!="No, never")%>%count(n4_c2_j=="Yes"))[2,2])
  vara11 <-as.numeric(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c1!="No, never")%>%count(n4_c2_k=="Yes"))[2,2])
  vara1a <-vara1/len
  vara2a <-vara2/len
  vara3a <-vara3/len
  vara4a <-vara4/len
  vara5a <-vara5/len
  vara6a <-vara6/len
  vara7a <-vara7/len
  vara8a <-vara8/len
  vara9a <-vara9/len
  vara10a <-vara10/len
  vara11a <-vara11/len
  tot<-cbind(rbind(vara1,vara2,vara3,vara4,vara5,vara6,vara7,vara8,vara9,vara10,vara11),rbind(vara1a,vara2a,vara3a,vara4a,vara5a,vara6a,vara7a,vara8a,vara9a,vara10a,vara11a))
  return(tot)
}
