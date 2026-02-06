#' Estimate Parameters
#'
#' Functions in this file generate numbers from cleaned Alumnae Survey data, to be used in graphs and tables.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile


#' @export
fn_as<-function(asur,coun,var,year){
  dimension<-dim(as.matrix(allcountries %>%filter(survey_year==year,alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)=="Yes") %>% mutate(freq = n / sum(n))))[1]
  dim_yes<-dim(as.matrix(allcountries %>%filter(survey_year==year,alum_survey==asur,get(var)=="Yes" ,country==coun)))[1]
  varb<-ifelse(dimension>1 ,percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)=="Yes") %>% mutate(freq = n / sum(n)))[2,3],accuracy=0.1),
               ifelse(dimension >0 & dim_yes>0,percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)=="Yes") %>% mutate(freq = n / sum(n)))[1,3], accuracy=0.1),
                      ifelse(dimension>0,0,NA)))
  return(varb)
}

#' @export
fn_asc4<-function(asur,coun,var,year){
varb<-ifelse(dim(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)!="No, never")%>% mutate(freq = n / sum(n))))[1]>1,percent(as.matrix(allcountries %>% filter(alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)!="No, never")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1),NA)
return(varb)
}

#' @export
fn_asd5<-function(asur,coun,var,year){
  varb<-ifelse(dim(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)=="Very helpful")%>% mutate(freq = n / sum(n))))[1]>1,percent(as.matrix(allcountries %>% filter(alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)=="Very helpful")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1),NA)
  return(varb)
}


#' @export
fn_asd7<-function(asur,coun,var,year){
  dimension<-dim(as.matrix(allcountries %>%filter(survey_year==year,alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)=="To a great extent") %>% mutate(freq = n / sum(n))))[1]
  dim_yes<-dim(as.matrix(allcountries %>%filter(survey_year==year,alum_survey==asur,get(var)=="To a great extent" ,country==coun)))[1]
  varb<-ifelse(dimension>1 ,percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)=="To a great extent") %>% mutate(freq = n / sum(n)))[2,3],accuracy=0.1),
               ifelse(dimension >0 & dim_yes>0,percent(as.matrix(allcountries %>% filter(alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)=="To a great extent") %>% mutate(freq = n / sum(n)))[1,3], accuracy=0.1),
                      ifelse(dimension>0,0,NA)))
  return(varb)
}


#' @export
fn_asmar<-function(asur,coun,var,year){
  varb<-ifelse(dim(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)=="Married")%>%
                               mutate(freq = n / sum(n))))[1]>1,percent(as.matrix(allcountries %>% filter(alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)=="Married")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1),ifelse(dim(as.matrix(allcountries %>% filter(alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)=="Married")%>% mutate(freq = n / sum(n))))[1]>0,0,NA))
  return(varb)
}


#' @export
fn_asvnb<-function(asur,coun,var,year){
  varb<-ifelse(dim(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)==1)%>% mutate(freq = n / sum(n))))[1]>1,percent(1-as.matrix(allcountries %>% filter(alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)==1)%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1),NA)
  return(varb)
}

#' @export
fn_asvn<-function(asur,coun,var,year){
  varb<-ifelse(dim(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)==1)%>% mutate(freq = n / sum(n))))[1]>1,percent(as.matrix(allcountries %>% filter(alum_survey==asur,get(var)!="" ,country==coun)%>% count(get(var)==1)%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1),NA)
  return(varb)
}

#' @export
fn_asme<-function(asur,coun,var,year){
  varb<-ifelse(dim(allcountries %>% filter(survey_year==year,alum_survey==asur,country==coun)%>% summarize(mean(get(var),na.rm = TRUE)))[1]>0,round((allcountries %>% filter(alum_survey==asur,country==coun)%>% summarize(mean(get(var),na.rm = TRUE)))[1,1],digits=1),NA)
  return(varb)
}

