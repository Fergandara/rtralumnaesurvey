#' Creates Tables for Particular Years and Countries
#'
#' This function loads Alumnae Survey data and creates formatted tables to be shared with country teams.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile



#' @export
create_as_table <- function(alum,cn,year) {
  if(dim(allcountries %>% filter(survey_year==year,alum_survey==alum,country==cn))[1]==0){
    return(as.vector(rep(NA,42)))
  }
  else{
    var1 <- fn_asmar(alum,cn,"n3_b1",year)
    var2 <- fn_as(alum,cn,"n3_b3",year)
    var3 <- fn_asme(alum,cn,"n3_b2",year)
    var4 <- fn_asme(alum,cn,"n3_b5",year)
    var5 <- fn_as(alum,cn,"n2_a1",year)
    var6 <- fn_as(alum,cn,"n2_a3",year)
    var7 <- fn_as(alum,cn,"n2_a2",year)
    var8 <- fn_asvn(alum,cn,"voc",year)
    var9 <- fn_as(alum,cn,"n2_a7",year)
    var10 <- fn_as(alum,cn,"n2_a8",year)
    var11 <- fn_as(alum,cn,"n2_a9",year)
    var12 <- fn_asvnb(alum,cn,"none",year)
    var13 <- fn_asc4(alum,cn,"n4_c1",year)
    var14 <- fn_as(alum,cn,"n4_c2_a",year)
    var15 <- fn_as(alum,cn,"n4_c2_b",year)
    var16 <- fn_as(alum,cn,"n4_c2_c",year)
    var17 <- fn_as(alum,cn,"n4_c2_d",year)
    var18 <- fn_as(alum,cn,"n4_c2_e",year)
    var19 <- fn_as(alum,cn,"n4_c2_f",year)
    var20 <- fn_as(alum,cn,"n4_c2_g",year)
    var21 <- fn_as(alum,cn,"n4_c2_h",year)
    var22 <- fn_as(alum,cn,"n4_c2_i",year)
    var23 <- fn_as(alum,cn,"n4_c2_j",year)
    var24 <- fn_as(alum,cn,"n4_c2_k",year)
    var25 <- fn_as(alum,cn,"n4_c3_a",year)
    var26 <- fn_as(alum,cn,"n4_c3_b",year)
    var27 <- fn_as(alum,cn,"n4_c3_c",year)
    var28 <- fn_as(alum,cn,"n4_c3_d",year)
    var29 <- fn_as(alum,cn,"n4_c3_e",year)
    var30 <- fn_as(alum,cn,"n4_c3_f",year)
    var31 <- fn_as(alum,cn,"n4_c3_g",year)
    var32 <- fn_as(alum,cn,"n4_c3_h",year)
    var33 <- fn_as(alum,cn,"n4_c3_i",year)
    var34 <- fn_as(alum,cn,"n4_c3_j",year)
    var35 <- fn_as(alum,cn,"n4_c3_k",year)
    var36 <-fn_as(alum,cn,"n5_d1",year)
    var37 <- fn_as(alum,cn,"n5_d2",year)
    var38 <- fn_as(alum,cn,"n5_d3",year)
    var39 <- fn_as(alum,cn,"n5_d4",year)
    var40 <- fn_asd5(alum,cn,"n5_d5",year)
    var41 <- fn_as(alum,cn,"n5_d6",year)
    var42 <- fn_asd7(alum,cn,"n5_d7",year)
    return(rbind(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12,var13,var14,var15,var16,var17,var18,var19,var20,var21,var22,var23,var24,var25,var26,var27,var28,var29,var30,var31,var32,var33,
                 var34,var35,var36,var37,var38,var39,var40,var41,var42))
  }
}


#' @export
create_as_table_YR <- function(alum,year) {
  var1 <- percent(as.matrix(allcountries %>% filter(alum_survey==alum,survey_year==year,n3_b1!="" )%>% count(n3_b1=="Married")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var2 <- percent(as.matrix(allcountries %>% filter(alum_survey==alum,survey_year==year,n3_b3!="" )%>% count(n3_b3=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var3 <- round((allcountries %>% filter(survey_year==year,alum_survey==alum)%>% summarize(mean(n3_b2,na.rm = TRUE)))[1,1], digits=1)
  var4 <- round((allcountries %>% filter(survey_year==year,alum_survey==alum)%>% summarize(mean(n3_b5,na.rm = TRUE)))[1,1], digits=1)
  var5 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n2_a1!="" )%>%count(n2_a1=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var6 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n2_a3!="" )%>%count(n2_a3=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var7 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n2_a2!="" )%>%count(n2_a2=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var8 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum) %>%count(voc==1) %>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var9 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n2_a7!="" )%>%count(n2_a7=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var10 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n2_a8!="" )%>%count(n2_a8=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var11 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum, n2_a9!=""  )%>%count(n2_a9=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var12 <- percent(1-as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(none==1)%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var13 <- percent(as.matrix((allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c1!="" )%>%count(n4_c1!="No, never"))%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var14 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c2_a!="" )%>%count(n4_c2_a=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var15 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c2_b!="" )%>%count(n4_c2_b=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var16 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c2_c!="" )%>%count(n4_c2_c=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var17 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c2_d!="" )%>%count(n4_c2_d=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var18 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c2_e!="" )%>%count(n4_c2_e=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var19 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c2_f!="" )%>%count(n4_c2_f=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var20 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c2_g!="" )%>%count(n4_c2_g=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var21 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c2_h!="" )%>%count(n4_c2_h=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var22 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c2_i!="" )%>%count(n4_c2_i=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var23 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c2_j!="" )%>%count(n4_c2_j=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var24 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n4_c2_k!="" )%>%count(n4_c2_k=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var25 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c3_a=="Yes"))[2,2]/(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c2_a=="Yes"))[2,2], accuracy=0.1)
  var26 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c3_b=="Yes"))[2,2]/(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c2_b=="Yes"))[2,2], accuracy=0.1)
  var27 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c3_c=="Yes"))[2,2]/(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c2_c=="Yes"))[2,2], accuracy=0.1)
  var28 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c3_d=="Yes"))[2,2]/(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c2_d=="Yes"))[2,2], accuracy=0.1)
  var29 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c3_e=="Yes"))[2,2]/(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c2_e=="Yes"))[2,2], accuracy=0.1)
  var30 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c3_f=="Yes"))[2,2]/(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c2_f=="Yes"))[2,2], accuracy=0.1)
  var31 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c3_g=="Yes"))[2,2]/(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c2_g=="Yes"))[2,2], accuracy=0.1)
  var32 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c3_h=="Yes"))[2,2]/(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c2_h=="Yes"))[2,2], accuracy=0.1)
  var33 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c3_i=="Yes"))[2,2]/(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c2_i=="Yes"))[2,2], accuracy=0.1)
  var34 <- ifelse(year=="2024" & alum=="1 Year",percent(0),percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c3_j=="Yes"))[2,2]/(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c2_j=="Yes"))[2,2], accuracy=0.1))
  var35 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c3_k=="Yes"))[2,2]/(allcountries %>% filter(survey_year==year,alum_survey==alum)%>%count(n4_c2_k=="Yes"))[2,2], accuracy=0.1)
  var36 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n5_d1!="" )%>%count(n5_d1=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var37 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n5_d2!="" )%>%count(n5_d2=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var38 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n5_d3!="" )%>%count(n5_d3=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var39 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n5_d4!="" )%>%count(n5_d4=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var40 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n5_d5!="" )%>%count(n5_d5=="Very helpful")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var41 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n5_d6!="" )%>%count(n5_d6=="Yes")%>% mutate(freq = n / sum(n)))[2,3], accuracy=0.1)
  var42 <- percent(as.matrix(allcountries %>% filter(survey_year==year,alum_survey==alum,n5_d7!="" )%>%count(n5_d7=="To a great extent")%>% mutate(freq = n / sum(n)))[2,3] , accuracy=0.1)
  return(rbind(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12,var13,var14,var15,var16,var17,var18,var19,var20,var21,var22,var23,var24,var25,var26,var27,var28,var29,var30,var31,var32,var33,
               var34,var35,var36,var37,var38,var39,var40,var41,var42))
}

