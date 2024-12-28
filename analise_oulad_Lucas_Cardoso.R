# Install packages ----
  # install.packages("dplyr")
  # install.packages("plotly")

# Library ----
library(dplyr)
library(plotly)

#Set directory to work
  # setwd(dir = ".../Input")

#Reading informations ----

student_info <-
  read.csv(
    file = paste0(getwd(),"/Input/studentInfo.csv")
  )

student_info_profile <-
  student_info[,c(2:6,8,11)] %>%
  distinct(id_student, .keep_all = TRUE)

#Tipos de apresentação e a quantidade de alunos
student_info_presentation <-
  student_info_profile %>%
  mutate(
    type_presentation = substr(code_presentation, nchar(code_presentation), nchar(code_presentation))
  ) %>%
  group_by(type_presentation) %>%
  count() %>%
  ungroup()

student_info_presentation_fig <- plot_ly(
  data = student_info_presentation,
  x = ~type_presentation,
  y = ~n,
  type = 'bar'
)

student_info_presentation_fig <-
  student_info_presentation_fig %>% layout(xaxis = list(title = 'Tipo de Apresentação'),
                                           yaxis = list(title = 'Quantidade de Alunos'))

student_info_presentation_fig

#Alunos por gênero
student_info_gender <-
  student_info_profile %>%
  group_by(gender) %>%
  count() %>%
  ungroup()

student_info_gender_fig <- plot_ly(
  data = student_info_gender,
  x = ~gender,
  y = ~n,
  type = 'bar'
)

student_info_gender_fig <-
  student_info_gender_fig %>% layout(xaxis = list(title = 'Gênero'),
                                     yaxis = list(title = 'Quantidade de Alunos'))

student_info_gender_fig

#Alunos por regiâo
student_info_region <-
  student_info_profile %>%
  group_by(region) %>%
  count() %>%
  ungroup()

#Escolaridade
student_info_education <-
  student_info_profile %>%
  group_by(highest_education) %>%
  count() %>%
  ungroup()

student_info_education_fig <- plot_ly(
  data = student_info_education,
  x = ~highest_education,
  y = ~n,
  type = 'bar'
)

student_info_education_fig <-
  student_info_education_fig %>% layout(xaxis = list(title = 'Escolaridade'),
                                        yaxis = list(title = 'Quantidade de Alunos'))
student_info_education_fig

#Idade
student_info_age <-
  student_info_profile %>%
  group_by(age_band) %>%
  count() %>%
  ungroup()

student_info_age_fig <- plot_ly(
  data = student_info_age,
  x = ~age_band,
  y = ~n,
  type = 'bar'
)

student_info_age_fig <-
  student_info_age_fig %>% layout(xaxis = list(title = 'Idade'),
                                  yaxis = list(title = 'Quantidade de Alunos'))

student_info_age_fig

#Portador de deficiência
student_info_disability <-
  student_info_profile %>%
  group_by(disability) %>%
  count() %>%
  ungroup()

student_info_disability_fig <- plot_ly(
  data = student_info_disability,
  x = ~disability,
  y = ~n,
  type = 'bar'
)

student_info_disability_fig <-
  student_info_disability_fig %>% layout(xaxis = list(title = 'Portador de deficiência'),
                                         yaxis = list(title = 'Quantidade de Alunos'))

student_info_disability_fig

#Informações demográficas compiladas
student_demographic_data <-
  student_info_profile %>%
  group_by(gender, region, highest_education, age_band, disability) %>%
  count() %>%
  ungroup()

#Escolaridade por região dos alunos
student_info_region_education <-
  student_info_profile %>%
  group_by(region, highest_education) %>%
  count() %>%
  ungroup()

#Escolaridade por idade dos alunos
student_info_age_education <-
  student_info_profile %>%
  group_by(age_band, highest_education) %>%
  count() %>%
  ungroup()

#Escolaridade por idade dos alunos
representative_student_group_info <-
  student_info_profile %>%
  subset(
    age_band == "0-35" & 
      (
        highest_education == "A Level or Equivalent"  | 
          highest_education == "Lower Than A Level"  | 
          highest_education == "HE Qualification"
      )
  )

#O quanto esse grupo representa
group_percentage <- 
  (nrow(representative_student_group_info)/nrow(student_info_profile))*100

tabble <-
  data.frame(
    labels = c("Grupo Representativo", "Grupo Não Representativo"),
    value = c(nrow(representative_student_group_info), (nrow(student_info_profile)-nrow(representative_student_group_info)))
  )

representative_pie <- 
  plot_ly(tabble, labels = ~labels, values = ~value, type = 'pie')

representative_pie <- 
  representative_pie %>% layout(title = 'Amostras representativas',
                                
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


representative_pie

#Informações das provas por estudante
student_assessment <-
  read.csv(
    file = paste0(getwd(),"/Input/studentAssessment.csv")
  ) 

#Informações das provas
assessments <-
  read.csv(
    file = paste0(getwd(),"/Input/assessments.csv")
  )

final_exams <-
  assessments %>%
  subset(assessment_type == "Exam")

others_exams <-
  assessments %>%
  subset(assessment_type != "Exam")

#Criando data frame 'student_group_kpis'

student_group_kpis <-
  student_assessment %>%
  mutate(pass = ifelse(score>=40, TRUE, FALSE))

#Juntando com as informações de exame e criando as colunas de quem passou no exame e o peso da grade

student_group_others_exams <-
  student_group_kpis %>%
  inner_join(others_exams, by = "id_assessment") 

student_group_others_exams <-
  student_group_others_exams %>%
  mutate(weight_grade = score*weight/100)

#Média de avaliação final por aluno por módulo

avg_grade_others_exams <-
  student_group_others_exams %>%
  dplyr::group_by(id_student, code_module, code_presentation) %>% 
  mutate(avg_grade = sum(weight_grade)) %>%
  select("id_student","code_module","code_presentation", "avg_grade")

#Pontuação dos exames finais

student_group_final_exams <-
  student_group_kpis %>%
  inner_join(final_exams, by = "id_assessment")  %>%
  dplyr::rename("exams_score" = "score")%>%
  select("id_student","code_module","code_presentation", "exams_score")

#Lendo as tabelas de interações

student_vle <-
  read.csv(
    file = paste0(getwd(),"/Input/studentVle.csv")
  )

vle <-
  read.csv(
    file = paste0(getwd(),"/Input/vle.csv")
    
#limpando os dados de VLE, pois algumas amostras não possuem a semana de referência para os materiais
    
vle <-
  vle %>%
  subset(!is.na(week_from))

#Média geral por aluno por módulo

avg_per_student <-
  student_vle %>%
  dplyr::group_by(id_student, code_module, code_presentation) %>%
  mutate(
    date_mean = mean(date),
    sum_click_mean = mean(sum_click)) %>%
  select("id_student","code_module","code_presentation", "date_mean", "sum_click_mean")
  )

#Filtrando somente amostras representativas (Segundo a analise de perfil dos estudantes)

representative_student_group_info <-
  student_info %>%
  subset(
    age_band == "0-35" & 
      (
        highest_education == "A Level or Equivalent"  | 
          highest_education == "Lower Than A Level"  | 
          highest_education == "HE Qualification"
      ) &
      final_result != "Withdrawn"
  ) %>%
  distinct(id_student, .keep_all = TRUE)

#Compilando as tabelas relevantes

df_1 <-
  inner_join(avg_grade_others_exams, student_group_final_exams, 
             by = c("id_student", "code_module", "code_presentation"))

df_2 <-
  inner_join(representative_student_group_info, df_1, 
             by = c("id_student", "code_module", "code_presentation"))

final_df <-
  inner_join(df_2, avg_per_student, 
             by = c("id_student", "code_module", "code_presentation")) %>%
  select(num_of_prev_attempts, final_result, avg_grade, exams_score, date_mean, sum_click_mean)

final_df <-
  final_df %>%
  subset(sum_click_mean<10)

final_df <-
  final_df %>%
  subset(num_of_prev_attempts<4)

pass_student <-
  final_df %>%
  subset(final_result != "Fail")

fail_student <-
  final_df %>%
  subset(final_result == "Fail")

group_for_analise_performance_pass <-
  pass_student[1:100000,]

performance_pass <- 
  plot_ly(
    group_for_analise_performance_pass, 
    x = ~final_result, y = ~avg_grade, 
    type = 'bar', name = 'Media nos exames'
  )

performance_pass <- 
  performance_pass %>% 
  add_trace(y = ~date_mean, name = 'Tempo interação com o material')

performance_pass <- 
  performance_pass %>% 
  layout(xaxis = list(title = 'Pessoaas Estudantes'),
         yaxis = list(title = 'Valores'), barmode = 'group')

performance_pass

group_for_analise_performance_fail <-
  fail_student[1:100000,]

performance_fail <- 
  plot_ly(
    group_for_analise_performance_fail, 
    x = ~final_result, y = ~avg_grade, 
    type = 'bar', name = 'Media nos exames'
  )

performance_fail <- 
  performance_fail %>% 
  add_trace(y = ~date_mean, name = 'Tempo interação com o material')

performance_fail <- 
  performance_fail %>% 
  layout(xaxis = list(title = 'Pessoaas Estudantes'),
         yaxis = list(title = 'Valores'), barmode = 'group')

performance_fail