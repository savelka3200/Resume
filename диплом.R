library(dplyr)
library(ggplot2)

setwd("C:/Users/USER/OneDrive/Рабочий стол/дипломна робота")

# Дані

# t0 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t0 <- read.csv("T0_u.csv",sep = ";")
# нижній регістр
t0 <- t0 %>%
  mutate(Q3 = tolower(Q3))
# всі пробіли
t0 <- t0 %>%
  mutate(Q3 = gsub("\\s+", "", Q3))
View(t0)
nrow(t0)
length(unique(t0$Q3))
# T1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t1 <- read.csv("Т1_u.csv",sep = ";")
# прибрати перший рядок з розшифровкою питання
t1 <- t1[-1,]
t1 <- t1 %>%
  mutate(Q3 = tolower(Q3))
# всі пробіли
t1 <- t1 %>%
  mutate(Q3 = gsub("\\s+", "", Q3))
View(t1)
nrow(t1)
length(unique(t1$Q3))

# T2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t2 <- read.csv("Т2_u.csv",sep = ";")
# прибрати перший рядок з розшифровкою питання
t2 <- t2[-1,]
t2 <- t2 %>%
  mutate(Q3 = tolower(Q3))
t2 <- t2 %>%
  mutate(Q3 = gsub("\\s+", "", Q3))

nrow(t2)
length(unique(t2$Q3))
View(t2)

# T3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t3 <- read.csv("Т3_u.csv",sep = ";")
# прибрати перший рядок з розшифровкою питання
t3 <- t3[-1,]
t3 <- t3 %>%
  mutate(Q3 = tolower(Q3))
t3 <- t3 %>%
  mutate(Q3 = gsub("\\s+", "", Q3))
View(t3)
nrow(t3)
length(unique(t3$Q3))


# Функції обчислювання індексів

# PSS-10
compute_pss10 <- function(data) {

  #Q20 "Як часто за останній місяць ви були засмучені через щось, що трапилося несподівано?"
  #□	Ніколи = 0
  #□	Майже ніколи = 1
  #□	Іноді = 2
  #□	Досить часто = 3
  #□	Дуже часто = 4
  
  #Q21 "Як часто за останній місяць Ви відчували, що не можете контролювати важливі речі у вашому житті?"
  #□	Ніколи = 0
  #□	Майже ніколи = 1
  #□	Іноді = 2
  #□	Досить часто = 3
  #□	Дуже часто = 4
  
  #Q22 "Як часто за останній місяць ви відчували знервованість та стрес?"
  #□	Ніколи = 0
  #□	Майже ніколи = 1
  #□	Іноді = 2
  #□	Досить часто = 3
  #□	Дуже часто = 4
  
  #Q23 "Як часто за останній місяць ви були впевнені, що зможете вирішити свої особисті проблеми?"
  #□	Ніколи = 4
  #□	Майже ніколи = 3
  #□	Іноді = 2
  #□	Досить часто = 1
  #□	Дуже часто = 0

  #Q24 "Як часто за останній місяць ви відчували, що все відбувається по-вашому (відповідно до ваших бажань, планів та поглядів)?"
  #□	Ніколи = 4
  #□	Майже ніколи = 3
  #□	Іноді = 2
  #□	Досить часто = 1
  #□	Дуже часто = 0
  
  #Q25 "Як часто за останній місяць ви відчували, що не можете впоратися з тим, що маєте зробити?"
  #□	Ніколи = 0
  #□	Майже ніколи = 1
  #□	Іноді = 2
  #□	Досить часто = 3
  #□	Дуже часто = 4
  
  #Q26 Як часто за останній місяць ви були здатні контролювати роздратованість з приводу того, що відбувається у вашому житті?
  #□	Ніколи = 4
  #□	Майже ніколи = 3
  #□	Іноді = 2
  #□	Досить часто = 1
  #□	Дуже часто = 0
  
  #Q27 Як часто за останній місяць ви почувалися «господарем становища»?
  #□	Ніколи = 4
  #□	Майже ніколи = 3
  #□	Іноді = 2
  #□	Досить часто = 1
  #□	Дуже часто = 0
  
  # Q28 "Як часто за останній місяць ви були розсерджені через події, на які не могли впливати?"
  #□	Ніколи = 0
  #□	Майже ніколи = 1
  #□	Іноді = 2
  #□	Досить часто = 3
  #□	Дуже часто = 4
  
  # Q29 Як часто за останній місяць ви відчували, ніби труднощів накопичилося так багато, що ви не можете їх подолати?
  #□	Ніколи = 0
  #□	Майже ніколи = 1
  #□	Іноді = 2
  #□	Досить часто = 3
  #□	Дуже часто = 4
  
  data %>%
    mutate(
      PSS.10 = rowSums(
        cbind(
          recode(Q20, "Ніколи"=0, "Майже ніколи"=1, "Іноді"=2, "Досить часто"=3, "Дуже часто"=4, .default=NA_real_),
          recode(Q21, "Ніколи"=0, "Майже ніколи"=1, "Іноді"=2, "Досить часто"=3, "Дуже часто"=4, .default=NA_real_),
          recode(Q22, "Ніколи"=0, "Майже ніколи"=1, "Іноді"=2, "Досить часто"=3, "Дуже часто"=4, .default=NA_real_),
          recode(Q23, "Ніколи"=4, "Майже ніколи"=3, "Іноді"=2, "Досить часто"=1, "Дуже часто"=0, .default=NA_real_),
          recode(Q24, "Ніколи"=4, "Майже ніколи"=3, "Іноді"=2, "Досить часто"=1, "Дуже часто"=0, .default=NA_real_),
          recode(Q25, "Ніколи"=0, "Майже ніколи"=1, "Іноді"=2, "Досить часто"=3, "Дуже часто"=4, .default=NA_real_),
          recode(Q26, "Ніколи"=4, "Майже ніколи"=3, "Іноді"=2, "Досить часто"=1, "Дуже часто"=0, .default=NA_real_),
          recode(Q27, "Ніколи"=4, "Майже ніколи"=3, "Іноді"=2, "Досить часто"=1, "Дуже часто"=0, .default=NA_real_),
          recode(Q28, "Ніколи"=0, "Майже ніколи"=1, "Іноді"=2, "Досить часто"=3, "Дуже часто"=4, .default=NA_real_),
          recode(Q29, "Ніколи"=0, "Майже ніколи"=1, "Іноді"=2, "Досить часто"=3, "Дуже часто"=4, .default=NA_real_)
        ),
        na.rm = FALSE
      )
    )
}

# PHQ-9
compute_phq9 <- function(data) {
  #Як часто за останні 2 тижні вас непокоїли наступні проблеми?
  #Q31. Зниження інтересу чи відчуття задоволення від виконання справ
  #Q32. Поганий настрій, відчуття пригнічення чи безнадії
  #Q33. Труднощі з засинанням, поверхневий сон або,  навпаки, надмірна сонливість
  #Q34. Відчуття втоми або зниження енергії
  #Q35. Поганий апетит або переїдання
  #Q36. Негативне відчуття щодо себе — що Ви невдаха або, що Ви підвели себе чи свою родину
  #Q37. Труднощі з концентрацією уваги, наприклад, під час читання чи перегляду телевізора
  #Q38. Сповільненість рухів та мовлення, помітна навіть для оточуючих. Або навпаки, надмірна і не притаманна вам метушливість та активність
  #Q39. Думки, що було б краще, якби Ви померли або думки про те, щоб заподіяти собі шкоду
  #ніколи  0
  #кілька дні 1
  #більше половини часу 2
  #майже щодня 3
  
  data %>%
    mutate(
      PHQ.9 = rowSums(
        cbind(
          recode(Q31, "ніколи"=0, "кілька днів"=1, "більше половини часу"=2, "майже щодня"=3, .default=NA_real_),
          recode(Q32, "ніколи"=0, "кілька днів"=1, "більше половини часу"=2, "майже щодня"=3, .default=NA_real_),
          recode(Q33, "ніколи"=0, "кілька днів"=1, "більше половини часу"=2, "майже щодня"=3, .default=NA_real_),
          recode(Q34, "ніколи"=0, "кілька днів"=1, "більше половини часу"=2, "майже щодня"=3, .default=NA_real_),
          recode(Q35, "ніколи"=0, "кілька днів"=1, "більше половини часу"=2, "майже щодня"=3, .default=NA_real_),
          recode(Q36, "ніколи"=0, "кілька днів"=1, "більше половини часу"=2, "майже щодня"=3, .default=NA_real_),
          recode(Q37, "ніколи"=0, "кілька днів"=1, "більше половини часу"=2, "майже щодня"=3, .default=NA_real_),
          recode(Q38, "ніколи"=0, "кілька днів"=1, "більше половини часу"=2, "майже щодня"=3, .default=NA_real_),
          recode(Q39, "ніколи"=0, "кілька днів"=1, "більше половини часу"=2, "майже щодня"=3, .default=NA_real_)
        ),
        na.rm = FALSE
      )
    )
}

# GAD-7
compute_gad7 <- function(data) {
  #Як часто за останні 2 тижні вас непокоїли наступні проблеми?
  					
  #Q42 Ви нервували, відчували тривогу або були дуже напружені.	
  #Q43 Ви не могли зупинити або контролювати своє хвилювання.	
  #Q44 Ви занадто хвилювались через різні речі.	
  #Q45 Вам було важко розслабитись.	
  #Q46 Ви були настільки неспокійні, що Вам було важко всидіти на одному місці.	
  #Q47 Вам було легко дошкулити або роздратувати.	
  #Q48 Ви відчували страх, неначе щось жахливе може статися
  #Зовсім не турбували 0
  #Декілька днів 1
  #Більше половини всіх днів 2
  #Майже щоденно 3
  
  
  data %>%
    mutate(
      GAD.7 = rowSums(
        cbind(
          recode(Q42, "Зовсім не турбували "=0, "Декілька днів "=1, "Більше половини усіх днів"=2, "Майже щоденно"=3, .default=NA_real_),
          recode(Q43, "Зовсім не турбували "=0, "Декілька днів "=1, "Більше половини усіх днів"=2, "Майже щоденно"=3, .default=NA_real_),
          recode(Q44, "Зовсім не турбували "=0, "Декілька днів "=1, "Більше половини усіх днів"=2, "Майже щоденно"=3, .default=NA_real_),
          recode(Q45, "Зовсім не турбували "=0, "Декілька днів "=1, "Більше половини усіх днів"=2, "Майже щоденно"=3, .default=NA_real_),
          recode(Q46, "Зовсім не турбували "=0, "Декілька днів "=1, "Більше половини усіх днів"=2, "Майже щоденно"=3, .default=NA_real_),
          recode(Q47, "Зовсім не турбували "=0, "Декілька днів "=1, "Більше половини усіх днів"=2, "Майже щоденно"=3, .default=NA_real_),
          recode(Q48, "Зовсім не турбували "=0, "Декілька днів "=1, "Більше половини усіх днів"=2, "Майже щоденно"=3, .default=NA_real_)
        ),
        na.rm = FALSE
      )
    )
}

# PCL-5
compute_pcl5 <- function(data) {
  #Як часто за останні 2 тижні вас непокоїли наступні проблеми?
									
  #Q51 Повторювані, тривожні та небажані спогади про стресову подію/ситуацію	
  #Q52 Повторювані, тривожні сни про пережитий стресовий досвід/ситуацію	
  #Q53 Раптове відчуття чи поведінка таким чином, наче стресова подія/ситуація повторюється насправді (наче ви насправді переживаєте її знову)	
  #Q54 Сильне засмучення, якщо щось нагадує вам про стресову подію/ситуацію	
  #Q55 Відчуття сильних фізичних реакцій, коли щось нагадує вам про стресову подію/ситуацію (наприклад, серцебиття, проблеми з диханням, пітливість)	
  #Q56 Уникання спогадів, думок чи почуттів, пов’язаних зі стресовою подією/ситуацією	
  #Q57 Уникання зовнішніх нагадувань про стресові події/ситуації (наприклад, люди, місця, розмови, дії, об’єкти чи ситуації)	
  #Q58 Не можете згадати важливі моменти стресової події/ситуації	
  #Q59 Відчуваєте сильні негативні переконання щодо себе, інших людей чи світу (наприклад, думки на кшталт: я поганий(-на), зі мною щось дуже сильно не так, нікому не можна довіряти, світ цілковито небезпечний)	
  #Q60 Звинувачення себе чи когось іншого у стресовій події/ситуації чи в тому, що сталося після неї	
  #Q61 Сильні негативні почуття, такі як страх, огида, гнів, провина чи сором	
  #Q62 Втрата інтересу до занять, які вам подобалися раніше	
  #Q63 Відчуття віддаленості або відрізаності від інших людей	
  #Q64 Труднощі з проживанням позитивних почуттів (наприклад, нездатність відчувати щастя або почуття любові до близьких)	
  #Q65 Дратівливість, спалахи гніву чи агресивна поведінка	
  #Q66 Занадто ризикована поведінка або дії, які можуть завдати вам шкоди	
  #Q67 Стан “підвищеної тривожності” або пильність чи перебування напоготові	
  #Q68 Відчуття стривоженості або лякливості	
  #Q69 Відчуваєте труднощі з концентрацією?	
  #Q70 Труднощі зі сном чи засинанням?
  #Жодним чином 0
  #Зовсім трохи  1
  #Трохи  2
  #Досить відчутно 3
  #Значним чином 4
 
  data %>%
    mutate(
      PCL.5 = rowSums(
        cbind(
          recode(Q51, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q52, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q53, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q54, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q55, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q56, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q57, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q58, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q59, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q60, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q61, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q62, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q63, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q64, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q65, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q66, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q67, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q68, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q69, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_),
          recode(Q70, "Жодним чином"=0, "Зовсім трохи "=1, "Трохи "=2, "Досить відчутно"=3, "Значним чином"=4, .default=NA_real_)
        ),
        na.rm = FALSE
      )
    )
}

#CD-RISC-10
compute_cdrisk10 <- function(data) {
  #Будь ласка, вкажіть, наскільки Ви погоджуєтеся з такими твердженнями, оцінюючи свій досвід за минулий місяць. Якщо та чи інша ситуація не мала місця останнім часом, як би Ви, на Вашу думку, почувалися за таких обставин?

  									
  #Q72 Я можу адаптуватися до змін	
  #Q73 Я можу впоратися з будь-якими перепонами на своєму шляху	
  #Q74 Я намагаюсь підходити з гумором до проблем, що виникають	
  #Q75 Необхідність протистояти стресу робить мене сильнішим	
  #Q76 Я швидко приходжу до норми після хвороб, травм чи інших негараздів	
  #Q77 Я вважаю, що можу досягти своєї мети, навіть якщо є перешкоди	
  #Q78 У стресовій ситуації я не втрачаю здатність зосереджуватись і ясно мислити	
  #Q79 Я не з тих, кого зупиняють невдачі	
  #Q80 Я вважаю себе сильною особистістю, коли йдеться про виклики та труднощі життя	
  #Q81 Я можу справлятися з неприємними чи болісними відчуттями, як-от сум, страх та гнів
  # Зовсім невірно 0
  # Дуже рідко вірно 1 
  # Іноді вірно 2 
  # Часто вірно 3
  # Майже завжди вірно 4
  
  data %>%
    mutate(
      CD.RISC.10 = rowSums(
        cbind(
          recode(Q72, "Зовсім невірно"=0, "Дуже рідко вірно"=1, "Іноді вірно"=2, "Часто вірно"=3, "Майже завжди вірно"=4, .default=NA_real_),
          recode(Q73, "Зовсім невірно"=0, "Дуже рідко вірно"=1, "Іноді вірно"=2, "Часто вірно"=3, "Майже завжди вірно"=4, .default=NA_real_),
          recode(Q74, "Зовсім невірно"=0, "Дуже рідко вірно"=1, "Іноді вірно"=2, "Часто вірно"=3, "Майже завжди вірно"=4, .default=NA_real_),
          recode(Q75, "Зовсім невірно"=0, "Дуже рідко вірно"=1, "Іноді вірно"=2, "Часто вірно"=3, "Майже завжди вірно"=4, .default=NA_real_),
          recode(Q76, "Зовсім невірно"=0, "Дуже рідко вірно"=1, "Іноді вірно"=2, "Часто вірно"=3, "Майже завжди вірно"=4, .default=NA_real_),
          recode(Q77, "Зовсім невірно"=0, "Дуже рідко вірно"=1, "Іноді вірно"=2, "Часто вірно"=3, "Майже завжди вірно"=4, .default=NA_real_),
          recode(Q78, "Зовсім невірно"=0, "Дуже рідко вірно"=1, "Іноді вірно"=2, "Часто вірно"=3, "Майже завжди вірно"=4, .default=NA_real_),
          recode(Q79, "Зовсім невірно"=0, "Дуже рідко вірно"=1, "Іноді вірно"=2, "Часто вірно"=3, "Майже завжди вірно"=4, .default=NA_real_),
          recode(Q80, "Зовсім невірно"=0, "Дуже рідко вірно"=1, "Іноді вірно"=2, "Часто вірно"=3, "Майже завжди вірно"=4, .default=NA_real_),
          recode(Q81, "Зовсім невірно"=0, "Дуже рідко вірно"=1, "Іноді вірно"=2, "Часто вірно"=3, "Майже завжди вірно"=4, .default=NA_real_)
        ),
        na.rm = FALSE
      )
    )
}

#CRAFFT 
compute_crafft <- function(data) {
  #Скільки днів за останні 12 місяців Ви:
  #Q83_1 Вживали, більш ніж декілька ковтків, пива, вина або ж інших алкогольних напоїв? Відмітьте «0» якщо не вживали. - Кількість днів	
  #Q84_1 Вживаєте якийсь вид марихуани (канабіс, олія, віск, паління, вейпінг, вдихання випарування або їстівні продукти) або синтетичну марихуану («К2» чи «спайс»)? Відмітьте «0» якщо не вживали. - Кількість днів	
  #Q85_1 Вживаєте щось інше для «кайфу» (наприклад, пігулки, заборонені наркотичні засоби, медикаменти, що відпускаються без рецепта, речовини, які ви нюхаєте або вдихаєте, або вводите ін’єкцією)? Відмітьте «0», якщо не вживали - Кількість днів	
  
  #Q86 Чи сідали ви коли-небудь у машину, за кермом якої знаходилася людина (включаючи вас) у стані алкогольного сп’яніння, під дією наркотиків або «під кайфом»?	
  #Q87 Чи вживали ви коли-небудь алкоголь або наркотики, щоб розслабитися, самоствердитися або «вписатися»?	
  #Q88 Чи вживали ви коли-небудь алкоголь або наркотики без друзів, самостійно?	
  #Q89 Чи забували ви коли-небудь те, що робили під впливом алкогольних напоїв або наркотиків?	
  #Q90 Чи говорили вам коли-небудь ваші родичі або друзі, що вам треба менше вживати алкоголь або наркотики?	
  #Q91 Чи потрапляли ви коли-небудь у неприємності, перебуваючи під впливом алкогольних напоїв або наркотиків?
  #Ні 0
  #Так 1
  
  data %>%
    mutate(
      CRAFFT = rowSums(
        cbind(
          recode(Q86, "Ні"=0, "Так"=1, .default=NA_real_),
          recode(Q87, "Ні"=0, "Так"=1, .default=NA_real_),
          recode(Q88, "Ні"=0, "Так"=1, .default=NA_real_),
          recode(Q89, "Ні"=0, "Так"=1, .default=NA_real_),
          recode(Q90, "Ні"=0, "Так"=1, .default=NA_real_),
          recode(Q91, "Ні"=0, "Так"=1, .default=NA_real_)
        ),
        na.rm = FALSE
      )
    )
}

#WHO-5
compute_who5 <- function(data) {
  #Будь ласка, вкажіть для кожного з п'яти тверджень, ту відповідь, яка є найбільш близькою до того, як ви відчували себе протягом останніх двох тижнів. Зверніть увагу, чим вищі цифри, тим краще ваше самопочуття.
  #Наскільки часто протягом минулих двох тижнів Ви почувалися:
  				
  #Q93 Я відчував/відчувала бадьорість і був/була у гарному настрої	
  #Q94 Я відчував/відчувала спокій і розслабленість	
  #Q95 Я почувався/почувалась активним/активною і повним/повною енергії	
  #Q96 Я прокидався/прокидалась свіжим/свіжою і відпочившим/відпочившою	
  #Q97 Моє щоденне життя було наповнене цікавими речами
  #Повсякчас 5
  #Більш як половину часу 4
  #Понад половину часу 3
  #Менш ніж половину часу 2
  #Якийсь час 1
  #Ніколи 0
  
  data %>%
    mutate(
      WHO.5 = rowSums(
        cbind(
          recode(Q93, "Весь час"=5, "Більшість часу"=4, "Більше половини часу"=3, "Менше половини часу"=2, "Певний час"=1, "Ніколи"=0, .default=NA_real_),
          recode(Q94, "Весь час"=5, "Більшість часу"=4, "Більше половини часу"=3, "Менше половини часу"=2, "Певний час"=1, "Ніколи"=0, .default=NA_real_),
          recode(Q95, "Весь час"=5, "Більшість часу"=4, "Більше половини часу"=3, "Менше половини часу"=2, "Певний час"=1, "Ніколи"=0, .default=NA_real_),
          recode(Q96, "Весь час"=5, "Більшість часу"=4, "Більше половини часу"=3, "Менше половини часу"=2, "Певний час"=1, "Ніколи"=0, .default=NA_real_),
          recode(Q97, "Весь час"=5, "Більшість часу"=4, "Більше половини часу"=3, "Менше половини часу"=2, "Певний час"=1, "Ніколи"=0, .default=NA_real_)
        ),
        na.rm = FALSE
      )
    )
}

#WHODAS
compute_whodas <- function(data) {
  # Ця анкета стосується труднощів, пов’язаних зі станом здоров’я. Стан здоров’я включає захворювання або хвороби, інші проблеми зі здоров’ям, які можуть бути короткотривалими або довготривалими, травми, психічні чи емоційні проблеми, а також проблеми, пов'язані з алкоголем або наркотиками. Згадайте останні 30 днів і відповідайте на запитання, думаючи про те, наскільки вам було важко виконувати наступні дії. Для кожного запитання, будь ласка, оберіть лише одну відповідь.
  # Упродовж останніх 30 днів настільки вам було складно?

  #Q100 Виконувати домашню роботу?	
  #Q101 Виконувати більшість робіт по дому якісно?	
  #Q102 Виконувати всю необхідну домашню роботу?	
  #Q103 Виконувати домашню роботу настільки швидко, наскільки це потрібно?	
  #Q104 Виконувати повсякденну діяльність на роботі або на навчанні?	
  #Q105 Виконувати повсякденну діяльність на роботі або на навчанні якісно?	
  #Q106 Виконувати всю необхідну роботу?	
  #Q107 Виконувати всю роботу так швидко, наскільки це необхідно?
  # Зовсім ні
  # Трохи
  # Досить
  # Дуже
  # Неймовірно складно або не можу зробити
  
  data %>%
    mutate(
      WHODAS = rowSums(
        cbind(
          recode(Q100, "Зовсім ні"=0, "Трохи"=1, "Досить"=2, "Дуже"=3, "Неймовірно складно або не можу зробити"=4, .default=NA_real_),
          recode(Q101, "Зовсім ні"=0, "Трохи"=1, "Досить"=2, "Дуже"=3, "Неймовірно складно або не можу зробити"=4, .default=NA_real_),
          recode(Q102, "Зовсім ні"=0, "Трохи"=1, "Досить"=2, "Дуже"=3, "Неймовірно складно або не можу зробити"=4, .default=NA_real_),
          recode(Q103, "Зовсім ні"=0, "Трохи"=1, "Досить"=2, "Дуже"=3, "Неймовірно складно або не можу зробити"=4, .default=NA_real_),
          recode(Q104, "Зовсім ні"=0, "Трохи"=1, "Досить"=2, "Дуже"=3, "Неймовірно складно або не можу зробити"=4, .default=NA_real_),
          recode(Q105, "Зовсім ні"=0, "Трохи"=1, "Досить"=2, "Дуже"=3, "Неймовірно складно або не можу зробити"=4, .default=NA_real_),
          recode(Q106, "Зовсім ні"=0, "Трохи"=1, "Досить"=2, "Дуже"=3, "Неймовірно складно або не можу зробити"=4, .default=NA_real_),
          recode(Q107, "Зовсім ні"=0, "Трохи"=1, "Досить"=2, "Дуже"=3, "Неймовірно складно або не можу зробити"=4, .default=NA_real_)
        ),
        na.rm = FALSE
      )
    )
}

# Обчислюємо індекс PSS-10
t1 <- compute_pss10(t1)
t2 <- compute_pss10(t2)
t3 <- compute_pss10(t3)

# Обчислюємо індекс PHQ-9
t1 <- compute_phq9(t1)
t2 <- compute_phq9(t2)
t3 <- compute_phq9(t3)

# Обчислюємо індекс GAD-7
t1 <- compute_gad7(t1)
t2 <- compute_gad7(t2)
t3 <- compute_gad7(t3)

# Обчислюємо індекс PCL-5
t1 <- compute_pcl5(t1)
t2 <- compute_pcl5(t2)
t3 <- compute_pcl5(t3)

# Обчислюємо індекс CD-RISC-10
t1 <- compute_cdrisk10(t1)
t2 <- compute_cdrisk10(t2)
t3 <- compute_cdrisk10(t3)

# Обчислюємо індекс CRAFFT
t1 <- compute_crafft(t1)
t2 <- compute_crafft(t2)
t3 <- compute_crafft(t3)

# Обчислюємо індекс #WHO-5
t1 <- compute_who5(t1)
t2 <- compute_who5(t2)
t3 <- compute_who5(t3)

# Обчислюємо індекс WHODAS
t1 <- compute_whodas(t1)
t2 <- compute_whodas(t2)
t3 <- compute_whodas(t3)

# Ключові колонки
keys <- c("Q3")

# Додаємо префікси
t1 <- t1 %>% rename_with(~ paste0("T1_", .x), -all_of(keys))
t2 <- t2 %>% rename_with(~ paste0("T2_", .x), -all_of(keys))
t3 <- t3 %>% rename_with(~ paste0("T3_", .x), -all_of(keys))
t0 <- t0 %>% rename_with(~ paste0("T0_", .x), -all_of(keys))

# Об’єднуємо всі таблиці FULL JOIN
merged <- t0 %>%
  full_join(t1, by = keys) %>%
  full_join(t2, by = keys) %>%
  full_join(t3, by = keys)
View(merged)

# 1376 рядків у скринінгу
nrow(t0)
#1376

#T0_stats <- data.frame(
  #N_missing = sum(is.na(t0$T0_PSS.10)),
  #N = sum(!is.na(t0$T0_PSS.10)),
  #Mean = mean(t0$T0_PSS.10, na.rm = TRUE),
  #SD = sd(t0$T0_PSS.10, na.rm = TRUE),
  #Min = min(t0$T0_PSS.10, na.rm = TRUE),
  #Q1 = quantile(t0$T0_PSS.10, 0.25, na.rm = TRUE),
  #Median = median(t0$T0_PSS.10, na.rm = TRUE),
  #Q3 = quantile(t0$T0_PSS.10, 0.75, na.rm = TRUE),
  #Max = max(t0$T0_PSS.10, na.rm = TRUE)
#)
#T0_stats

# 877 рядків з етапу T1
nrow(t1)

# 677 рядків з етапу T2
nrow(t2)

# 619 рядків з етапу T1
nrow(t3)

nrow(merged)
#1547
#write.csv(merged, "merged2.csv", row.names = FALSE)


options(digits=4)


T0_stats <- data.frame(
  N_missing = sum(is.na(merged$T0_PSS.10)),
  N = sum(!is.na(merged$T0_PSS.10)),
  Mean = mean(merged$T0_PSS.10, na.rm = TRUE),
  SD = sd(merged$T0_PSS.10, na.rm = TRUE),
  Min = min(merged$T0_PSS.10, na.rm = TRUE),
  Q1 = quantile(merged$T0_PSS.10, 0.25, na.rm = TRUE),
  Median = median(merged$T0_PSS.10, na.rm = TRUE),
  Q3 = quantile(merged$T0_PSS.10, 0.75, na.rm = TRUE),
  Max = max(merged$T0_PSS.10, na.rm = TRUE)
)

T0_stats
# N_missing    N  Mean    SD Min Q1 Median Q3 Max
# 25%       171 1376 19.04 6.607   0 15     19 24  40


T1_stats <- data.frame(
  N_missing = sum(is.na(merged$T1_PSS.10)),
  N = sum(!is.na(merged$T1_PSS.10)),
  Mean = mean(merged$T1_PSS.10, na.rm = TRUE),
  SD = sd(merged$T1_PSS.10, na.rm = TRUE),
  Min = min(merged$T1_PSS.10, na.rm = TRUE),
  Q1 = quantile(merged$T1_PSS.10, 0.25, na.rm = TRUE),
  Median = median(merged$T1_PSS.10, na.rm = TRUE),
  Q3 = quantile(merged$T1_PSS.10, 0.75, na.rm = TRUE),
  Max = max(merged$T1_PSS.10, na.rm = TRUE)
)
T1_stats
# N_missing   N  Mean   SD Min Q1 Median Q3 Max
# 25%       670 877 18.04 6.23   0 14     18 22  40


merged %>%
  filter(!is.na(merged$T0_PSS.10) & !is.na(merged$T1_PSS.10)) %>%
  nrow()
#763 на Т0,Т1

merged %>%
  filter(!is.na(merged$T0_PSS.10) & !is.na(merged$T1_PSS.10) & !is.na(merged$T2_PSS.10) & !is.na(merged$T3_PSS.10)) %>%
  nrow()
#435 на Т0,Т1,Т2,Т3

merged %>%
  filter(!is.na(merged$T1_PSS.10) & !is.na(merged$T2_PSS.10) & !is.na(merged$T3_PSS.10)) %>%
  nrow()
#482 на Т1,Т2,Т3

merged %>%
  filter(!is.na(merged$T0_PSS.10) & T0_PSS.10 > 26 ) %>%
  nrow()
#184 випадків високого рівня сприйнятого стресу під час скринінгу

merged %>%
  filter(!is.na(merged$T0_PSS.10) & T0_PSS.10 > 26 & !is.na(merged$T1_PSS.10)) %>%
  nrow()
#49 випадків високого рівня сприйнятого стресу під час скринінгу та Т1

merged %>%
  filter(is.na(merged$T0_PSS.10) & ((T1_PSS.10 <= 7) | T1_PSS.10 > 26)) %>%
  nrow()
#15 випадків високого рівня або безсимптомів сприйнятого стресу під час T1 серед тих, кого не було на скринінгу

merged %>%
  filter((!is.na(merged$T0_PSS.10) & T0_PSS.10 > 26) | (!is.na(merged$T0_PSS.10) & T1_PSS.10 > 26)) %>%
  nrow()
# тобто 222 випадків високого стрессу під час Т0 та Т1

merged %>%
  filter(!is.na(merged$T0_PSS.10) & (T0_PSS.10 > 7) & (T0_PSS.10 <= 26) & !is.na(merged$T1_PSS.10) & !is.na(merged$T2_PSS.10) & !is.na(merged$T3_PSS.10)) %>%
  nrow()
#386 хто пройшов скрінінг та всі етапи




# ============================================================
# CONSORT FLOW DIAGRAM
# Схема: Screening → Baseline → Post-intervention → Follow-up
# Джерело: Tol et al. 2020, Acarturk et al. 2022, Purgato et al. 2021
# ============================================================

library(dplyr)
library(DiagrammeR)      # для побудови flow diagram
library(DiagrammeRsvg)   # для експорту в SVG
library(rsvg)            # для конвертації SVG → PNG/PDF

# --- T0: Скринінг ---
n_t0_total      <- sum(!is.na(merged$T0_PSS.10))
n_t0_high       <- sum(!is.na(merged$T0_PSS.10) & merged$T0_PSS.10 > 26)   # високий стрес — виключені
n_t0_low        <- sum(!is.na(merged$T0_PSS.10) & merged$T0_PSS.10 <= 7)   # відсутність стресу — виключені
n_t0_eligible   <- sum(!is.na(merged$T0_PSS.10) & merged$T0_PSS.10 > 7 & merged$T0_PSS.10 <= 26)

# --- T1: Baseline (пройшли скринінг і заповнили T1) ---
n_t1_of_eligible <- merged %>%
  filter(!is.na(T0_PSS.10) & T0_PSS.10 > 7 & T0_PSS.10 <= 26 & !is.na(T1_PSS.10)) %>%
  nrow()

n_t1_lost <- n_t0_eligible - n_t1_of_eligible

# --- T2: Post-intervention ---
n_t2_of_t1 <- merged %>%
  filter(!is.na(T0_PSS.10) & T0_PSS.10 > 7 & T0_PSS.10 <= 26 & 
           !is.na(T1_PSS.10) & !is.na(T2_PSS.10)) %>%
  nrow()

n_t2_lost <- n_t1_of_eligible - n_t2_of_t1

# --- T3: 3-місячний follow-up ---
n_t3_of_t2 <- merged %>%
  filter(!is.na(T0_PSS.10) & T0_PSS.10 > 7 & T0_PSS.10 <= 26 & 
           !is.na(T1_PSS.10) & !is.na(T2_PSS.10) & !is.na(T3_PSS.10)) %>%
  nrow()

n_t3_lost <- n_t2_of_t1 - n_t3_of_t2

# --- Фінальна вибірка для аналізу (= valid_data, яка створиться пізніше) ---
n_analyzed <- n_t3_of_t2 

# Вивід у консоль для перевірки

cat("T0 Скринінг (всього заповнили):      ", n_t0_total, "\n")
cat("  - Виключено (PSS-10 > 26):         ", n_t0_high, "\n")
cat("  - Виключено (PSS-10 <= 7):         ", n_t0_low, "\n")
cat("  - Придатні до участі (8-26):       ", n_t0_eligible, "\n")
cat("T1 Baseline (з придатних):           ", n_t1_of_eligible, 
    " (втрачено:", n_t1_lost, ")\n")
cat("T2 Post-intervention:                ", n_t2_of_t1, 
    " (втрачено:", n_t2_lost, ")\n")
cat("T3 Follow-up (3 місяці):             ", n_t3_of_t2, 
    " (втрачено:", n_t3_lost, ")\n")
cat("АНАЛІЗОВАНА ВИБІРКА (valid_data):    ", n_analyzed, "\n")


consort_diagram <- DiagrammeR::grViz(paste0("
digraph CONSORT {

  # --- Налаштування графу ---
  graph [layout = dot, rankdir = TB, nodesep = 0.4, ranksep = 0.5]
  node  [shape = box, style = 'filled,rounded', fontname = 'Helvetica', 
         fontsize = 11, fillcolor = '#E8F1F8', color = '#2C5F8D', penwidth = 1.5]
  edge  [color = '#2C5F8D', penwidth = 1.2, arrowsize = 0.8]

  # --- Основний потік (вертикальна вісь) ---
  screening [label = 'Скринінг (T0)\\nЗаповнили опитувальник PSS-10\\nN = ", n_t0_total, "',
             fillcolor = '#D4E4F0']

  eligible  [label = 'Придатні до участі\\n(PSS-10: 8-26 балів)\\nN = ", n_t0_eligible, "',
             fillcolor = '#D4E4F0']

  baseline  [label = 'Baseline (T1)\\nЗаповнили до інтервенції\\nN = ", n_t1_of_eligible, "',
             fillcolor = '#B8D4E8']

  post      [label = 'Post-intervention (T2)\\nОдразу після курсу SH+\\nN = ", n_t2_of_t1, "',
             fillcolor = '#B8D4E8']

  followup  [label = 'Follow-up (T3)\\n3 місяці після курсу\\nN = ", n_t3_of_t2, "',
             fillcolor = '#B8D4E8']

  analyzed  [label = 'Аналізована вибірка\\n(complete cases)\\nN = ", n_analyzed, "',
             fillcolor = '#8BBAD9', fontcolor = 'white']

  # --- Вузли виключення/випадання (справа) ---
  excluded  [label = 'Виключено за критеріями:\\n- PSS-10 > 26 (високий стрес): n = ", n_t0_high, "\\n- PSS-10 <= 7 (без стресу): n = ", n_t0_low, "',
             fillcolor = '#FCE8E6', color = '#C04A3E', shape = box]

  lost_t1   [label = 'Не заповнили T1\\nn = ", n_t1_lost, "',
             fillcolor = '#FCE8E6', color = '#C04A3E']

  lost_t2   [label = 'Не заповнили T2\\nn = ", n_t2_lost, "',
             fillcolor = '#FCE8E6', color = '#C04A3E']

  lost_t3   [label = 'Не заповнили T3\\nn = ", n_t3_lost, "',
             fillcolor = '#FCE8E6', color = '#C04A3E']

  # --- Основні зв'язки ---
  screening -> eligible
  eligible  -> baseline
  baseline  -> post
  post      -> followup
  followup  -> analyzed

  # --- Бічні зв'язки (виключення) ---
  screening -> excluded [constraint = false]
  eligible  -> lost_t1  [constraint = false]
  baseline  -> lost_t2  [constraint = false]
  post      -> lost_t3  [constraint = false]

  # --- Вирівнювання по рівнях ---
  { rank = same; screening; excluded }
  { rank = same; eligible;  lost_t1 }
  { rank = same; baseline;  lost_t2 }
  { rank = same; post;      lost_t3 }
}
"))

# Показати у Viewer RStudio
print(consort_diagram)

svg_code <- DiagrammeRsvg::export_svg(consort_diagram)
# Зберегти як PNG (для Word / дипломної)
rsvg::rsvg_png(charToRaw(svg_code), 
               file = "consort_diagram.png", 
               width = 1600, height = 1200)

# Зберегти як PDF (векторний — для друку)
rsvg::rsvg_pdf(charToRaw(svg_code), 
               file = "consort_diagram.pdf")



# гістограма Т0(просто змінювати та подивитись всі)
p0 <- ggplot(merged, aes(x = T0_PSS.10)) +
  geom_histogram(
    binwidth = 1,
    fill = "steelblue",
    color = "white",
    boundary = 0
  ) +
  geom_vline(xintercept = c(7.5, 13.5, 26.5),
             linetype = "dashed",
             color = "tomato") +
  annotate("text", x = 3.5,  y = -3, label = "Без симптомів", color = "darkgreen", size = 4) +
  annotate("text", x = 10.5, y = -3, label = "Легкі",        color = "orange",    size = 4) +
  annotate("text", x = 20,   y = -3, label = "Помірні",      color = "orange",    size = 4) +
  annotate("text", x = 33,   y = -3, label = "Тяжкі",        color = "red",       size = 4) +
  scale_x_continuous(breaks = seq(0, 40, by = 10), limits = c(0, 40)) +
  labs(
    title = "Гістограма значень PSS-10: Скринінг",
    x = "Значення шкали PSS-10",
    y = "Кількість респондентів"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(10, 10, 25, 10)
  ) +
  coord_cartesian(clip = "off")

p1 <- ggplot(merged, aes(x = T1_PSS.10)) +
  geom_histogram(
    binwidth = 1,
    fill = "steelblue",
    color = "white",
    boundary = 0
  ) +
  geom_vline(xintercept = c(7.5, 13.5, 26.5),
             linetype = "dashed",
             color = "tomato") +
  annotate("text", x = 3.5,  y = -3, label = "Без симптомів", color = "darkgreen", size = 4) +
  annotate("text", x = 10.5, y = -3, label = "Легкі",        color = "orange",    size = 4) +
  annotate("text", x = 20,   y = -3, label = "Помірні",      color = "orange",    size = 4) +
  annotate("text", x = 33,   y = -3, label = "Тяжкі",        color = "red",       size = 4) +
  scale_x_continuous(breaks = seq(0, 40, by = 10), limits = c(0, 40)) +
  labs(
    title = "Гістограма значень PSS-10: Т1",
    x = "Значення шкали PSS-10",
    y = "Кількість респондентів"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(10, 10, 25, 10)
  ) +
  coord_cartesian(clip = "off")

p2 <- ggplot(merged, aes(x = T2_PSS.10)) +
  geom_histogram(
    binwidth = 1,
    fill = "steelblue",
    color = "white",
    boundary = 0
  ) +
  geom_vline(xintercept = c(7.5, 13.5, 26.5),
             linetype = "dashed",
             color = "tomato") +
  annotate("text", x = 3.5,  y = -3, label = "Без симптомів", color = "darkgreen", size = 4) +
  annotate("text", x = 10.5, y = -3, label = "Легкі",        color = "orange",    size = 4) +
  annotate("text", x = 20,   y = -3, label = "Помірні",      color = "orange",    size = 4) +
  annotate("text", x = 33,   y = -3, label = "Тяжкі",        color = "red",       size = 4) +
  scale_x_continuous(breaks = seq(0, 40, by = 10), limits = c(0, 40)) +
  labs(
    title = "Гістограма значень PSS-10: Т2",
    x = "Значення шкали PSS-10",
    y = "Кількість респондентів"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(10, 10, 25, 10)
  ) +
  coord_cartesian(clip = "off")

p3 <- ggplot(merged, aes(x = T3_PSS.10)) +
  geom_histogram(
    binwidth = 1,
    fill = "steelblue",
    color = "white",
    boundary = 0
  ) +
  geom_vline(xintercept = c(7.5, 13.5, 26.5),
             linetype = "dashed",
             color = "tomato") +
  annotate("text", x = 3.5,  y = -3, label = "Без симптомів", color = "darkgreen", size = 4) +
  annotate("text", x = 10.5, y = -3, label = "Легкі",        color = "orange",    size = 4) +
  annotate("text", x = 20,   y = -3, label = "Помірні",      color = "orange",    size = 4) +
  annotate("text", x = 33,   y = -3, label = "Тяжкі",        color = "red",       size = 4) +
  scale_x_continuous(breaks = seq(0, 40, by = 10), limits = c(0, 40)) +
  labs(
    title = "Гістограма значень PSS-10: Т3",
    x = "Значення шкали PSS-10",
    y = "Кількість респондентів"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(10, 10, 25, 10)
  ) +
  coord_cartesian(clip = "off")

# Зберегти
ggsave("hist_pss_T0.png", plot = p0, width = 8, height = 6, dpi = 300)
ggsave("hist_pss_T1.png", plot = p1, width = 8, height = 6, dpi = 300)
ggsave("hist_pss_T2.png", plot = p2, width = 8, height = 6, dpi = 300)
ggsave("hist_pss_T3.png", plot = p3, width = 8, height = 6, dpi = 300)



#кількість значень яку відкидає гістограма
merged %>%
  summarise(
    n_na = sum(is.na(T0_PSS.10)),
    below_0 = sum(T0_PSS.10 < 0, na.rm = TRUE),
    above_40 = sum(T0_PSS.10 > 40, na.rm = TRUE)
  )


# ============================================================
# ATTRITION ANALYSIS
# ============================================================
# Питання: чи completers (хто дійшов до Т3) систематично 
# відрізняються від dropouts (хто випав) за характеристиками
# на baseline? Якщо так — це привід для обмежень дослідження.
#
# Вибірка: ті, хто пройшов скринінг (PSS-10 8-26) і має Т1.
# Completer = має Т3, Dropout = не має Т3.
# ============================================================

library(dplyr)
library(rstatix)

# Беремо всіх, хто мав T1 і пройшов критерії скринінгу
attrition_data <- merged %>%
  filter(!is.na(T0_PSS.10),
         T0_PSS.10 >= 8, T0_PSS.10 <= 26,
         !is.na(T1_PSS.10)) %>%
  mutate(group = ifelse(!is.na(T2_PSS.10) & !is.na(T3_PSS.10), 
                        "Completer", "Dropout"))

# Перетворюємо вік на число 
attrition_data$T1_Q4 <- as.numeric(attrition_data$T1_Q4)

#Розмір вибірки
cat("Загалом:    ", nrow(attrition_data), "\n")
cat("Completers: ", sum(attrition_data$group == "Completer"), 
    sprintf("(%.1f%%)\n", 100 * mean(attrition_data$group == "Completer")))
cat("Dropouts:   ", sum(attrition_data$group == "Dropout"),
    sprintf("(%.1f%%)\n", 100 * mean(attrition_data$group == "Dropout")))


# Wilcoxon (бо дані не нормальні) -------
numeric_vars <- c("T1_Q4",          # вік
                  "T0_PSS.10",      # baseline стрес на скринінгу
                  "T1_PSS.10", "T1_PHQ.9", "T1_GAD.7", "T1_PCL.5",
                  "T1_CD.RISC.10", "T1_WHO.5", "T1_WHODAS")

attrition_numeric <- lapply(numeric_vars, function(v) {
  d <- attrition_data %>% 
    select(group, value = all_of(v)) %>%
    filter(!is.na(value))
  
  med_comp <- d %>% filter(group == "Completer") %>% pull(value) %>% median()
  med_drop <- d %>% filter(group == "Dropout") %>% pull(value) %>% median()
  
  test <- wilcox.test(value ~ group, data = d, exact = FALSE)
  
  data.frame(
    Variable    = v,
    Completer   = sprintf("%.1f", med_comp),
    Dropout     = sprintf("%.1f", med_drop),
    p_value     = round(test$p.value, 4)
  )
}) %>% bind_rows()

#Категоріальні змінні: chi-square 
categorical_vars <- c("T1_University",  # університет
                      "T1_Q5",          # стать
                      "T1_Q9",          # ВПО
                      "T1_Q11",         # розлучення через війну
                      "T1_Q12",         # родич у ЗСУ
                      "T1_Q16")         # курс

attrition_categorical <- lapply(categorical_vars, function(v) {
  d <- attrition_data %>% 
    select(group, value = all_of(v)) %>%
    filter(!is.na(value))
  
  tab <- table(d$value, d$group)
  
  # Якщо є комірки з малою кількістю — Fisher замість chi-square
  test <- if (any(suppressWarnings(chisq.test(tab))$expected < 5)) {
    list(p.value = fisher.test(tab, simulate.p.value = TRUE)$p.value, 
         method = "Fisher")
  } else {
    list(p.value = chisq.test(tab)$p.value, method = "Chi-square")
  }
  
  # % completers vs dropouts по групах
  pct_comp <- prop.table(tab, margin = 2)[, "Completer"] * 100
  pct_drop <- prop.table(tab, margin = 2)[, "Dropout"] * 100
  
  comp_str <- paste0(rownames(tab), ": ", round(pct_comp, 1), "%", collapse = " | ")
  drop_str <- paste0(rownames(tab), ": ", round(pct_drop, 1), "%", collapse = " | ")
  
  data.frame(
    Variable    = v,
    Completer   = comp_str,
    Dropout     = drop_str,
    p_value     = round(test$p.value, 4)
  )
}) %>% bind_rows()

#Зведена таблиця
attrition_table2 <- bind_rows(attrition_numeric, attrition_categorical) %>%
  mutate(
    Significance = case_when(
      is.na(p_value)   ~ "",
      p_value < 0.001  ~ "***",
      p_value < 0.01   ~ "**",
      p_value < 0.05   ~ "*",
      TRUE             ~ ""
    )
  )

print(attrition_table2)


#чи є систематичні відмінності?
sig_vars <- attrition_table2 %>% filter(p_value < 0.05) %>% pull(Variable)

if (length(sig_vars) == 0) {
  cat("ВИСНОВОК: Completers і Dropouts НЕ відрізняються значущо\n")
  cat("   Аналізована вибірка (N=386) репрезентативна.\n")
} else {
  cat("ВИСНОВОК: Виявлені значущі відмінності за змінними:\n")
  cat("   ", paste(sig_vars, collapse = ", "), "\n")
  cat("   Це треба чесно описати в розділі 'Обмеження'.\n")
}



# # Чищення даних
# {
# #171 рядків у таблиці, де є пропущені значення у стовпці T0_PSS.10 
# noscr <- merged %>%
#   filter(is.na(T0_PSS.10))
# write.csv(noscr, "noscr.csv", row.names = FALSE)
# nrow(noscr)
# 
# colnames(noscr)
# write.csv(noscr[c("Q3","T0_RecipientEmail", "T1_RecipientEmail", "T2_RecipientEmail", "T3_RecipientEmail", "T1_University", "T2_University", "T3_University")], "not0-emails.csv", row.names = FALSE)
# 
# not1yest2 <- merged %>%
#   filter(!is.na(T0_PSS.10) & is.na(T1_PSS.10) & !is.na(T2_PSS.10))
# write.csv(not1yest2[c("Q3","T0_RecipientEmail", "T1_RecipientEmail", "T2_RecipientEmail", "T3_RecipientEmail", "T1_University", "T2_University", "T3_University")], "not1yest2.csv", row.names = FALSE)
# 
# yest3 <- merged %>%
#   filter(is.na(T0_PSS.10) & is.na(T1_PSS.10) & is.na(T2_PSS.10) & !is.na(T3_PSS.10))
# write.csv(yest3[c("Q3","T0_RecipientEmail", "T1_RecipientEmail", "T2_RecipientEmail", "T3_RecipientEmail", "T1_University", "T2_University", "T3_University")], "yest3.csv", row.names = FALSE)
# 
# yest2 <- merged %>%
#   filter(is.na(T0_PSS.10) & is.na(T1_PSS.10) & !is.na(T2_PSS.10))
# write.csv(yest2[c("Q3","T0_RecipientEmail", "T1_RecipientEmail", "T2_RecipientEmail", "T3_RecipientEmail", "T1_University", "T2_University", "T3_University")], "yest2.csv", row.names = FALSE)
# 
# yest2t3 <- merged %>%
#   filter(is.na(T0_PSS.10) & is.na(T1_PSS.10)  & !is.na(T2_PSS.10) & !is.na(T3_PSS.10))
# write.csv(yest2t3[c("Q3","T0_RecipientEmail", "T1_RecipientEmail", "T2_RecipientEmail", "T3_RecipientEmail", "T1_University", "T2_University", "T3_University")], "yest23.csv", row.names = FALSE)
# 
# # підрахувати скільки рядків у таблиці мають усі 4 значення, а скільки — лише в 1, 2 або 3 стовпцях.
# 
# # Вибір потрібних стовпців
# cols <- c("T0_PSS.10", "T1_PSS.10", "T2_PSS.10", "T3_PSS.10")
# 
# # Логічна матриця наявності даних
# m <- !is.na(merged[cols])
# 
# # Перетворимо логічну матрицю у підписи (наприклад, "T0_T1" або "T1_T3_T0")
# pattern <- apply(m, 1, function(x) {
#   if (sum(x) == 0) return("none")
#   paste(cols[x], collapse = "_")
# })
# 
# # Підрахунок кількості рядків для кожної комбінації
# tab <- sort(table(pattern), decreasing = TRUE)
# tab
# 
# 
# # Підрахунок кількості рядків у кожній комбінації для кожного університету
# 
# # Вибір лише потрібних стовпців
# vars <- c("T0_PSS.10", "T1_PSS.10", "T2_PSS.10", "T3_PSS.10")
# 
# # Створюємо колонку з комбінацією наявних вимірів
# noscr_comb <- merged %>%
#   mutate(
#     combo = apply(select(., all_of(vars)),
#                   1,
#                   function(x) {
#                     present <- names(x)[!is.na(x)]
#                     if(length(present) == 0) "NONE"
#                     else paste(present, collapse = "_")
#                   })
#   )
# 
# # Підрахунок кількості рядків у кожній комбінації для кожного університету
# result_by_univ <- noscr_comb %>%
#   group_by(T1_University, combo) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   arrange(T1_University, desc(n))
# 
# write.csv(result_by_univ, "combo_by_university_t1.csv", row.names = FALSE)
# 
# # Підрахунок кількості рядків у кожній комбінації для кожного університету
# result_by_univ <- noscr_comb %>%
#   group_by(T2_University, combo) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   arrange(T2_University, desc(n))
# 
# write.csv(result_by_univ, "combo_by_university_t2.csv", row.names = FALSE)
# 
# # Підрахунок кількості рядків у кожній комбінації для кожного університету
# result_by_univ <- noscr_comb %>%
#   group_by(T3_University, combo) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   arrange(T3_University, desc(n))
# 
# write.csv(result_by_univ, "combo_by_university_t3.csv", row.names = FALSE)
# 
# # Підрахунок кількості рядків у кожній комбінації для кожного університету
# result_by_univ <- noscr_comb %>%
#   group_by(T0_University, combo) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   arrange(T0_University, desc(n))
# 
# write.csv(result_by_univ, "combo_by_university_t0.csv", row.names = FALSE)
# 
# # Підрахунок кількості рядків у кожній комбінації для кожного університету
# result_by_univ <- noscr_comb %>%
#   group_by(T1_University, combo) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   arrange(T1_University, desc(n))
# 
# # Переглянути результат
# print(result_by_univ)
# 
# write.csv(result_by_univ, "combo_by_university_t1.csv", row.names = FALSE)
# }




library(Hmisc)
library(corrplot)


# Відфільтрувати рядки, які мають індекс T0_PSS.10 > 26 чи < 8
valid_data <- merged %>%
  filter(!is.na(merged$T0_PSS.10) & (T0_PSS.10 > 7) & (T0_PSS.10 <= 26) & !is.na(merged$T1_PSS.10) 
         & !is.na(merged$T2_PSS.10) & !is.na(merged$T3_PSS.10))
nrow(valid_data)
#386

View(valid_data)



# ============================================================
# TABLE — Соціально-демографічні та клінічні характеристики
#           аналізованої вибірки (N = 386) за університетами
# ============================================================
library(gtsummary)
library(dplyr)

valid_data$T1_Q4 <- as.numeric(valid_data$T1_Q4)

valid_data$T1_Q16 <- factor(valid_data$T1_Q16,
                            levels = c("1 курс Бакалавр", "2 курс Бакалавр", "3 курс Бакалавр",
                                       "4 курс Бакалавр", "5 курс Магістр",  "6 курс Магістр",
                                       "Аспірантура"))

df_for_table <- valid_data %>%
  select(
    `Вік, роки`                              = T1_Q4,
    `Стать`                                  = T1_Q5,
    `Курс навчання`                          = T1_Q16,
    `ВПО-статус`                             = T1_Q9,
    `Розлучення з сім'єю через війну`        = T1_Q11,
    `Родич у ЗСУ`                            = T1_Q12,
    `PSS-10 (сприйнятий стрес)`              = T1_PSS.10,
    `PHQ-9 (депресія)`                       = T1_PHQ.9,
    `GAD-7 (тривожність)`                    = T1_GAD.7,
    `PCL-5 (ПТСР)`                           = T1_PCL.5,
    `CD-RISC-10 (резильєнтність)`            = T1_CD.RISC.10,
    `WHO-5 (благополуччя)`                   = T1_WHO.5,
    `WHODAS (функціонування)`                = T1_WHODAS,
    Університет                              = T1_University
  )

table1_gt <- df_for_table %>%
  tbl_summary(
    by = Університет,                          # розбивка по університетах
    missing = "no",                             # не показувати рядок missing
    statistic = list(
      all_continuous()  ~ "{median} [{p25}, {p75}]",   # числові: Med [IQR]
      all_categorical() ~ "{n} ({p}%)"                  # категоріальні: N (%)
    ),
    digits = list(
      all_continuous()  ~ 1,
      all_categorical() ~ c(0, 1)
    )
  ) %>%
  add_overall(last = FALSE) %>%                 # колонка "Overall" зліва
  add_p(
    test = list(
      all_continuous()  ~ "kruskal.test",       # KW для числових
      all_categorical() ~ "chisq.test"          # Фішер для категоріальних
    ),
    pvalue_fun = ~style_pvalue(., digits = 3)
  ) %>%
  modify_header(
    label     = "**Характеристика**",
    stat_0    = "**Overall**, N = {N}",
    all_stat_cols() ~ "**{level}**, N = {n}",
    p.value   = "***p***"
  ) %>%
  modify_caption(
    "**Таблиця 1.** Соціально-демографічні та клінічні характеристики 
    аналізованої вибірки за університетами (N = 386)"
  ) %>%
  bold_labels()

table1_gt %>%
  as_kable_extra(format = "latex", booktabs = TRUE) %>%
  cat(file = "table1.tex")

# table1_gt %>% as_gt() %>% gt::gtsave("table1.png")

# ============================================================
# ДОДАТКОВА ТАБЛИЦЯ: Географічний розподіл студентів
#   Кожен рядок = університет
#   Показано: регіон ВНЗ, кількість у «рідній» області,
#             % залишилось, хто переїхав
# ============================================================

library(dplyr)

univ_to_oblast <- c(
  "Вінниця"   = "Вінницька область",
  "Київ"      = "Київська область",
  "Одеса"     = "Одеська область",
  "Тернопіль" = "Тернопільська область",
  "Харків"    = "Харківська область"
)

# Функція для формування рядка "інші області"
format_others <- function(df, home_oblast) {
  others <- df %>%
    filter(T1_Q6 != home_oblast, !is.na(T1_Q6)) %>%
    count(T1_Q6, sort = TRUE) %>%
    mutate(label = paste0(gsub(" область", "", T1_Q6), " (", n, ")"))
  
  if (nrow(others) == 0) return("—")
  paste(others$label, collapse = ", ")
}

universities <- unique(valid_data$T1_University)
universities <- universities[!is.na(universities)]

geo_list <- lapply(universities, function(uni) {
  df_uni <- valid_data %>%
    filter(T1_University == uni, !is.na(T1_Q6))
  
  home <- univ_to_oblast[as.character(uni)]
  N_total <- nrow(df_uni)
  N_home  <- sum(df_uni$T1_Q6 == home)
  
  data.frame(
    Університет                  = paste0(uni, " (N=", N_total, ")"),
    `Більшість живе в`           = paste0(home, " (", N_home, ")"),
    `% залишилось`               = paste0(round(100 * N_home / N_total, 0), "%"),
    `Переїхали з інших областей` = format_others(df_uni, home),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
})

geo_table <- do.call(rbind, geo_list)

print(geo_table, row.names = FALSE)

data_for_pct <- valid_data %>%
  filter(!is.na(T1_University), !is.na(T1_Q6))

n_relocated <- sum(
  data_for_pct$T1_Q6 != univ_to_oblast[as.character(data_for_pct$T1_University)]
)
n_total <- nrow(data_for_pct)

cat(sprintf(
  "\n %d з %d студентів (%.1f%%) проживають поза регіоном свого ВНЗ\n\n",
  n_relocated, n_total, 100 * n_relocated / n_total
))


# ============================================================
# ПЕРЕВІРКА: чи ВПО-статус (T1_Q9) збігається з фактом 
#            переміщення (область проживання ≠ регіон ВНЗ)?
# ============================================================
# Логіка:
#   relocated = TRUE, якщо T1_Q6 (область) ≠ регіон ВНЗ
#   ВПО       = T1_Q9 ("Так"/"Ні")
# ============================================================

# --- 1. Створюємо допоміжну колонку "relocated" ---------------
valid_data$relocated <- ifelse(
  is.na(valid_data$T1_Q6) | is.na(valid_data$T1_University),
  NA,
  ifelse(valid_data$T1_Q6 != univ_to_oblast[as.character(valid_data$T1_University)],
         "Переїхав", "Не переїхав")
)

# --- 2. Хрест-табуляція: ВПО × Переміщення --------------------
cross_tab <- table(
  "Переміщення" = valid_data$relocated,
  "ВПО-статус"  = valid_data$T1_Q9,
  useNA = "ifany"
)

print(cross_tab)

# --- 3. Статистичний тест залежності --------------------------
# χ² (або Fisher, якщо частоти малі)
chi_result <- tryCatch(
  chisq.test(cross_tab),
  warning = function(w) fisher.test(cross_tab)
)
print(chi_result)

# --- 4. Cohen's kappa як міра узгодженості --------------------
# Показує, наскільки дві змінні "збігаються" як класифікатори
# κ = 0   → випадкова узгодженість
# κ < 0.2 → слабка
# κ 0.2–0.4 → середня
# κ 0.4–0.6 → помірна
# κ 0.6–0.8 → сильна
# κ > 0.8   → майже ідентичні
if (requireNamespace("psych", quietly = TRUE)) {
  library(psych)
  
  # Приводимо до числа: 1 = "є ознака", 0 = "немає"
  x1 <- ifelse(valid_data$relocated == "Переїхав", 1, 0)
  x2 <- ifelse(valid_data$T1_Q9 == "Так", 1, 0)
  
  kappa_res <- psych::cohen.kappa(cbind(x1, x2))
  cat("\n=== Cohen's kappa (узгодженість ВПО ↔ Переміщення) ===\n")
  print(kappa_res)
}

# --- 5. Деталізація: хто саме не збігається -------------------
n_reloc_vpo    <- sum(valid_data$relocated == "Переїхав"     & valid_data$T1_Q9 == "Так", na.rm = TRUE)
n_reloc_novpo  <- sum(valid_data$relocated == "Переїхав"     & valid_data$T1_Q9 == "Ні",  na.rm = TRUE)
n_home_vpo     <- sum(valid_data$relocated == "Не переїхав"  & valid_data$T1_Q9 == "Так", na.rm = TRUE)
n_home_novpo   <- sum(valid_data$relocated == "Не переїхав"  & valid_data$T1_Q9 == "Ні",  na.rm = TRUE)
n_total        <- sum(!is.na(valid_data$relocated) & !is.na(valid_data$T1_Q9))

cat(sprintf("✅ Переїхав + має ВПО:       %d (%.1f%%) — логічно\n",
            n_reloc_vpo,   100 * n_reloc_vpo   / n_total))
cat(sprintf("⚠️  Переїхав + немає ВПО:    %d (%.1f%%) — переїхав, але не оформив\n",
            n_reloc_novpo, 100 * n_reloc_novpo / n_total))
cat(sprintf("⚠️  Не переїхав + має ВПО:   %d (%.1f%%) — можливо переїхав з іншого району області\n",
            n_home_vpo,    100 * n_home_vpo    / n_total))
cat(sprintf("✅ Не переїхав + немає ВПО:  %d (%.1f%%) — залишився вдома\n",
            n_home_novpo,  100 * n_home_novpo  / n_total))
cat(sprintf("\nУсього проаналізовано: %d учасників\n", n_total))


# --- 8. Таблиця переміщення за університетом × ВПО ------------
# Ще цікавіший розріз: чи відрізняються університети за тим,
# наскільки ВПО-статус "збігається" з переміщенням?
univ_breakdown <- valid_data %>%
  filter(!is.na(relocated), !is.na(T1_Q9)) %>%
  group_by(T1_University) %>%
  summarise(
    N            = n(),
    Переїхали    = sum(relocated == "Переїхав"),
    `ВПО (Так)`  = sum(T1_Q9 == "Так"),
    `Переїхали І ВПО` = sum(relocated == "Переїхав" & T1_Q9 == "Так"),
    `Переїхали але НЕ ВПО` = sum(relocated == "Переїхав" & T1_Q9 == "Ні"),
    `НЕ переїхали але ВПО` = sum(relocated == "Не переїхав" & T1_Q9 == "Так"),
    .groups = "drop"
  )
print(univ_breakdown)


# # Подивитись 10 випадків з кожної категорії
# library(dplyr)
# 
# cat("\n=== ✅ Переїхав + ВПО (16 осіб) — приклади ===\n")
# valid_data %>%
#   filter(relocated == "Переїхав" & T1_Q9 == "Так") %>%
#   select(T1_University, T1_Q6, T1_Q9) %>%
#   head(10) %>%
#   print()
# 
# cat("\n=== ⚠️ Переїхав + НЕМАЄ ВПО (46 осіб) — приклади ===\n")
# valid_data %>%
#   filter(relocated == "Переїхав" & T1_Q9 == "Ні") %>%
#   select(T1_University, T1_Q6, T1_Q9) %>%
#   head(10) %>%
#   print()
# 
# cat("\n=== ⚠️ НЕ переїхав + ВПО (45 осіб) — приклади ===\n")
# valid_data %>%
#   filter(relocated == "Не переїхав" & T1_Q9 == "Так") %>%
#   select(T1_University, T1_Q6, T1_Q9) %>%
#   head(10) %>%
#   print()




calculate_stats <- function(valid_data, prefix, suffix = "PSS.10") {
  # Формуємо повну назву змінної
  var_name <- paste0(prefix, "_", suffix)
  
  # Перевіряємо, чи така змінна існує в даних
  if (!var_name %in% names(valid_data)) {
    stop(paste("Змінна", var_name, "не знайдена в даних."))
  }
  
  # Витягуємо змінну
  variable <- valid_data[[var_name]]
  
  # Обчислюємо статистику
  stats <- data.frame(
    Variable = var_name,
    N_missing = sum(is.na(variable)),
    N = sum(!is.na(variable)),
    Mean = mean(variable, na.rm = TRUE),
    SD = sd(variable, na.rm = TRUE),
    Min = min(variable, na.rm = TRUE),
    Q1 = quantile(variable, 0.25, na.rm = TRUE),
    Median = median(variable, na.rm = TRUE),
    Q3 = quantile(variable, 0.75, na.rm = TRUE),
    Max = max(variable, na.rm = TRUE)
  )
  
  return(stats)
}

options(digits = 4)
calculate_stats(valid_data, "T0")
calculate_stats(valid_data, "T1")
calculate_stats(valid_data, "T2")
calculate_stats(valid_data, "T3")



library(dplyr)
library(ggplot2)
library(patchwork)  # для об'єднання графіків

# ============================================================
# ПЕРЕВІРКА НОРМАЛЬНОСТІ — valid_data
# valid_data вже відфільтрована:
#   - T0_PSS.10 від 8 до 26 (легкий і середній рівень)
#   - присутні всі 4 часові точки (T0, T1, T2, T3)
#   - N = 386
# ============================================================

# --- Всі шкали та часові точки ---
scales_to_test <- list(
  # PSS-10
  "PSS-10 T0"      = valid_data$T0_PSS.10,
  "PSS-10 T1"      = valid_data$T1_PSS.10,
  "PSS-10 T2"      = valid_data$T2_PSS.10,
  "PSS-10 T3"      = valid_data$T3_PSS.10,
  # PHQ-9
  "PHQ-9 T1"       = valid_data$T1_PHQ.9,
  "PHQ-9 T2"       = valid_data$T2_PHQ.9,
  "PHQ-9 T3"       = valid_data$T3_PHQ.9,
  # GAD-7
  "GAD-7 T1"       = valid_data$T1_GAD.7,
  "GAD-7 T2"       = valid_data$T2_GAD.7,
  "GAD-7 T3"       = valid_data$T3_GAD.7,
  # PCL-5
  "PCL-5 T1"       = valid_data$T1_PCL.5,
  "PCL-5 T2"       = valid_data$T2_PCL.5,
  "PCL-5 T3"       = valid_data$T3_PCL.5,
  # CD-RISC-10
  "CD-RISC-10 T1"  = valid_data$T1_CD.RISC.10,
  "CD-RISC-10 T2"  = valid_data$T2_CD.RISC.10,
  "CD-RISC-10 T3"  = valid_data$T3_CD.RISC.10,
  # WHO-5
  "WHO-5 T1"       = valid_data$T1_WHO.5,
  "WHO-5 T2"       = valid_data$T2_WHO.5,
  "WHO-5 T3"       = valid_data$T3_WHO.5,
  # WHODAS
  "WHODAS T1"      = valid_data$T1_WHODAS,
  "WHODAS T2"      = valid_data$T2_WHODAS,
  "WHODAS T3"      = valid_data$T3_WHODAS,
  # CRAFFT
  "CRAFFT T1"      = valid_data$T1_CRAFFT,
  "CRAFFT T2"      = valid_data$T2_CRAFFT,
  "CRAFFT T3"      = valid_data$T3_CRAFFT
)

# ============================================================
# 1. ТАБЛИЦЯ SHAPIRO-WILK
# ============================================================
normality_results <- data.frame(
  Scale    = names(scales_to_test),
  N        = NA_integer_,
  W        = NA_real_,
  p_value  = NA_real_,
  Normal   = NA_character_,
  stringsAsFactors = FALSE
)

for (i in seq_along(scales_to_test)) {
  x <- na.omit(scales_to_test[[i]])
  normality_results$N[i] <- length(x)
  
  if (length(x) >= 3 && length(x) <= 5000) {
    test <- shapiro.test(x)
    normality_results$W[i]       <- round(test$statistic, 4)
    normality_results$p_value[i] <- round(test$p.value, 4)
    normality_results$Normal[i]  <- ifelse(test$p.value > 0.05, "Так ✓", "Ні ✗")
  }
}

print(normality_results)

# ============================================================
# 2. ГІСТОГРАМИ + Q-Q PLOTS для кожної шкали
#    Зберігаємо окремий PDF або PNG для кожної групи шкал
# ============================================================

plot_normality <- function(data_vec, label) {
  df <- data.frame(x = na.omit(data_vec))
  # Гістограма
  p1 <- ggplot(df, aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 20,
                   fill = "steelblue", color = "white", alpha = 0.8) +
    stat_function(fun = dnorm,
                  args = list(mean = mean(df$x), sd = sd(df$x)),
                  color = "tomato", linewidth = 1) +
    labs(title = label,
         x = "Значення", y = "Щільність") +
    theme_minimal(base_size = 11) +
    theme(plot.title    = element_text(face = "bold"),
          plot.subtitle = element_text(size = 9, color = "gray40"))
  
  # Q-Q plot
  p2 <- ggplot(df, aes(sample = x)) +
    stat_qq(color = "steelblue", alpha = 0.6) +
    stat_qq_line(color = "tomato", linewidth = 1) +
    labs(title = paste("Q-Q:", label),
         x = "Теоретичні квантилі", y = "Вибіркові квантилі") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
  
  p1 + p2  # patchwork об'єднує side-by-side
}

# ============================================================
# Зберігаємо графіки — по одному файлу на кожну шкалу
# ============================================================

scale_groups <- list(
  PSS10    = c("PSS-10 T0",     "PSS-10 T1",     "PSS-10 T2",     "PSS-10 T3"),
  PHQ9     = c("PHQ-9 T1",      "PHQ-9 T2",      "PHQ-9 T3"),
  GAD7     = c("GAD-7 T1",      "GAD-7 T2",      "GAD-7 T3"),
  PCL5     = c("PCL-5 T1",      "PCL-5 T2",      "PCL-5 T3"),
  CDRISC   = c("CD-RISC-10 T1", "CD-RISC-10 T2", "CD-RISC-10 T3"),
  WHO5     = c("WHO-5 T1",      "WHO-5 T2",      "WHO-5 T3"),
  WHODAS   = c("WHODAS T1",     "WHODAS T2",     "WHODAS T3"),
  CRAFFT   = c("CRAFFT T1",     "CRAFFT T2",     "CRAFFT T3")
)

for (group_name in names(scale_groups)) {
  keys <- scale_groups[[group_name]]
  
  # Збираємо всі plots для групи
  plots_list <- lapply(keys, function(k) {
    plot_normality(scales_to_test[[k]], k)
  })
  
  # Об'єднуємо вертикально через patchwork
  combined <- wrap_plots(plots_list, ncol = 1)
  
  filename <- paste0("normality_", group_name, ".png")
  ggsave(filename,
         plot   = combined,
         width  = 12,
         height = 5 * length(keys),  # 5 дюймів на кожну шкалу
         dpi    = 150,
         bg     = "white")
  
  message("Збережено: ", filename)
}






library(dplyr)
library(tidyr)
library(ggplot2)

plot_boxplot <- function(valid_data, suffix = "PSS.10", ylim = c(0, 40)) {
  # Формуємо список змінних, які потрібно вибрати
  timepoints <- paste0("T", 0:3, "_", suffix)
  
  # Перевірка наявності змінних у даних
  missing_vars <- setdiff(timepoints, names(valid_data))
  if (length(missing_vars) > 0) {
    stop(paste("Відсутні змінні:", paste(missing_vars, collapse = ", ")))
  }
  
  # Перетворення в long-формат
  valid_data_long <- valid_data %>%
    select(all_of(timepoints)) %>%
    pivot_longer(cols = everything(),
                 names_to = "Time",
                 values_to = "Score") %>%
    mutate(Time = recode(Time,
                         !!timepoints[1] := "Скринінг",
                         !!timepoints[2] := "T1",
                         !!timepoints[3] := "T2",
                         !!timepoints[4] := "T3"),
           Time = factor(Time, levels = c("Скринінг", "T1", "T2", "T3")))
  
  # Побудова графіка
  ggplot(valid_data_long, aes(x = Time, y = Score)) +
    geom_boxplot(width = 0.3, outlier.shape = "*", fill = "skyblue") +
    theme_minimal(base_size = 14) +
    labs(x = "Етап",
         y = paste(suffix, "")) +
    coord_cartesian(ylim = ylim)
}


plot_boxplot(valid_data,"PSS.10")
ggsave("boxplot_PSS10.png",plot = plot_boxplot(valid_data,"PSS.10"),
  width = 6,height = 5,dpi = 300)


run_friedman_test <- function(valid_data, suffix = "PSS.10", set=0:3) {
  # Формуємо список змінних
  timepoints <- paste0("T", set, "_", suffix)
  
  # Перевірка наявності змінних
  missing_vars <- setdiff(timepoints, names(valid_data))
  if (length(missing_vars) > 0) {
    stop(paste("Відсутні змінні:", paste(missing_vars, collapse = ", ")))
  }
  
  # Витягуємо дані
  data_wide <- valid_data[timepoints]
  
  # Видаляємо рядки з NA
  data_complete <- na.omit(data_wide)
  
  # Перетворюємо в матрицю
  data_matrix <- as.matrix(data_complete)
  
  # Тест Фрідмана
  test_result <- friedman.test(data_matrix)
  
  return(test_result)
}

library(FSA)
run_dunn_posthoc <- function(valid_data, suffix = "PSS.10", set=0:3, method = "bonferroni") {
  # Потрібні пакети
  if (!requireNamespace("FSA", quietly = TRUE)) {
    install.packages("FSA")
  }

  # Формуємо список змінних
  timepoints <- paste0("T", set, "_", suffix)
  
  # Перевірка наявності змінних
  missing_vars <- setdiff(timepoints, names(valid_data))
  if (length(missing_vars) > 0) {
    stop(paste("Відсутні змінні:", paste(missing_vars, collapse = ", ")))
  }
  
  # Підготовка даних
  data_wide <- valid_data[timepoints]
  data_complete <- na.omit(data_wide)
  data_complete$id <- seq_len(nrow(data_complete))  # додаємо ідентифікатор
  
  # Перетворення в long-формат
  data_long <- reshape(data_complete,
                       varying = timepoints,
                       v.names = "Score",
                       timevar = "Time",
                       times = c("Скринінг", "T1", "T2", "T3"),
                       direction = "long")
  
  # Тест Данна
  dunn_result <- dunnTest(Score ~ Time, data = data_long, method = method)
  
  return(dunn_result)
}

options(digits =5)

run_friedman_test(valid_data,"PSS.10")
run_dunn_posthoc(valid_data, suffix = "PSS.10")



options(digits = 4)

#########################################################  PHQ.9

calculate_stats(valid_data, "T1", "PHQ.9")
calculate_stats(valid_data, "T2", "PHQ.9")
calculate_stats(valid_data, "T3", "PHQ.9")

plot_boxplot_t1_t3 <- function(valid_data, suffix = "PSS.10", ylim = c(0, 40)) {
  # Формуємо список змінних, які потрібно вибрати
  timepoints <- paste0("T", 1:3, "_", suffix)
  
  # Перевірка наявності змінних у даних
  missing_vars <- setdiff(timepoints, names(valid_data))
  if (length(missing_vars) > 0) {
    stop(paste("Відсутні змінні:", paste(missing_vars, collapse = ", ")))
  }
  
  # Перетворення в long-формат
  valid_data_long <- valid_data %>%
    select(all_of(timepoints)) %>%
    pivot_longer(cols = everything(),
                 names_to = "Time",
                 values_to = "Score") %>%
    mutate(Time = recode(Time,
                         !!timepoints[1] := "T1",
                         !!timepoints[2] := "T2",
                         !!timepoints[3] := "T3"),
           Time = factor(Time, levels = c("T1", "T2", "T3")))
  
  # Побудова графіка
  ggplot(valid_data_long, aes(x = Time, y = Score)) +
    geom_boxplot(width = 0.3, outlier.shape = "*", fill = "skyblue") +
    theme_minimal(base_size = 14) +
    labs(
         x = "Етап",
         y = paste(suffix, "")) +
    coord_cartesian(ylim = ylim)
}
plot_boxplot_t1_t3(valid_data,"PHQ.9", c(0,27))
ggsave("boxplot_PHQ9.png",plot = plot_boxplot_t1_t3(valid_data,"PHQ.9", c(0,27)),
       width = 6,height = 5,dpi = 300)

options(digits = 6)

run_friedman_test(valid_data,"PHQ.9", 1:3)

run_dunn_posthoc_t1_t3 <- function(valid_data, suffix = "PSS.10", set=1:3, method = "bonferroni") {
  # Потрібні пакети
  if (!requireNamespace("FSA", quietly = TRUE)) {
    install.packages("FSA")
  }
  
  # Формуємо список змінних
  timepoints <- paste0("T", set, "_", suffix)
  
  # Перевірка наявності змінних
  missing_vars <- setdiff(timepoints, names(valid_data))
  if (length(missing_vars) > 0) {
    stop(paste("Відсутні змінні:", paste(missing_vars, collapse = ", ")))
  }
  
  # Підготовка даних
  data_wide <- valid_data[timepoints]
  data_complete <- na.omit(data_wide)
  data_complete$id <- seq_len(nrow(data_complete))  # додаємо ідентифікатор
  
  # Перетворення в long-формат
  data_long <- reshape(data_complete,
                       varying = timepoints,
                       v.names = "Score",
                       timevar = "Time",
                       times = c("T1", "T2", "T3"),
                       direction = "long")
  
  # Тест Данна
  dunn_result <- dunnTest(Score ~ Time, data = data_long, method = method)
  
  return(dunn_result)
}


run_dunn_posthoc_t1_t3(valid_data, suffix = "PHQ.9")


options(digits = 3)




#########################################################  GAD.7

calculate_stats(valid_data, "T1", "GAD.7")
calculate_stats(valid_data, "T2", "GAD.7")
calculate_stats(valid_data, "T3", "GAD.7")

plot_boxplot_t1_t3(valid_data,"GAD.7", c(0,21))
ggsave("boxplot_GAD7.png",plot = plot_boxplot_t1_t3(valid_data,"GAD.7", c(0,21)),
       width = 6,height = 5,dpi = 300)
options(digits = 6)

run_friedman_test(valid_data,"GAD.7", 1:3)

run_dunn_posthoc_t1_t3(valid_data, suffix = "GAD.7")






#########################################################  PCL.5

calculate_stats(valid_data, "T1", "PCL.5")
calculate_stats(valid_data, "T2", "PCL.5")
calculate_stats(valid_data, "T3", "PCL.5")

plot_boxplot_t1_t3(valid_data,"PCL.5", c(0,80))
ggsave("boxplot_PCL5.png",plot = plot_boxplot_t1_t3(valid_data,"PCL.5", c(0,80)),
       width = 6,height = 5,dpi = 300)

options(digits = 6)

run_friedman_test(valid_data,"PCL.5", 1:3)

options(digits = 4)
run_dunn_posthoc_t1_t3(valid_data, suffix = "PCL.5")




#########################################################  CD-RISC-10

calculate_stats(valid_data, "T1", "CD.RISC.10")
calculate_stats(valid_data, "T2", "CD.RISC.10")
calculate_stats(valid_data, "T3", "CD.RISC.10")

plot_boxplot_t1_t3(valid_data,"CD.RISC.10", c(0,45))
ggsave("boxplot_CDRISC10.png",plot = plot_boxplot_t1_t3(valid_data,"CD.RISC.10", c(0,45)),
       width = 6,height = 5,dpi = 300)

options(digits = 6)

run_friedman_test(valid_data,"CD.RISC.10", 1:3)

options(digits = 4)
run_dunn_posthoc_t1_t3(valid_data, suffix = "CD.RISC.10")



#########################################################  CRAFFT

calculate_stats(valid_data, "T1", "CRAFFT")
calculate_stats(valid_data, "T2", "CRAFFT")
calculate_stats(valid_data, "T3", "CRAFFT")

plot_boxplot_t1_t3(valid_data,"CRAFFT", c(0,40))
ggsave("boxplot_CRAFFT.png",plot = plot_boxplot_t1_t3(valid_data,"CRAFFT", c(0,40)),
       width = 6,height = 5,dpi = 300)


p1 <- ggplot(valid_data, aes(x = T1_CRAFFT)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Етап T1",
       x = "CRAFFT",
       y = "Кількість респондентів") +
  theme_minimal()

p2 <- ggplot(valid_data, aes(x = T2_CRAFFT)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Етап T2",
       x = "CRAFFT",
       y = "Кількість респондентів") +
  theme_minimal()

p3 <- ggplot(valid_data, aes(x = T3_CRAFFT)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Етап T3",
       x = "CRAFFT",
       y = "Кількість респондентів") +
  theme_minimal()


combined_plot <- (p1 + p2 + p3) +
  plot_annotation(title = "Розподіл значень CRAFFT на різних етапах")
combined_plot
ggsave("CRAFFT_hist_all.png",
  plot = combined_plot,width = 12,height = 4,dpi = 300)

options(digits = 6)

run_friedman_test(valid_data,"CRAFFT", 1:3)

options(digits = 4)
run_dunn_posthoc_t1_t3(valid_data, suffix = "CRAFFT")



table(valid_data$T1_CRAFFT)
prop.table(table(valid_data$T1_CRAFFT)) * 100
table(valid_data$T2_CRAFFT)
prop.table(table(valid_data$T2_CRAFFT)) * 100
table(valid_data$T3_CRAFFT)
prop.table(table(valid_data$T3_CRAFFT)) * 100


#Скільки днів за останні 12 місяців Ви:
#Q83_1 Вживали, більш ніж декілька ковтків, пива, вина або ж інших алкогольних напоїв? Відмітьте «0» якщо не вживали. - Кількість днів	
#Q84_1 Вживаєте якийсь вид марихуани (канабіс, олія, віск, паління, вейпінг, вдихання випарування або їстівні продукти) або синтетичну марихуану («К2» чи «спайс»)? Відмітьте «0» якщо не вживали. - Кількість днів	
#Q85_1 Вживаєте щось інше для «кайфу» (наприклад, пігулки, заборонені наркотичні засоби, медикаменти, що відпускаються без рецепта, речовини, які ви нюхаєте або вдихаєте, або вводите ін’єкцією)? Відмітьте «0», якщо не вживали - Кількість днів	

#Q86 Чи сідали ви коли-небудь у машину, за кермом якої знаходилася людина (включаючи вас) у стані алкогольного сп’яніння, під дією наркотиків або «під кайфом»?	
#Q87 Чи вживали ви коли-небудь алкоголь або наркотики, щоб розслабитися, самоствердитися або «вписатися»?	
#Q88 Чи вживали ви коли-небудь алкоголь або наркотики без друзів, самостійно?	
#Q89 Чи забували ви коли-небудь те, що робили під впливом алкогольних напоїв або наркотиків?	
#Q90 Чи говорили вам коли-небудь ваші родичі або друзі, що вам треба менше вживати алкоголь або наркотики?	
#Q91 Чи потрапляли ви коли-небудь у неприємності, перебуваючи під впливом алкогольних напоїв або наркотиків?

valid_data$T1_Q83_1 <- as.numeric(valid_data$T1_Q83_1)
valid_data$T2_Q83_1 <- as.numeric(valid_data$T2_Q83_1)
valid_data$T3_Q83_1 <- as.numeric(valid_data$T3_Q83_1)
calculate_stats(valid_data, "T1", "Q83_1")
calculate_stats(valid_data, "T2", "Q83_1")
calculate_stats(valid_data, "T3", "Q83_1")

plot_boxplot_t1_t3(valid_data,"Q83_1", c(0,365))
ggsave("boxplot_Q83_1.png",plot_boxplot_t1_t3(valid_data,"Q83_1", c(0,365)),
       width = 6,height = 5,dpi = 300)
run_friedman_test(valid_data,"Q83_1", 1:3)

table(valid_data$T1_Q83_1)
prop.table(table(valid_data$T1_Q83_1)) * 100
table(valid_data$T2_Q83_1)
prop.table(table(valid_data$T2_Q83_1)) * 100
table(valid_data$T3_Q83_1)
prop.table(table(valid_data$T3_Q83_1)) * 100


valid_data$T1_Q84_1 <- as.numeric(valid_data$T1_Q84_1)
valid_data$T2_Q84_1 <- as.numeric(valid_data$T2_Q84_1)
valid_data$T3_Q84_1 <- as.numeric(valid_data$T3_Q84_1)
calculate_stats(valid_data, "T1", "Q84_1")
calculate_stats(valid_data, "T2", "Q84_1")
calculate_stats(valid_data, "T3", "Q84_1")

table(valid_data$T1_Q84_1)
prop.table(table(valid_data$T1_Q84_1)) * 100
table(valid_data$T2_Q84_1)
prop.table(table(valid_data$T2_Q84_1)) * 100
table(valid_data$T3_Q84_1)
prop.table(table(valid_data$T3_Q84_1)) * 100

valid_data$T1_Q85_1 <- as.numeric(valid_data$T1_Q85_1)
valid_data$T2_Q85_1 <- as.numeric(valid_data$T2_Q85_1)
valid_data$T3_Q85_1 <- as.numeric(valid_data$T3_Q85_1)
calculate_stats(valid_data, "T1", "Q85_1")
calculate_stats(valid_data, "T2", "Q85_1")
calculate_stats(valid_data, "T3", "Q85_1")

table(valid_data$T1_Q85_1)
prop.table(table(valid_data$T1_Q85_1)) * 100
table(valid_data$T2_Q85_1)
prop.table(table(valid_data$T2_Q85_1)) * 100
table(valid_data$T3_Q85_1)
prop.table(table(valid_data$T3_Q85_1)) * 100


##### WHO-5 #############################################################################

calculate_stats(valid_data, "T1", "WHO.5")
calculate_stats(valid_data, "T2", "WHO.5")
calculate_stats(valid_data, "T3", "WHO.5")

plot_boxplot_t1_t3(valid_data,"WHO.5", c(0,25))
ggsave("boxplot_WHO5.png",plot_boxplot_t1_t3(valid_data,"WHO.5", c(0,25)),
       width = 6,height = 5,dpi = 300)

options(digits = 6)

run_friedman_test(valid_data,"WHO.5", 1:3)

run_dunn_posthoc_t1_t3(valid_data, suffix = "WHO.5")



##### WHODAS #############################################################################


calculate_stats(valid_data, "T1", "WHODAS")
calculate_stats(valid_data, "T2", "WHODAS")
calculate_stats(valid_data, "T3", "WHODAS")

plot_boxplot_t1_t3(valid_data,"WHODAS", c(0,29))
ggsave("boxplot_WHODAS.png",plot_boxplot_t1_t3(valid_data,"WHODAS", c(0,29)),
       width = 6,height = 5,dpi = 300)

options(digits = 6)

run_friedman_test(valid_data,"WHODAS", 1:3)

run_dunn_posthoc_t1_t3(valid_data, suffix = "WHODAS")





# ============================================================
# COHEN'S D з 95% CI — розмір ефекту для всіх шкал
# ============================================================
# ПІСЛЯ Friedman/Dunn тестів: ми вже знаємо, ЩО є
# різниця. Тепер дізнаємось, НАСКІЛЬКИ велика ця різниця.
#
# Для парних вимірів (той самий учасник на T1, T2, T3)
# використовується Cohen's d_z = M_diff / SD_diff
# 
# Інтерпретація (Cohen, 1988):
#   |d| < 0.2  → дуже малий / відсутній
#   |d| 0.2-0.5 → малий
#   |d| 0.5-0.8 → середній
#   |d| > 0.8   → великий
#
# Для шкал, де ↑ = гірше (PSS, PHQ, GAD, PCL, WHODAS):
#   d > 0 означає ПОКРАЩЕННЯ (зниження балу)
# Для шкал, де ↑ = краще (CD-RISC, WHO-5):
#   d > 0 означає ПОКРАЩЕННЯ (зростання балу)
# ============================================================

# Встановити, якщо ще немає:
# install.packages("effectsize")
library(effectsize)
library(dplyr)

# --- 1. Список шкал та їхній напрям -------------------------
scales_d <- list(
  list(suffix = "PSS.10",     name = "PSS-10",     higher_better = FALSE),
  list(suffix = "PHQ.9",      name = "PHQ-9",      higher_better = FALSE),
  list(suffix = "GAD.7",      name = "GAD-7",      higher_better = FALSE),
  list(suffix = "PCL.5",      name = "PCL-5",      higher_better = FALSE),
  list(suffix = "CD.RISC.10", name = "CD-RISC-10", higher_better = TRUE),
  list(suffix = "WHO.5",      name = "WHO-5",      higher_better = TRUE),
  list(suffix = "WHODAS",     name = "WHODAS",     higher_better = FALSE)
)

# --- 2. Функція: Cohen's d для одного переходу --------------
compute_cohens_d <- function(data, scale_suffix, t_from, t_to, higher_better) {
  
  col_from <- paste0(t_from, "_", scale_suffix)
  col_to   <- paste0(t_to,   "_", scale_suffix)
  
  # Беремо лише пари без NA
  df <- data %>%
    select(all_of(c(col_from, col_to))) %>%
    filter(!is.na(.[[1]]) & !is.na(.[[2]]))
  
  if (nrow(df) < 10) {
    return(data.frame(
      Scale = scale_suffix, Transition = paste0(t_from, "→", t_to),
      N = nrow(df), M_pre = NA, M_post = NA, 
      d = NA, CI_low = NA, CI_high = NA, interpretation = "недостатньо даних"
    ))
  }
  
  x_pre  <- df[[col_from]]
  x_post <- df[[col_to]]
  
  # Cohen's d_z з 95% CI (для парних вимірів)
  # Для шкал "↑ = гірше" порядок (pre, post) → d > 0 = покращення (зниження)
  # Для шкал "↑ = краще" порядок (post, pre) → d > 0 = покращення (зростання)
  if (higher_better) {
    d_result <- effectsize::cohens_d(x_post, x_pre, paired = TRUE, ci = 0.95)
  } else {
    d_result <- effectsize::cohens_d(x_pre, x_post, paired = TRUE, ci = 0.95)
  }
  
  d_value <- d_result$Cohens_d
  ci_low  <- d_result$CI_low
  ci_high <- d_result$CI_high
  
  # Інтерпретація за абсолютним значенням
  abs_d <- abs(d_value)
  interp <- case_when(
    abs_d < 0.2  ~ "дуже малий / відсутній",
    abs_d < 0.5  ~ "малий",
    abs_d < 0.8  ~ "середній",
    TRUE         ~ "великий"
  )
  if (d_value > 0)      interp <- paste(interp, "↓ покращення")
  else if (d_value < 0) interp <- paste(interp, "↑ погіршення")
  
  data.frame(
    Scale          = scale_suffix,
    Transition     = paste0(t_from, "→", t_to),
    N              = nrow(df),
    M_pre          = round(mean(x_pre, na.rm = TRUE), 2),
    M_post         = round(mean(x_post, na.rm = TRUE), 2),
    d              = round(d_value, 3),
    CI_low         = round(ci_low,  3),
    CI_high        = round(ci_high, 3),
    interpretation = interp
  )
}

# --- 3. Запуск для всіх шкал та переходів --------------------
transitions <- list(
  c("T1", "T2"),
  c("T1", "T3"),
  c("T2", "T3")
)

cohens_d_results <- bind_rows(
  lapply(scales_d, function(sc) {
    bind_rows(
      lapply(transitions, function(tr) {
        compute_cohens_d(
          data         = valid_data,
          scale_suffix = sc$suffix,
          t_from       = tr[1],
          t_to         = tr[2],
          higher_better = sc$higher_better
        )
      })
    ) %>% mutate(Scale = sc$name)
  })
)

# --- 4. Бонус: PSS-10 T0 → T1 (до інтервенції) -------------
cohens_d_pss_t0t1 <- compute_cohens_d(
  data = valid_data, scale_suffix = "PSS.10",
  t_from = "T0", t_to = "T1", higher_better = FALSE
) %>% mutate(Scale = "PSS-10")

# Додаємо його до основних результатів
cohens_d_results <- bind_rows(cohens_d_pss_t0t1, cohens_d_results)

cat("\n=== РОЗМІР ЕФЕКТУ (Cohen's d з 95% CI) ===\n")
print(cohens_d_results, row.names = FALSE)








######## Вілкоксон 

library(dplyr)
library(purrr)
library(tibble)

# Вхідні імена стовпців (за потреби змініть)
times <- c("T0_PSS.10", "T1_PSS.10", "T2_PSS.10", "T3_PSS.10")

# Функція, що робить парний тест Вілкоксона для двох колонок (парні спостереження по Q3)
pair_wilcox <- function(data, a, b, id_col = "Q3") {
  # беремо тільки рядки, де обидва не NA
  sel <- !is.na(data[[a]]) & !is.na(data[[b]])
  x <- data[[a]][sel]
  y <- data[[b]][sel]
  n <- length(x)
  if(n < 1) return(tibble(var1 = a, var2 = b, n = n, statistic = NA_real_, p.value = NA_real_))
  wt <- wilcox.test(x, y, paired = TRUE, exact = FALSE) # exact = FALSE for large samples
  tibble(
    var1 = a,
    var2 = b,
    n = n,
    statistic = wt$statistic[[1]],   # V (Wilcoxon signed-rank)
    p.value = wt$p.value,
    median_diff = median(x - y, na.rm = TRUE)
  )
}

# Згенеруємо всі пари
pairs <- combn(times, 2, simplify = FALSE)

# Застосуємо тест до кожної пари
res_list <- map_dfr(pairs, ~ pair_wilcox(valid_data, .x[1], .x[2], id_col = "Q3"))

# Додаємо поправку на множинні порівняння (Bonferroni; або "holm", "BH" тощо)
res <- res_list %>%
  mutate(
    p.adj.bonf = p.adjust(p.value, method = "bonferroni"),
    p.adj.holm = p.adjust(p.value, method = "holm")
  ) %>%
  arrange(p.adj.bonf)

# Виводимо результат
print(res)

# Парний t-тест для T2 та T3
t_test_res <- t.test(
  valid_data$T2_PSS.10,
  valid_data$T3_PSS.10,
  paired = TRUE,
  alternative = "two.sided"
)

t_test_res




################## Статистика окремо по статі

gender_filtered <- valid_data %>%
  filter(T1_Q5 %in% c("чоловіча", "жіноча"))

# Перетворення у long формат
gender_long <- gender_filtered %>%
  pivot_longer(
    cols = c(T0_PSS.10, T1_PSS.10, T2_PSS.10, T3_PSS.10),
    names_to = "Time",
    values_to = "Score"
  ) %>%
  mutate(Time = recode(Time,
                       "T0_PSS.10" = "Скринінг",
                       "T1_PSS.10" = "T1",
                       "T2_PSS.10" = "T2",
                       "T3_PSS.10" = "T3"),
         Time = factor(Time, levels = c("Скринінг", "T1", "T2", "T3")))  # порядок рівнів

# Попарні boxplot для кожного Time
ggplot(gender_long, aes(x = T1_Q5, y = Score, fill = T1_Q5)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ Time, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  labs(title = "PSS-10 за статтю",
       x = "Стать",
       y = "PSS-10",
       fill = "Стать") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q5_Стать.png", width = 10, height = 4, dpi = 300)

T0_by_gender <- valid_data %>%
  group_by(T1_Q5) %>%
  summarise(
    N_missing = sum(is.na(T0_PSS.10)),
    N = sum(!is.na(T0_PSS.10)),
    Mean = mean(T0_PSS.10, na.rm = TRUE),
    SD = sd(T0_PSS.10, na.rm = TRUE),
    Min = min(T0_PSS.10, na.rm = TRUE),
    Q1 = quantile(T0_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T0_PSS.10, na.rm = TRUE),
    Q3 = quantile(T0_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T0_PSS.10, na.rm = TRUE)
  )
T0_by_gender


T1_by_gender <- valid_data %>%
  group_by(T1_Q5) %>%
  summarise(
    N_missing = sum(is.na(T1_PSS.10)),
    N = sum(!is.na(T1_PSS.10)),
    Mean = mean(T1_PSS.10, na.rm = TRUE),
    SD = sd(T1_PSS.10, na.rm = TRUE),
    Min = min(T1_PSS.10, na.rm = TRUE),
    Q1 = quantile(T1_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T1_PSS.10, na.rm = TRUE),
    Q3 = quantile(T1_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T1_PSS.10, na.rm = TRUE)
  )
T1_by_gender


T2_by_gender <- valid_data %>%
  group_by(T1_Q5) %>%
  summarise(
    N_missing = sum(is.na(T2_PSS.10)),
    N = sum(!is.na(T2_PSS.10)),
    Mean = mean(T2_PSS.10, na.rm = TRUE),
    SD = sd(T2_PSS.10, na.rm = TRUE),
    Min = min(T2_PSS.10, na.rm = TRUE),
    Q1 = quantile(T2_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T2_PSS.10, na.rm = TRUE),
    Q3 = quantile(T2_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T2_PSS.10, na.rm = TRUE)
  )
T2_by_gender


T3_by_gender <- valid_data %>%
  group_by(T1_Q5) %>%
  summarise(
    N_missing = sum(is.na(T3_PSS.10)),
    N = sum(!is.na(T3_PSS.10)),
    Mean = mean(T3_PSS.10, na.rm = TRUE),
    SD = sd(T3_PSS.10, na.rm = TRUE),
    Min = min(T3_PSS.10, na.rm = TRUE),
    Q1 = quantile(T3_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T3_PSS.10, na.rm = TRUE),
    Q3 = quantile(T3_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T3_PSS.10, na.rm = TRUE)
  )
T3_by_gender


library(rstatix)

# Тест Вілкоксона для кожного рівня Time Манна-Уітні на кожному зрізі часу (жінки vs чоловіки)
wilcox_results <- gender_long %>%
  group_by(Time) %>%
  wilcox_test(Score ~ T1_Q5) %>%                       # value - числові значення PSS
  adjust_pvalue(method = "bonferroni") %>%              # корекція p-value, за потреби
  add_significance("p.adj")

wilcox_results

# Використовуємо вже існуючу функцію pair_wilcox і змінну times

# Розділяємо дані за статтю
women_data <- gender_filtered %>% filter(T1_Q5 == "жіноча")
men_data   <- gender_filtered %>% filter(T1_Q5 == "чоловіча")

# Застосовуємо pair_wilcox до кожної статі окремо
run_paired_wilcox <- function(data, group_label) {
  pairs <- combn(times, 2, simplify = FALSE)
  map_dfr(pairs, ~ pair_wilcox(data, .x[1], .x[2])) %>%
    mutate(
      Стать      = group_label,
      p.adj.bonf = p.adjust(p.value, method = "bonferroni"),
      p.adj.holm = p.adjust(p.value, method = "holm"),
      sig        = symnum(p.adj.bonf, corr = FALSE,
                          cutpoints = c(0, .001, .01, .05, .1, 1),
                          symbols   = c("***", "**", "*", ".", "ns"))
    ) %>%
    select(Стать, var1, var2, n, statistic, p.value, p.adj.bonf, p.adj.holm, sig, median_diff)
}

wilcox_women <- run_paired_wilcox(women_data, "Жінки")
wilcox_men   <- run_paired_wilcox(men_data,   "Чоловіки")

# Об'єднуємо результати
wilcox_by_gender <- bind_rows(wilcox_women, wilcox_men)

print(wilcox_by_gender, n = Inf)





############### Статистика окремо по університетах

# Перетворення у long формат
univ_long <- valid_data %>%
  pivot_longer(
    cols = c(T0_PSS.10, T1_PSS.10, T2_PSS.10, T3_PSS.10),
    names_to = "Time",
    values_to = "Score"
  ) %>%
  mutate(Time = recode(Time,
                       "T0_PSS.10" = "Скринінг",
                       "T1_PSS.10" = "T1",
                       "T2_PSS.10" = "T2",
                       "T3_PSS.10" = "T3"),
         Time = factor(Time, levels = c("Скринінг", "T1", "T2", "T3")))  # порядок рівнів

# Попарні boxplot для кожного Time
ggplot(univ_long, aes(x = T1_University, y = Score, fill = T1_University)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ Time, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # вертикальні підписи
  labs(title = "PSS-10 за університетом",
       x = "Університет",
       y = "PSS-10",
       fill = "Університет") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q_University1.png", width = 10, height = 4, dpi = 300)

# Попарні boxplot для кожного Time
ggplot(univ_long, aes(x = Time, y = Score, fill = T1_University)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ T1_University, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10 за університетом",
       x = "Етап",
       y = "PSS-10",
       fill = "Університет") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q_University2.png", width = 10, height = 4, dpi = 300)

T0_by_univ <- valid_data %>%
  group_by(T1_University) %>%
  summarise(
    N_missing = sum(is.na(T0_PSS.10)),
    N = sum(!is.na(T0_PSS.10)),
    Mean = mean(T0_PSS.10, na.rm = TRUE),
    SD = sd(T0_PSS.10, na.rm = TRUE),
    Min = min(T0_PSS.10, na.rm = TRUE),
    Q1 = quantile(T0_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T0_PSS.10, na.rm = TRUE),
    Q3 = quantile(T0_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T0_PSS.10, na.rm = TRUE)
  )
T0_by_univ

T1_by_univ <- valid_data %>%
  group_by(T1_University) %>%
  summarise(
    N_missing = sum(is.na(T1_PSS.10)),
    N = sum(!is.na(T1_PSS.10)),
    Mean = mean(T1_PSS.10, na.rm = TRUE),
    SD = sd(T1_PSS.10, na.rm = TRUE),
    Min = min(T1_PSS.10, na.rm = TRUE),
    Q1 = quantile(T1_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T1_PSS.10, na.rm = TRUE),
    Q3 = quantile(T1_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T1_PSS.10, na.rm = TRUE)
  )
T1_by_univ


T2_by_univ <- valid_data %>%
  group_by(T1_University) %>%
  summarise(
    N_missing = sum(is.na(T2_PSS.10)),
    N = sum(!is.na(T2_PSS.10)),
    Mean = mean(T2_PSS.10, na.rm = TRUE),
    SD = sd(T2_PSS.10, na.rm = TRUE),
    Min = min(T2_PSS.10, na.rm = TRUE),
    Q1 = quantile(T2_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T2_PSS.10, na.rm = TRUE),
    Q3 = quantile(T2_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T2_PSS.10, na.rm = TRUE)
  )
T2_by_univ

T3_by_univ <- valid_data %>%
  group_by(T1_University) %>%
  summarise(
    N_missing = sum(is.na(T3_PSS.10)),
    N = sum(!is.na(T3_PSS.10)),
    Mean = mean(T3_PSS.10, na.rm = TRUE),
    SD = sd(T3_PSS.10, na.rm = TRUE),
    Min = min(T3_PSS.10, na.rm = TRUE),
    Q1 = quantile(T3_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T3_PSS.10, na.rm = TRUE),
    Q3 = quantile(T3_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T3_PSS.10, na.rm = TRUE)
  )
T3_by_univ

# Переконаємось, що University – фактор
univ_long$T1_University <- as.factor(univ_long$T1_University)

# Чи є відмінність між рівнями для кожного етапу?
# Виконаємо тест Вілкоксона для кожного Time 
#тест Манна-Уітні між університетами на кожному часовому зрізі окремо. Питання: "Чи відрізняються університети між собою на Скринінгу? На T1? На T2? На T3?
wilcox_results <- univ_long %>%
  group_by(Time) %>%
  wilcox_test(Score ~ T1_University) %>%                       # value - числові значення PSS
  adjust_pvalue(method = "bonferroni") %>%              # корекція p-value, за потреби
  add_significance("p.adj")

wilcox_results
wilcox_results %>%
  filter(p.adj < 0.05)
# Парний Вілкоксон для кожного університету окремо. Питання: "Чи змінювався стрес у студентів Вінниці з часом? А Києва? А Харкова?"
# Створимо список для результатів
wilcox_results <- list()

selected_universities <- c("Вінниця", "Київ", "Одеса", "Тернопіль", "Харків")

# Цикл по університетах
for (uni in selected_universities) {
  
  df_uni <- univ_long %>%
    filter(T1_University == uni)
  
  # Тест Вілкоксона з попарними порівняннями між часами
  res_uni <- df_uni %>%
    pairwise_wilcox_test(
      Score ~ Time,
      paired = TRUE,          # якщо різні учасники
      p.adjust.method = "bonferroni"
    ) %>%
    mutate(T1_University = uni)
  
  wilcox_results[[uni]] <- res_uni
}

# Об’єднати результати всіх університетів в одну таблицю
wilcox_results_all <- bind_rows(wilcox_results)


wilcox_results_all %>%
  filter(p.adj < 0.05)
# Переглянути результати
wilcox_results_all







# Статистика за кількістю членів сім"ї ##################################################################

# Перетворення у long формат
family_long <- valid_data %>%
  pivot_longer(
    cols = c(T0_PSS.10, T1_PSS.10, T2_PSS.10, T3_PSS.10),
    names_to = "Time",
    values_to = "Score"
  ) %>%
  mutate(Time = recode(Time,
                       "T0_PSS.10" = "Скринінг",
                       "T1_PSS.10" = "T1",
                       "T2_PSS.10" = "T2",
                       "T3_PSS.10" = "T3"),
         Time = factor(Time, levels = c("Скринінг", "T1", "T2", "T3")))  # порядок рівнів

# Попарні boxplot для кожного Time
ggplot(family_long, aes(x = T1_Q7, y = Score, fill = T1_Q7)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ Time, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10 за к-стю членів сім'ї",
       x = "Число членів сім'ї",
       y = "PSS-10",
       fill = "Число членів сім'ї") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q7_Сім'я1.png", width = 10, height = 4, dpi = 300)

# Попарні boxplot для кожного Time
ggplot(family_long, aes(x = Time, y = Score, fill = T1_Q7)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ T1_Q7, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10 за к-стю членів сім'ї",
       x = "Етап",
       y = "PSS-10",
       fill = "Число членів сім'ї") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q7_Сім'я2.png", width = 10, height = 4, dpi = 300)

T0_by_family_members <- valid_data %>%
  group_by(T1_Q7) %>%
  summarise(
    N_missing = sum(is.na(T0_PSS.10)),
    N = sum(!is.na(T0_PSS.10)),
    Mean = mean(T0_PSS.10, na.rm = TRUE),
    SD = sd(T0_PSS.10, na.rm = TRUE),
    Min = min(T0_PSS.10, na.rm = TRUE),
    Q1 = quantile(T0_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T0_PSS.10, na.rm = TRUE),
    Q3 = quantile(T0_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T0_PSS.10, na.rm = TRUE)
  )
T0_by_family_members


T1_by_family_members <- valid_data %>%
  group_by(T1_Q7) %>%
  summarise(
    N_missing = sum(is.na(T1_PSS.10)),
    N = sum(!is.na(T1_PSS.10)),
    Mean = mean(T1_PSS.10, na.rm = TRUE),
    SD = sd(T1_PSS.10, na.rm = TRUE),
    Min = min(T1_PSS.10, na.rm = TRUE),
    Q1 = quantile(T1_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T1_PSS.10, na.rm = TRUE),
    Q3 = quantile(T1_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T1_PSS.10, na.rm = TRUE)
  )
T1_by_family_members



T2_by_family_members <- valid_data %>%
  group_by(T1_Q7) %>%
  summarise(
    N_missing = sum(is.na(T2_PSS.10)),
    N = sum(!is.na(T2_PSS.10)),
    Mean = mean(T2_PSS.10, na.rm = TRUE),
    SD = sd(T2_PSS.10, na.rm = TRUE),
    Min = min(T2_PSS.10, na.rm = TRUE),
    Q1 = quantile(T2_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T2_PSS.10, na.rm = TRUE),
    Q3 = quantile(T2_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T2_PSS.10, na.rm = TRUE)
  )
T2_by_family_members


T3_by_family_members <- valid_data %>%
  group_by(T1_Q7) %>%
  summarise(
    N_missing = sum(is.na(T3_PSS.10)),
    N = sum(!is.na(T3_PSS.10)),
    Mean = mean(T3_PSS.10, na.rm = TRUE),
    SD = sd(T3_PSS.10, na.rm = TRUE),
    Min = min(T3_PSS.10, na.rm = TRUE),
    Q1 = quantile(T3_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T3_PSS.10, na.rm = TRUE),
    Q3 = quantile(T3_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T3_PSS.10, na.rm = TRUE)
  )
T3_by_family_members


# Чи є відмінність між рівнями для кожного етапу?
# Виконаємо тест Вілкоксона для кожного Time 
wilcox_results <- univ_long %>%
  group_by(Time) %>%
  wilcox_test(Score ~ T1_Q7) %>%                       # value - числові значення PSS
  adjust_pvalue(method = "bonferroni") %>%              # корекція p-value, за потреби
  add_significance("p.adj")

wilcox_results
#write.csv(wilcox_results, "wilcox_results_family.csv", row.names = FALSE)

# Створимо список для результатів
wilcox_results_family <- list()

selected_members <- c("1", "2", "3", "4", "більше 5")

# Цикл по к-сті членів сім"ї
for (uni in selected_members) {
  
  df_uni <- family_long %>%
    filter(T1_Q7 == uni)
  
  # Тест Вілкоксона з попарними порівняннями між часами
  res_uni <- df_uni %>%
    pairwise_wilcox_test(
      Score ~ Time,
      paired = TRUE,          # якщо різні учасники
      p.adjust.method = "bonferroni"
    ) %>%
    mutate(T1_Q7 = uni)
  
  wilcox_results_family[[uni]] <- res_uni
}

# Об’єднати результати в одну таблицю
wilcox_results_f_all <- bind_rows(wilcox_results_family)
wilcox_results_f_all
#write.csv(wilcox_results_f_all, "wilcox_results_family_all.csv", row.names = FALSE)





# Більшу частину часу Ви проживали ##################################################################

# Попарні boxplot для кожного Time
ggplot(family_long, aes(x = T1_Q8, y = Score, fill = T1_Q8)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ Time, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10 за часом проживання",
       x = "Більшу частину часу проживав (-ла)",
       y = "PSS-10") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q8_Проживали1.png", width = 10, height = 4, dpi = 300)

ggplot(family_long, aes(x = Time, y = Score, fill = T1_Q8)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ T1_Q8, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10 за часом проживання",
       x = NULL,
       y = "PSS-10",
       fill = "Більшу частину часу проживав (-ла)") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q8_Проживали2.png", width = 10, height = 4, dpi = 300)

T0_by_time_spent <- valid_data %>%
  group_by(T1_Q8) %>%
  summarise(
    N_missing = sum(is.na(T0_PSS.10)),
    N = sum(!is.na(T0_PSS.10)),
    Mean = mean(T0_PSS.10, na.rm = TRUE),
    SD = sd(T0_PSS.10, na.rm = TRUE),
    Min = min(T0_PSS.10, na.rm = TRUE),
    Q1 = quantile(T0_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T0_PSS.10, na.rm = TRUE),
    Q3 = quantile(T0_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T0_PSS.10, na.rm = TRUE)
  )
T0_by_time_spent

T1_by_time_spent <- valid_data %>%
  group_by(T1_Q8) %>%
  summarise(
    N_missing = sum(is.na(T1_PSS.10)),
    N = sum(!is.na(T1_PSS.10)),
    Mean = mean(T1_PSS.10, na.rm = TRUE),
    SD = sd(T1_PSS.10, na.rm = TRUE),
    Min = min(T1_PSS.10, na.rm = TRUE),
    Q1 = quantile(T1_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T1_PSS.10, na.rm = TRUE),
    Q3 = quantile(T1_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T1_PSS.10, na.rm = TRUE)
  )
T1_by_time_spent


T2_by_time_spent <- valid_data %>%
  group_by(T1_Q8) %>%
  summarise(
    N_missing = sum(is.na(T2_PSS.10)),
    N = sum(!is.na(T2_PSS.10)),
    Mean = mean(T2_PSS.10, na.rm = TRUE),
    SD = sd(T2_PSS.10, na.rm = TRUE),
    Min = min(T2_PSS.10, na.rm = TRUE),
    Q1 = quantile(T2_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T2_PSS.10, na.rm = TRUE),
    Q3 = quantile(T2_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T2_PSS.10, na.rm = TRUE)
  )
T2_by_time_spent


T3_by_time_spent <- valid_data %>%
  group_by(T1_Q8) %>%
  summarise(
    N_missing = sum(is.na(T3_PSS.10)),
    N = sum(!is.na(T3_PSS.10)),
    Mean = mean(T3_PSS.10, na.rm = TRUE),
    SD = sd(T3_PSS.10, na.rm = TRUE),
    Min = min(T3_PSS.10, na.rm = TRUE),
    Q1 = quantile(T3_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T3_PSS.10, na.rm = TRUE),
    Q3 = quantile(T3_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T3_PSS.10, na.rm = TRUE)
  )
T3_by_time_spent


# ── 1. Манна-Уітні: чи є відмінність між групами T1_Q8 на кожному етапі? ──
wilcox_results_ts <- family_long %>%
  group_by(Time) %>%
  wilcox_test(Score ~ T1_Q8) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")

wilcox_results_ts
# write.csv(wilcox_results_ts, "wilcox_results_time_spent.csv", row.names = FALSE)

# ── 2. Парний Вілкоксон: динаміка в часі для кожної групи T1_Q8 ────────────
# Створимо список для результатів
wilcox_results_time_spent <- list()

selected_members <- c("Інше ", "З батьками або іншими членами сім'ї", "З домашніми тваринами", "З друзями", "З партнером", "З партнером і дитиною", "Самотньо")

# Цикл по к-сті членів сім"ї
for (uni in selected_members) {
  
  df_uni <- family_long %>%
    filter(T1_Q8 == uni)
  
  # Тест Вілкоксона з попарними порівняннями між часами
  res_uni <- df_uni %>%
    pairwise_wilcox_test(
      Score ~ Time,
      paired = TRUE,          # якщо різні учасники
      p.adjust.method = "bonferroni"
    ) %>%
    mutate(T1_Q8 = uni)
  
  wilcox_results_time_spent[[uni]] <- res_uni
}

# Об’єднати результати в одну таблицю
wilcox_results_ts_all <- bind_rows(wilcox_results_time_spent)
wilcox_results_ts_all
#write.csv(wilcox_results_ts_all, "wilcox_results_time_spent.csv", row.names = FALSE)






# Чи маєте Ви статус внутрішньо переміщеної особи? ##################################################################

# Попарні boxplot для кожного Time
ggplot(family_long, aes(x = T1_Q9, y = Score, fill = T1_Q9)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ Time, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10 за статусом ВПО",
       x = "Статус ВПО",
       y = "PSS-10") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q9_ВПО1.png", width = 10, height = 4, dpi = 300)

ggplot(family_long, aes(x = Time, y = Score, fill = T1_Q9)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ T1_Q9, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10 за статусом ВПО",
       x = NULL,
       y = "PSS-10",
       fill = "Більшу частину часу проживав (-ла)") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q9_ВПО2.png", width = 10, height = 4, dpi = 300)

T0_by_vpo <- valid_data %>%
  group_by(T1_Q9) %>%
  summarise(
    N_missing = sum(is.na(T0_PSS.10)),
    N = sum(!is.na(T0_PSS.10)),
    Mean = mean(T0_PSS.10, na.rm = TRUE),
    SD = sd(T0_PSS.10, na.rm = TRUE),
    Min = min(T0_PSS.10, na.rm = TRUE),
    Q1 = quantile(T0_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T0_PSS.10, na.rm = TRUE),
    Q3 = quantile(T0_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T0_PSS.10, na.rm = TRUE)
  )
T0_by_vpo


T1_by_vpo <- valid_data %>%
  group_by(T1_Q9) %>%
  summarise(
    N_missing = sum(is.na(T1_PSS.10)),
    N = sum(!is.na(T1_PSS.10)),
    Mean = mean(T1_PSS.10, na.rm = TRUE),
    SD = sd(T1_PSS.10, na.rm = TRUE),
    Min = min(T1_PSS.10, na.rm = TRUE),
    Q1 = quantile(T1_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T1_PSS.10, na.rm = TRUE),
    Q3 = quantile(T1_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T1_PSS.10, na.rm = TRUE)
  )
T1_by_vpo


T2_by_vpo <- valid_data %>%
  group_by(T1_Q9) %>%
  summarise(
    N_missing = sum(is.na(T2_PSS.10)),
    N = sum(!is.na(T2_PSS.10)),
    Mean = mean(T2_PSS.10, na.rm = TRUE),
    SD = sd(T2_PSS.10, na.rm = TRUE),
    Min = min(T2_PSS.10, na.rm = TRUE),
    Q1 = quantile(T2_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T2_PSS.10, na.rm = TRUE),
    Q3 = quantile(T2_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T2_PSS.10, na.rm = TRUE)
  )
T2_by_vpo


T3_by_vpo <- valid_data %>%
  group_by(T1_Q9) %>%
  summarise(
    N_missing = sum(is.na(T3_PSS.10)),
    N = sum(!is.na(T3_PSS.10)),
    Mean = mean(T3_PSS.10, na.rm = TRUE),
    SD = sd(T3_PSS.10, na.rm = TRUE),
    Min = min(T3_PSS.10, na.rm = TRUE),
    Q1 = quantile(T3_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T3_PSS.10, na.rm = TRUE),
    Q3 = quantile(T3_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T3_PSS.10, na.rm = TRUE)
  )
T3_by_vpo



# Створимо список для результатів
wilcox_results_vpo <- list()

selected_members <- c("Ні", "Так")

# Цикл по к-сті членів сім"ї
for (uni in selected_members) {
  
  df_uni <- family_long %>%
    filter(T1_Q9 == uni)
  
  # Тест Вілкоксона з попарними порівняннями між часами
  res_uni <- df_uni %>%
    pairwise_wilcox_test(
      Score ~ Time,
      paired = TRUE,          # якщо різні учасники
      p.adjust.method = "bonferroni"
    ) %>%
    mutate(T1_Q9 = uni)
  
  wilcox_results_vpo[[uni]] <- res_uni
}

# Об’єднати результати в одну таблицю
wilcox_results_vpo_all <- bind_rows(wilcox_results_vpo)
wilcox_results_vpo_all
#write.csv(wilcox_results_vpo_all, "wilcox_results_vpo.csv", row.names = FALSE)

# Чи є відмінність між рівнями для кожного етапу?
# Виконаємо тест Вілкоксона для кожного Time 
wilcox_results <- univ_long %>%
  group_by(Time) %>%
  wilcox_test(Score ~ T1_Q9) %>%                       # value - числові значення PSS
  adjust_pvalue(method = "bonferroni") %>%              # корекція p-value, за потреби
  add_significance("p.adj")

wilcox_results



# Q10 Чи проживали Ви на окупованій території?  ##################################################################

# Попарні boxplot для кожного Time
ggplot(univ_long, aes(x = T1_Q10, y = Score, fill = T1_Q10)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ Time, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10 за проживанням на окупованій території",
       x = "Проживання на окупованій території",
       y = "PSS-10") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q10_1.png", width = 10, height = 4, dpi = 300)

ggplot(univ_long, aes(x = Time, y = Score, fill = T1_Q10)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ T1_Q10, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10 за проживанням на окупованій території",
       x = NULL,
       y = "PSS-10",
       fill = "Проживання на окупованій території") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q10_2.png", width = 10, height = 4, dpi = 300)

T0_by_occupied <- valid_data %>%
  group_by(T1_Q10) %>%
  summarise(
    N_missing = sum(is.na(T0_PSS.10)),
    N = sum(!is.na(T0_PSS.10)),
    Mean = mean(T0_PSS.10, na.rm = TRUE),
    SD = sd(T0_PSS.10, na.rm = TRUE),
    Min = min(T0_PSS.10, na.rm = TRUE),
    Q1 = quantile(T0_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T0_PSS.10, na.rm = TRUE),
    Q3 = quantile(T0_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T0_PSS.10, na.rm = TRUE)
  )
T0_by_occupied


T1_by_occupied <- valid_data %>%
  group_by(T1_Q10) %>%
  summarise(
    N_missing = sum(is.na(T1_PSS.10)),
    N = sum(!is.na(T1_PSS.10)),
    Mean = mean(T1_PSS.10, na.rm = TRUE),
    SD = sd(T1_PSS.10, na.rm = TRUE),
    Min = min(T1_PSS.10, na.rm = TRUE),
    Q1 = quantile(T1_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T1_PSS.10, na.rm = TRUE),
    Q3 = quantile(T1_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T1_PSS.10, na.rm = TRUE)
  )
T1_by_occupied


T2_by_occupied <- valid_data %>%
  group_by(T1_Q10) %>%
  summarise(
    N_missing = sum(is.na(T2_PSS.10)),
    N = sum(!is.na(T2_PSS.10)),
    Mean = mean(T2_PSS.10, na.rm = TRUE),
    SD = sd(T2_PSS.10, na.rm = TRUE),
    Min = min(T2_PSS.10, na.rm = TRUE),
    Q1 = quantile(T2_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T2_PSS.10, na.rm = TRUE),
    Q3 = quantile(T2_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T2_PSS.10, na.rm = TRUE)
  )
T2_by_occupied


T3_by_occupied <- valid_data %>%
  group_by(T1_Q10) %>%
  summarise(
    N_missing = sum(is.na(T3_PSS.10)),
    N = sum(!is.na(T3_PSS.10)),
    Mean = mean(T3_PSS.10, na.rm = TRUE),
    SD = sd(T3_PSS.10, na.rm = TRUE),
    Min = min(T3_PSS.10, na.rm = TRUE),
    Q1 = quantile(T3_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T3_PSS.10, na.rm = TRUE),
    Q3 = quantile(T3_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T3_PSS.10, na.rm = TRUE)
  )
T3_by_occupied



# Створимо список для результатів
wilcox_results_occupied <- list()

selected_members <- c("Ні", "Так")

# Цикл по к-сті членів сім"ї
for (uni in selected_members) {
  
  df_uni <- family_long %>%
    filter(T1_Q10 == uni)
  
  # Тест Вілкоксона з попарними порівняннями між часами
  res_uni <- df_uni %>%
    pairwise_wilcox_test(
      Score ~ Time,
      paired = TRUE,          # якщо різні учасники
      p.adjust.method = "bonferroni"
    ) %>%
    mutate(T1_Q10 = uni)
  
  wilcox_results_occupied[[uni]] <- res_uni
}

# Об’єднати результати в одну таблицю
wilcox_results_occupied_all <- bind_rows(wilcox_results_occupied)
wilcox_results_occupied_all
#write.csv(wilcox_results_occupied_all, "wilcox_results_occupied.csv", row.names = FALSE)

# Чи є відмінність між рівнями для кожного етапу?
# Виконаємо тест Вілкоксона для кожного Time 
wilcox_results <- univ_long %>%
  group_by(Time) %>%
  wilcox_test(Score ~ T1_Q10) %>%                       # value - числові значення PSS
  adjust_pvalue(method = "bonferroni") %>%              # корекція p-value, за потреби
  add_significance("p.adj")

wilcox_results





# Чи були Ви розлучені з сім’єю через війну? $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Попарні boxplot для кожного Time
ggplot(univ_long, aes(x = T1_Q11, y = Score, fill = T1_Q11)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ Time, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10",
       x =  "Розлучені з сім’єю через війну",
       y = "PSS-10") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q11_1.png", width = 10, height = 4, dpi = 300)

ggplot(univ_long, aes(x = Time, y = Score, fill = T1_Q11)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ T1_Q11, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10 - Розлучені з сім’єю через війну",
       x = NULL,
       y = "PSS-10",
       fill = "Розлучені з сім’єю через війну") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q11_2.png", width = 10, height = 4, dpi = 300)

T0_by_sep_with_family <- valid_data %>%
  group_by(T1_Q11) %>%
  summarise(
    N_missing = sum(is.na(T0_PSS.10)),
    N = sum(!is.na(T0_PSS.10)),
    Mean = mean(T0_PSS.10, na.rm = TRUE),
    SD = sd(T0_PSS.10, na.rm = TRUE),
    Min = min(T0_PSS.10, na.rm = TRUE),
    Q1 = quantile(T0_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T0_PSS.10, na.rm = TRUE),
    Q3 = quantile(T0_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T0_PSS.10, na.rm = TRUE)
  )
T0_by_sep_with_family


T1_by_sep_with_family <- valid_data %>%
  group_by(T1_Q11) %>%
  summarise(
    N_missing = sum(is.na(T1_PSS.10)),
    N = sum(!is.na(T1_PSS.10)),
    Mean = mean(T1_PSS.10, na.rm = TRUE),
    SD = sd(T1_PSS.10, na.rm = TRUE),
    Min = min(T1_PSS.10, na.rm = TRUE),
    Q1 = quantile(T1_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T1_PSS.10, na.rm = TRUE),
    Q3 = quantile(T1_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T1_PSS.10, na.rm = TRUE)
  )
T1_by_sep_with_family


T2_by_sep_with_family <- valid_data %>%
  group_by(T1_Q11) %>%
  summarise(
    N_missing = sum(is.na(T2_PSS.10)),
    N = sum(!is.na(T2_PSS.10)),
    Mean = mean(T2_PSS.10, na.rm = TRUE),
    SD = sd(T2_PSS.10, na.rm = TRUE),
    Min = min(T2_PSS.10, na.rm = TRUE),
    Q1 = quantile(T2_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T2_PSS.10, na.rm = TRUE),
    Q3 = quantile(T2_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T2_PSS.10, na.rm = TRUE)
  )
T2_by_sep_with_family


T3_by_sep_with_family <- valid_data %>%
  group_by(T1_Q11) %>%
  summarise(
    N_missing = sum(is.na(T3_PSS.10)),
    N = sum(!is.na(T3_PSS.10)),
    Mean = mean(T3_PSS.10, na.rm = TRUE),
    SD = sd(T3_PSS.10, na.rm = TRUE),
    Min = min(T3_PSS.10, na.rm = TRUE),
    Q1 = quantile(T3_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T3_PSS.10, na.rm = TRUE),
    Q3 = quantile(T3_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T3_PSS.10, na.rm = TRUE)
  )
T3_by_sep_with_family



# Створимо список для результатів
wilcox_results_sep_with_family <- list()

selected_members <- c("Ні", "Так")

# Цикл по к-сті членів сім"ї
for (uni in selected_members) {
  
  df_uni <- family_long %>%
    filter(T1_Q11 == uni)
  
  # Тест Вілкоксона з попарними порівняннями між часами
  res_uni <- df_uni %>%
    pairwise_wilcox_test(
      Score ~ Time,
      paired = TRUE,          # якщо різні учасники
      p.adjust.method = "bonferroni"
    ) %>%
    mutate(T1_Q11 = uni)
  
  wilcox_results_sep_with_family[[uni]] <- res_uni
}

# Об’єднати результати в одну таблицю
wilcox_results_sep_with_family_all <- bind_rows(wilcox_results_sep_with_family)
wilcox_results_sep_with_family_all
#write.csv(wilcox_results_sep_with_family_all, "wilcox_results_sep_with_family.csv", row.names = FALSE)


# Чи є відмінність між рівнями для кожного етапу?
# Виконаємо тест Вілкоксона для кожного Time 
wilcox_results <- univ_long %>%
  group_by(Time) %>%
  wilcox_test(Score ~ T1_Q11) %>%                       # value - числові значення PSS
  adjust_pvalue(method = "bonferroni") %>%              # корекція p-value, за потреби
  add_significance("p.adj")

wilcox_results
#write.csv(wilcox_results, "wilcox_results_university.csv", row.names = FALSE)



############## Чи хтось із Ваших родичів проходить/проходив/проходила службу в Збройних силах України або у інших збройних формуваннях?

# Попарні boxplot для кожного Time
ggplot(univ_long, aes(x = T1_Q12, y = Score, fill = T1_Q12)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ Time, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10",
       x =  "Родичі в ЗСУ",
       y = "PSS-10") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q12_1.png", width = 10, height = 4, dpi = 300)

ggplot(univ_long, aes(x = Time, y = Score, fill = T1_Q12)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ T1_Q12, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10",
       x = NULL,
       y = "PSS-10",
       fill = "Родичі в ЗСУ") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q5_Стать.png", width = 10, height = 4, dpi = 300)
ggsave("Q12_2.png", width = 10, height = 4, dpi = 300)

T0_by_zsu <- valid_data %>%
  group_by(T1_Q12) %>%
  summarise(
    N_missing = sum(is.na(T0_PSS.10)),
    N = sum(!is.na(T0_PSS.10)),
    Mean = mean(T0_PSS.10, na.rm = TRUE),
    SD = sd(T0_PSS.10, na.rm = TRUE),
    Min = min(T0_PSS.10, na.rm = TRUE),
    Q1 = quantile(T0_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T0_PSS.10, na.rm = TRUE),
    Q3 = quantile(T0_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T0_PSS.10, na.rm = TRUE)
  )
T0_by_zsu


T1_by_zsu <- valid_data %>%
  group_by(T1_Q12) %>%
  summarise(
    N_missing = sum(is.na(T1_PSS.10)),
    N = sum(!is.na(T1_PSS.10)),
    Mean = mean(T1_PSS.10, na.rm = TRUE),
    SD = sd(T1_PSS.10, na.rm = TRUE),
    Min = min(T1_PSS.10, na.rm = TRUE),
    Q1 = quantile(T1_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T1_PSS.10, na.rm = TRUE),
    Q3 = quantile(T1_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T1_PSS.10, na.rm = TRUE)
  )
T1_by_zsu


T2_by_zsu <- valid_data %>%
  group_by(T1_Q12) %>%
  summarise(
    N_missing = sum(is.na(T2_PSS.10)),
    N = sum(!is.na(T2_PSS.10)),
    Mean = mean(T2_PSS.10, na.rm = TRUE),
    SD = sd(T2_PSS.10, na.rm = TRUE),
    Min = min(T2_PSS.10, na.rm = TRUE),
    Q1 = quantile(T2_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T2_PSS.10, na.rm = TRUE),
    Q3 = quantile(T2_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T2_PSS.10, na.rm = TRUE)
  )
T2_by_zsu


T3_by_zsu <- valid_data %>%
  group_by(T1_Q12) %>%
  summarise(
    N_missing = sum(is.na(T3_PSS.10)),
    N = sum(!is.na(T3_PSS.10)),
    Mean = mean(T3_PSS.10, na.rm = TRUE),
    SD = sd(T3_PSS.10, na.rm = TRUE),
    Min = min(T3_PSS.10, na.rm = TRUE),
    Q1 = quantile(T3_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T3_PSS.10, na.rm = TRUE),
    Q3 = quantile(T3_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T3_PSS.10, na.rm = TRUE)
  )
T3_by_zsu


# Чи є відмінність між рівнями для кожного етапу?
# Виконаємо тест Вілкоксона для кожного Time 
wilcox_results <- univ_long %>%
  group_by(Time) %>%
  wilcox_test(Score ~ T1_Q12) %>%                       # value - числові значення PSS
  adjust_pvalue(method = "bonferroni") %>%              # корекція p-value, за потреби
  add_significance("p.adj")

wilcox_results

# Створимо список для результатів
wilcox_results_sep <- list()

selected_members <- c("Ні", "Так")

# Цикл по к-сті членів сім"ї
for (uni in selected_members) {
  
  df_uni <- family_long %>%
    filter(T1_Q12 == uni)
  
  # Тест Вілкоксона з попарними порівняннями між часами
  res_uni <- df_uni %>%
    pairwise_wilcox_test(
      Score ~ Time,
      paired = TRUE,          # якщо різні учасники
      p.adjust.method = "bonferroni"
    ) %>%
    mutate(T1_Q12 = uni)
  
  wilcox_results_sep[[uni]] <- res_uni
}

# Об’єднати результати в одну таблицю
wilcox_results_sep_all <- bind_rows(wilcox_results_sep)
wilcox_results_sep_all
#write.csv(wilcox_results_sep_with_family_all, "wilcox_results_sep_with_family.csv", row.names = FALSE)




################## Чи змінювали Ви навчальний заклад під час війни?

# Попарні boxplot для кожного Time
ggplot(univ_long, aes(x = T1_Q14, y = Score, fill = T1_Q14)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ Time, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10",
       x =  "Зміна навчального закладу",
       y = "PSS-10") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q14_1.png", width = 10, height = 4, dpi = 300)

ggplot(univ_long, aes(x = Time, y = Score, fill = T1_Q14)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ T1_Q14, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10",
       x = NULL,
       y = "PSS-10",
       fill = "Зміна навчального закладу") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q14_2.png", width = 10, height = 4, dpi = 300)


T0_by_change_vnz <- valid_data %>%
  group_by(T1_Q14) %>%
  summarise(
    N_missing = sum(is.na(T0_PSS.10)),
    N = sum(!is.na(T0_PSS.10)),
    Mean = mean(T0_PSS.10, na.rm = TRUE),
    SD = sd(T0_PSS.10, na.rm = TRUE),
    Min = min(T0_PSS.10, na.rm = TRUE),
    Q1 = quantile(T0_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T0_PSS.10, na.rm = TRUE),
    Q3 = quantile(T0_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T0_PSS.10, na.rm = TRUE)
  )
T0_by_change_vnz


T1_by_change_vnz <- valid_data %>%
  group_by(T1_Q14) %>%
  summarise(
    N_missing = sum(is.na(T1_PSS.10)),
    N = sum(!is.na(T1_PSS.10)),
    Mean = mean(T1_PSS.10, na.rm = TRUE),
    SD = sd(T1_PSS.10, na.rm = TRUE),
    Min = min(T1_PSS.10, na.rm = TRUE),
    Q1 = quantile(T1_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T1_PSS.10, na.rm = TRUE),
    Q3 = quantile(T1_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T1_PSS.10, na.rm = TRUE)
  )
T1_by_change_vnz


T2_by_change_vnz <- valid_data %>%
  group_by(T1_Q14) %>%
  summarise(
    N_missing = sum(is.na(T2_PSS.10)),
    N = sum(!is.na(T2_PSS.10)),
    Mean = mean(T2_PSS.10, na.rm = TRUE),
    SD = sd(T2_PSS.10, na.rm = TRUE),
    Min = min(T2_PSS.10, na.rm = TRUE),
    Q1 = quantile(T2_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T2_PSS.10, na.rm = TRUE),
    Q3 = quantile(T2_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T2_PSS.10, na.rm = TRUE)
  )
T2_by_change_vnz


T3_by_change_vnz <- valid_data %>%
  group_by(T1_Q14) %>%
  summarise(
    N_missing = sum(is.na(T3_PSS.10)),
    N = sum(!is.na(T3_PSS.10)),
    Mean = mean(T3_PSS.10, na.rm = TRUE),
    SD = sd(T3_PSS.10, na.rm = TRUE),
    Min = min(T3_PSS.10, na.rm = TRUE),
    Q1 = quantile(T3_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T3_PSS.10, na.rm = TRUE),
    Q3 = quantile(T3_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T3_PSS.10, na.rm = TRUE)
  )
T3_by_change_vnz


# Чи є відмінність між рівнями для кожного етапу?
# Виконаємо тест Вілкоксона для кожного Time 
wilcox_results <- univ_long %>%
  group_by(Time) %>%
  wilcox_test(Score ~ T1_Q14) %>%                       # value - числові значення PSS
  adjust_pvalue(method = "bonferroni") %>%              # корекція p-value, за потреби
  add_significance("p.adj")

wilcox_results

# Створимо список для результатів
wilcox_results_sep <- list()

selected_members <- c("Ні", "Так")

# Цикл по к-сті членів сім"ї
for (uni in selected_members) {
  
  df_uni <- family_long %>%
    filter(T1_Q14 == uni)
  
  # Тест Вілкоксона з попарними порівняннями між часами
  res_uni <- df_uni %>%
    pairwise_wilcox_test(
      Score ~ Time,
      paired = TRUE,          # якщо різні учасники
      p.adjust.method = "bonferroni"
    ) %>%
    mutate(T1_Q14 = uni)
  
  wilcox_results_sep[[uni]] <- res_uni
}

# Об’єднати результати в одну таблицю
wilcox_results_sep_all <- bind_rows(wilcox_results_sep)
wilcox_results_sep_all
#write.csv(wilcox_results_sep_with_family_all, "wilcox_results_sep_with_family.csv", row.names = FALSE)






############### Вкажіть Ваш сімейний стан

# Попарні boxplot для кожного Time
ggplot(univ_long, aes(x = T1_Q15, y = Score, fill = T1_Q15)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ Time, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10",
       x =  "сімейний стан",
       y = "PSS-10") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q15_1.png", width = 10, height = 4, dpi = 300)

ggplot(univ_long, aes(x = Time, y = Score, fill = T1_Q15)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ T1_Q15, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10",
       x = NULL,
       y = "PSS-10",
       fill = "сімейний стан") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q15_2.png", width = 10, height = 4, dpi = 300)

T0_by_sim_stan <- valid_data %>%
  group_by(T1_Q15) %>%
  summarise(
    N_missing = sum(is.na(T0_PSS.10)),
    N = sum(!is.na(T0_PSS.10)),
    Mean = mean(T0_PSS.10, na.rm = TRUE),
    SD = sd(T0_PSS.10, na.rm = TRUE),
    Min = min(T0_PSS.10, na.rm = TRUE),
    Q1 = quantile(T0_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T0_PSS.10, na.rm = TRUE),
    Q3 = quantile(T0_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T0_PSS.10, na.rm = TRUE)
  )
T0_by_sim_stan


T1_by_sim_stan <- valid_data %>%
  group_by(T1_Q15) %>%
  summarise(
    N_missing = sum(is.na(T1_PSS.10)),
    N = sum(!is.na(T1_PSS.10)),
    Mean = mean(T1_PSS.10, na.rm = TRUE),
    SD = sd(T1_PSS.10, na.rm = TRUE),
    Min = min(T1_PSS.10, na.rm = TRUE),
    Q1 = quantile(T1_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T1_PSS.10, na.rm = TRUE),
    Q3 = quantile(T1_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T1_PSS.10, na.rm = TRUE)
  )
T1_by_sim_stan


T2_by_sim_stan <- valid_data %>%
  group_by(T1_Q15) %>%
  summarise(
    N_missing = sum(is.na(T2_PSS.10)),
    N = sum(!is.na(T2_PSS.10)),
    Mean = mean(T2_PSS.10, na.rm = TRUE),
    SD = sd(T2_PSS.10, na.rm = TRUE),
    Min = min(T2_PSS.10, na.rm = TRUE),
    Q1 = quantile(T2_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T2_PSS.10, na.rm = TRUE),
    Q3 = quantile(T2_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T2_PSS.10, na.rm = TRUE)
  )
T2_by_sim_stan


T3_by_sim_stan <- valid_data %>%
  group_by(T1_Q15) %>%
  summarise(
    N_missing = sum(is.na(T3_PSS.10)),
    N = sum(!is.na(T3_PSS.10)),
    Mean = mean(T3_PSS.10, na.rm = TRUE),
    SD = sd(T3_PSS.10, na.rm = TRUE),
    Min = min(T3_PSS.10, na.rm = TRUE),
    Q1 = quantile(T3_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T3_PSS.10, na.rm = TRUE),
    Q3 = quantile(T3_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T3_PSS.10, na.rm = TRUE)
  )
T3_by_sim_stan


# Чи є відмінність між рівнями для кожного етапу?
# Виконаємо тест Вілкоксона для кожного Time 
wilcox_results <- univ_long %>%
  group_by(Time) %>%
  wilcox_test(Score ~ T1_Q15) %>%                       # value - числові значення PSS
  adjust_pvalue(method = "bonferroni") %>%              # корекція p-value, за потреби
  add_significance("p.adj")

wilcox_results
#write.csv(wilcox_results, "wilcox_results_sim_stan.csv", row.names = FALSE)




###################### Оберіть курс, на якому Ви навчаєтесь
# Попарні boxplot для кожного Time
ggplot(univ_long, aes(x = T1_Q16, y = Score, fill = T1_Q16)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ Time, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10",
       x =  "Курс",
       y = "PSS-10") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q16_1.png", width = 10, height = 4, dpi = 300)

ggplot(univ_long, aes(x = Time, y = Score, fill = T1_Q16)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ T1_Q16, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10",
       x = NULL,
       y = "PSS-10",
       fill = "Курс") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q16_2.png", width = 10, height = 4, dpi = 300)



T0_by_sim_stan <- valid_data %>%
  group_by(T1_Q16) %>%
  summarise(
    N_missing = sum(is.na(T0_PSS.10)),
    N = sum(!is.na(T0_PSS.10)),
    Mean = mean(T0_PSS.10, na.rm = TRUE),
    SD = sd(T0_PSS.10, na.rm = TRUE),
    Min = min(T0_PSS.10, na.rm = TRUE),
    Q1 = quantile(T0_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T0_PSS.10, na.rm = TRUE),
    Q3 = quantile(T0_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T0_PSS.10, na.rm = TRUE)
  )
T0_by_sim_stan

T1_by_sim_stan <- valid_data %>%
  group_by(T1_Q16) %>%
  summarise(
    N_missing = sum(is.na(T1_PSS.10)),
    N = sum(!is.na(T1_PSS.10)),
    Mean = mean(T1_PSS.10, na.rm = TRUE),
    SD = sd(T1_PSS.10, na.rm = TRUE),
    Min = min(T1_PSS.10, na.rm = TRUE),
    Q1 = quantile(T1_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T1_PSS.10, na.rm = TRUE),
    Q3 = quantile(T1_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T1_PSS.10, na.rm = TRUE)
  )
T1_by_sim_stan


T2_by_sim_stan <- valid_data %>%
  group_by(T1_Q16) %>%
  summarise(
    N_missing = sum(is.na(T2_PSS.10)),
    N = sum(!is.na(T2_PSS.10)),
    Mean = mean(T2_PSS.10, na.rm = TRUE),
    SD = sd(T2_PSS.10, na.rm = TRUE),
    Min = min(T2_PSS.10, na.rm = TRUE),
    Q1 = quantile(T2_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T2_PSS.10, na.rm = TRUE),
    Q3 = quantile(T2_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T2_PSS.10, na.rm = TRUE)
  )
T2_by_sim_stan



T3_by_sim_stan <- valid_data %>%
  group_by(T1_Q16) %>%
  summarise(
    N_missing = sum(is.na(T3_PSS.10)),
    N = sum(!is.na(T3_PSS.10)),
    Mean = mean(T3_PSS.10, na.rm = TRUE),
    SD = sd(T3_PSS.10, na.rm = TRUE),
    Min = min(T3_PSS.10, na.rm = TRUE),
    Q1 = quantile(T3_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T3_PSS.10, na.rm = TRUE),
    Q3 = quantile(T3_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T3_PSS.10, na.rm = TRUE)
  )
T3_by_sim_stan



# Чи є відмінність між рівнями для кожного етапу?
# Виконаємо тест Вілкоксона для кожного Time 
wilcox_results <- univ_long %>%
  group_by(Time) %>%
  wilcox_test(Score ~ T1_Q16) %>%                       # value - числові значення PSS
  adjust_pvalue(method = "bonferroni") %>%              # корекція p-value, за потреби
  add_significance("p.adj")

wilcox_results
#write.csv(wilcox_results, "wilcox_results_sim_stan.csv", row.names = FALSE)



###################### Вкажіть щомісячний дохід сім’ї

# Попарні boxplot для кожного Time
ggplot(univ_long, aes(x = T1_Q18, y = Score, fill = T1_Q18)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ Time, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10",
       x =  " дохід сім’ї",
       y = "PSS-10") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q18_1.png", width = 10, height = 4, dpi = 300)

ggplot(univ_long, aes(x = Time, y = Score, fill = T1_Q18)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ T1_Q18, nrow = 1) +  # окремий графік для кожного Time
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # вертикальні підписи
  labs(title = "PSS-10",
       x = NULL,
       y = "PSS-10",
       fill = " дохід сім’ї") +
  coord_cartesian(ylim = c(0, 40))
ggsave("Q18_2.png", width = 10, height = 4, dpi = 300)

T0_by_sim_stan <- valid_data %>%
  group_by(T1_Q18) %>%
  summarise(
    N_missing = sum(is.na(T0_PSS.10)),
    N = sum(!is.na(T0_PSS.10)),
    Mean = mean(T0_PSS.10, na.rm = TRUE),
    SD = sd(T0_PSS.10, na.rm = TRUE),
    Min = min(T0_PSS.10, na.rm = TRUE),
    Q1 = quantile(T0_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T0_PSS.10, na.rm = TRUE),
    Q3 = quantile(T0_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T0_PSS.10, na.rm = TRUE)
  )
T0_by_sim_stan


T1_by_sim_stan <- valid_data %>%
  group_by(T1_Q18) %>%
  summarise(
    N_missing = sum(is.na(T1_PSS.10)),
    N = sum(!is.na(T1_PSS.10)),
    Mean = mean(T1_PSS.10, na.rm = TRUE),
    SD = sd(T1_PSS.10, na.rm = TRUE),
    Min = min(T1_PSS.10, na.rm = TRUE),
    Q1 = quantile(T1_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T1_PSS.10, na.rm = TRUE),
    Q3 = quantile(T1_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T1_PSS.10, na.rm = TRUE)
  )
T1_by_sim_stan


T2_by_sim_stan <- valid_data %>%
  group_by(T1_Q18) %>%
  summarise(
    N_missing = sum(is.na(T2_PSS.10)),
    N = sum(!is.na(T2_PSS.10)),
    Mean = mean(T2_PSS.10, na.rm = TRUE),
    SD = sd(T2_PSS.10, na.rm = TRUE),
    Min = min(T2_PSS.10, na.rm = TRUE),
    Q1 = quantile(T2_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T2_PSS.10, na.rm = TRUE),
    Q3 = quantile(T2_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T2_PSS.10, na.rm = TRUE)
  )
T2_by_sim_stan



T3_by_sim_stan <- valid_data %>%
  group_by(T1_Q18) %>%
  summarise(
    N_missing = sum(is.na(T3_PSS.10)),
    N = sum(!is.na(T3_PSS.10)),
    Mean = mean(T3_PSS.10, na.rm = TRUE),
    SD = sd(T3_PSS.10, na.rm = TRUE),
    Min = min(T3_PSS.10, na.rm = TRUE),
    Q1 = quantile(T3_PSS.10, 0.25, na.rm = TRUE),
    Median = median(T3_PSS.10, na.rm = TRUE),
    Q3 = quantile(T3_PSS.10, 0.75, na.rm = TRUE),
    Max = max(T3_PSS.10, na.rm = TRUE)
  )
T3_by_sim_stan



# Чи є відмінність між рівнями для кожного етапу?
# Виконаємо тест Вілкоксона для кожного Time 
wilcox_results <- univ_long %>%
  group_by(Time) %>%
  wilcox_test(Score ~ T1_Q18) %>%                       # value - числові значення PSS
  adjust_pvalue(method = "bonferroni") %>%              # корекція p-value, за потреби
  add_significance("p.adj")

wilcox_results
#write.csv(wilcox_results, "wilcox_results_sim_stan.csv", row.names = FALSE)






library(dplyr)
library(ggplot2)
library(corrplot)   # для візуалізації кореляційної матриці
library(rstatix)    # для cor_test (з p-value)
library(ggcorrplot) # альтернативна красива візуалізація

# ============================================================
# КОРЕЛЯЦІЯ СПІРМЕНА між психометричними шкалами
# ============================================================

# 1. ПІДГОТОВКА ДАНИХ
#    Вибираємо лише числові індекси шкал з вашого merged датасету

df_scales <- valid_data %>%
  select(
    T1_PSS.10, T1_PHQ.9, T1_GAD.7, T1_PCL.5,
    T1_WHO.5,  T1_WHODAS, T1_CD.RISC.10, T1_CRAFFT,
    T2_PSS.10, T2_PHQ.9, T2_GAD.7, T2_PCL.5,
    T2_WHO.5,  T2_WHODAS, T2_CD.RISC.10, T2_CRAFFT,
    T3_PSS.10, T3_PHQ.9, T3_GAD.7, T3_PCL.5,
    T3_WHO.5,  T3_WHODAS, T3_CD.RISC.10, T3_CRAFFT
  )

# 2. КОРЕЛЯЦІЯ МІЖ ШКАЛАМИ НА КОЖНОМУ ЕТАПІ ОКРЕМО

# ---------- ЕТАП T1 ----------
df_t1 <- df_scales %>%
  select(T1_PSS.10, T1_PHQ.9, T1_GAD.7, T1_PCL.5,
         T1_WHO.5, T1_WHODAS, T1_CD.RISC.10, T1_CRAFFT) %>%
  rename(
    "PSS-10"     = T1_PSS.10,
    "PHQ-9"      = T1_PHQ.9,
    "GAD-7"      = T1_GAD.7,
    "PCL-5"      = T1_PCL.5,
    "WHO-5"      = T1_WHO.5,
    "WHODAS"     = T1_WHODAS,
    "CD-RISC-10" = T1_CD.RISC.10,
    "CRAFFT"     = T1_CRAFFT
  )

cor_t1 <- cor(df_t1, method = "spearman", use = "pairwise.complete.obs")
print("=== Кореляційна матриця Спірмена — T1 ===")
round(cor_t1, 2)

# ---------- ЕТАП T2 ----------
df_t2 <- df_scales %>%
  select(T2_PSS.10, T2_PHQ.9, T2_GAD.7, T2_PCL.5,
         T2_WHO.5, T2_WHODAS, T2_CD.RISC.10, T2_CRAFFT) %>%
  rename(
    "PSS-10"     = T2_PSS.10,
    "PHQ-9"      = T2_PHQ.9,
    "GAD-7"      = T2_GAD.7,
    "PCL-5"      = T2_PCL.5,
    "WHO-5"      = T2_WHO.5,
    "WHODAS"     = T2_WHODAS,
    "CD-RISC-10" = T2_CD.RISC.10,
    "CRAFFT"     = T2_CRAFFT
  )

cor_t2 <- cor(df_t2, method = "spearman", use = "pairwise.complete.obs")
print("=== Кореляційна матриця Спірмена — T2 ===")
round(cor_t2, 2)

# ---------- ЕТАП T3 ----------
df_t3 <- df_scales %>%
  select(T3_PSS.10, T3_PHQ.9, T3_GAD.7, T3_PCL.5,
         T3_WHO.5, T3_WHODAS, T3_CD.RISC.10, T3_CRAFFT) %>%
  rename(
    "PSS-10"     = T3_PSS.10,
    "PHQ-9"      = T3_PHQ.9,
    "GAD-7"      = T3_GAD.7,
    "PCL-5"      = T3_PCL.5,
    "WHO-5"      = T3_WHO.5,
    "WHODAS"     = T3_WHODAS,
    "CD-RISC-10" = T3_CD.RISC.10,
    "CRAFFT"     = T3_CRAFFT
  )

cor_t3 <- cor(df_t3, method = "spearman", use = "pairwise.complete.obs")
print("=== Кореляційна матриця Спірмена — T3 ===")
round(cor_t3, 2)


# ============================================================
# 3. P-VALUE ДЛЯ КОЖНОЇ КОРЕЛЯЦІЇ (з пакету rstatix)
#    Важливо для дипломної — показує значущість зв'язку
# ============================================================

# T1
cor_test_t1 <- df_t1 %>%
  cor_test(method = "spearman")
print("=== Значущість кореляцій — T1 ===")
print(cor_test_t1)
cor_test_t1 %>%
  filter(p > 0.05)
# T2
cor_test_t2 <- df_t2 %>%
  cor_test(method = "spearman")
print("=== Значущість кореляцій — T2 ===")
print(cor_test_t2)

# T3
cor_test_t3 <- df_t3 %>%
  cor_test(method = "spearman")
print("=== Значущість кореляцій — T3 ===")
print(cor_test_t3)

# Зберегти результати у CSV
#write.csv(cor_test_t1, "spearman_pvalues_T1.csv", row.names = FALSE)
#write.csv(cor_test_t2, "spearman_pvalues_T2.csv", row.names = FALSE)
#write.csv(cor_test_t3, "spearman_pvalues_T3.csv", row.names = FALSE)

# ============================================================
# 4.ВІЗУАЛІЗАЦІЯ (ggcorrplot) — для публікацій
# ============================================================
# --- T1 ---
corrplot(cor_t1,
         method = "color",        # кольорові клітинки
         type   = "upper",        # тільки верхній трикутник (без дублювання)
         order  = "hclust",       # групує схожі змінні разом
         addCoef.col = "black",   # показує числові значення коефіцієнтів
         tl.col = "black",        # колір підписів
         tl.srt = 45,             # кут підписів
         diag = FALSE,            # без діагоналі
         col = colorRampPalette(c("#D73027", "white", "#1A9850"))(200),
         title = "Кореляція Спірмена між шкалами — T1",
         mar = c(0, 0, 2, 0))
# Матриця p-value для T1
p_mat_t1 <- as.matrix(cor_pmat(df_t1, method = "spearman"))
ggcorrplot(cor_t1,
           method = "square",
           type   = "upper",
           p.mat  = p_mat_t1,       # несignificant кореляції позначаються хрестиком
           sig.level = 0.05,
           insig  = "pch",        # або "pch" щоб показати хрестик
           lab    = TRUE,           # показує числові значення
           lab_size = 3,
           colors = c("#D73027", "white", "#1A9850"),
           title  = "Кореляція Спірмена між шкалами — T1 (p < 0.05)",
           ggtheme = theme_minimal())

# Зберегти графік
ggsave("corrplot_T1.png", width = 8, height = 6, dpi = 300)

p_mat_t2 <- as.matrix(cor_pmat(df_t2, method = "spearman"))
ggcorrplot(cor_t2,
           method = "square",
           type   = "upper",
           p.mat  = p_mat_t2,       # несignificant кореляції позначаються хрестиком
           sig.level = 0.05,
           insig  = "pch",        # або "pch" щоб показати хрестик
           lab    = TRUE,           # показує числові значення
           lab_size = 3,
           colors = c("#D73027", "white", "#1A9850"),
           title  = "Кореляція Спірмена між шкалами — T2 (p < 0.05)",
           ggtheme = theme_minimal())

# Зберегти графік
ggsave("corrplot_T2.png", width = 8, height = 6, dpi = 300)

p_mat_t3 <- as.matrix(cor_pmat(df_t3, method = "spearman"))
ggcorrplot(cor_t3,
           method = "square",
           type   = "upper",
           p.mat  = p_mat_t3,       # несignificant кореляції позначаються хрестиком
           sig.level = 0.05,
           insig  = "pch",        # або "pch" щоб показати хрестик
           lab    = TRUE,           # показує числові значення
           lab_size = 3,
           colors = c("#D73027", "white", "#1A9850"),
           title  = "Кореляція Спірмена між шкалами — T3 (p < 0.05)",
           ggtheme = theme_minimal())

# Зберегти графік
ggsave("corrplot_T3.png", width = 8, height = 6, dpi = 300)

# ============================================================
# 5. КОРЕЛЯЦІЯ PSS-10 МІЖ ЕТАПАМИ
#    (чи стабільний рейтинг стресу з часом?)
# ============================================================

df_pss_time <- valid_data %>%
  select(T1_PSS.10, T2_PSS.10, T3_PSS.10)

cor_pss_time <- cor(df_pss_time, method = "spearman", use = "pairwise.complete.obs")
print("=== Кореляція PSS-10 між часовими точками ===")
round(cor_pss_time, 2)

# p-value
df_pss_time %>% cor_test(method = "spearman")


# ============================================================
# ІНТЕРПРЕТАЦІЯ РЕЗУЛЬТАТІВ
# ============================================================
# ρ = 0.00–0.19 → дуже слабкий зв'язок
# ρ = 0.20–0.39 → слабкий зв'язок
# ρ = 0.40–0.59 → помірний зв'язок
# ρ = 0.60–0.79 → сильний зв'язок
# ρ = 0.80–1.00 → дуже сильний зв'язок
# Від'ємний знак → зворотній зв'язок (напр. більше стресу → менший добробут)
# p < 0.05      → зв'язок є статистично значущим












# ============================================================
# LINEAR MIXED MODELS — ITT підхід
# ============================================================
# Baseline:
#   PSS-10 -> baseline = T0 (скринінг), outcomes = T1, T2, T3
#   Решта  -> baseline = T1,             outcomes = T2, T3
#
# Основна формула:
#   Score ~ Baseline + Time_fac + (1 | participant_id)
#
# Додатково:
#   weights = varIdent(~1 | Time_fac) -> різні дисперсії по хвилях
#   (Purgato et al., 2021: "distinct variances for each timepoint")
# ============================================================

library(dplyr)
library(tidyr)
library(tibble)
library(nlme)
library(emmeans)


# 1. ITT-вибірка
itt_data <- merged %>%
  filter(
    !is.na(T0_PSS.10),
    T0_PSS.10 >= 8,
    T0_PSS.10 <= 26,
    (!is.na(T1_PSS.10) | !is.na(T2_PSS.10) | !is.na(T3_PSS.10))
  ) %>%
  mutate(participant_id = row_number())

cat("Загалом учасників:                N =", nrow(itt_data), "\n")
cat("Мають T1 PSS-10:                  N =", sum(!is.na(itt_data$T1_PSS.10)), "\n")
cat("Мають T2 PSS-10:                  N =", sum(!is.na(itt_data$T2_PSS.10)), "\n")
cat("Мають T3 PSS-10:                  N =", sum(!is.na(itt_data$T3_PSS.10)), "\n")
cat("Complete cases PSS-10 (T1+T2+T3): N =",
    sum(!is.na(itt_data$T1_PSS.10) &
          !is.na(itt_data$T2_PSS.10) &
          !is.na(itt_data$T3_PSS.10)), "\n\n")


# 2. WIDE -> LONG

# pss_mode = TRUE  (PSS-10):
#   Baseline = T0 (скринінговий бал, окрема колонка)
#   Outcomes = T1, T2, T3 | Референс = T1
#   Таблиця коефіцієнтів: Time_facT2 (T1→T2), Time_facT3 (T1→T3)
#
# pss_mode = FALSE (решта шкал) — підхід ANCOVA:
#   Baseline = T1 (числова коваріата, НЕ потрапляє в pivot)
#   Outcomes = T2, T3 | Референс = T2
#   Таблиця коефіцієнтів: Time_facT3 (T2→T3)
#   Зміну T1→T2 та T1→T3 видно через EMM + post-hoc (pairs)
# ============================================================
make_long <- function(data, scale_suffix, pss_mode = FALSE) {
  
  col_t1 <- paste0("T1_", scale_suffix)
  col_t2 <- paste0("T2_", scale_suffix)
  col_t3 <- paste0("T3_", scale_suffix)
  
  needed_cols <- if (pss_mode) c(col_t1, col_t2, col_t3, "T0_PSS.10")
  else          c(col_t1, col_t2, col_t3)
  
  missing_cols <- setdiff(needed_cols, names(data))
  if (length(missing_cols) > 0)
    stop(paste("Відсутні колонки:", paste(missing_cols, collapse = ", ")))
  
  demo_vars <- intersect(
    c("participant_id", "T1_University",
      "T1_Q5", "T1_Q7", "T1_Q8", "T1_Q9",
      "T1_Q15", "T1_Q16", "T1_Q18"),
    names(data)
  )
  
  if (pss_mode) {
    data %>%
      select(all_of(demo_vars), all_of(c(col_t1, col_t2, col_t3)), T0_PSS.10) %>%
      mutate(Baseline = T0_PSS.10) %>%
      pivot_longer(
        cols      = all_of(c(col_t1, col_t2, col_t3)),
        names_to  = "Wave",
        values_to = "Score"
      ) %>%
      mutate(
        Time_num = case_when(Wave == col_t1 ~ 1L,
                             Wave == col_t2 ~ 2L,
                             Wave == col_t3 ~ 3L),
        Time_fac = factor(Time_num, levels = 1:3, labels = c("T1", "T2", "T3"))
      ) %>%
      filter(!is.na(Score), !is.na(Baseline))
    
  } else {
    data %>%
      select(all_of(demo_vars), all_of(c(col_t1, col_t2, col_t3))) %>%
      mutate(Baseline = .data[[col_t1]]) %>%
      pivot_longer(
        cols      = all_of(c(col_t2, col_t3)),
        names_to  = "Wave",
        values_to = "Score"
      ) %>%
      mutate(
        Time_num = case_when(Wave == col_t2 ~ 2L,
                             Wave == col_t3 ~ 3L),
        Time_fac = factor(Time_num, levels = 2:3, labels = c("T2", "T3"))
      ) %>%
      filter(!is.na(Score), !is.na(Baseline))
  }
}


# 3. LMM

run_lmm <- function(data_long, scale_name = "Scale") {
  
  if (nrow(data_long) == 0) {
    message(scale_name, ": немає даних після підготовки"); return(NULL)
  }
  if (dplyr::n_distinct(data_long$participant_id) < 2) {
    message(scale_name, ": недостатньо учасників для LMM"); return(NULL)
  }
  if (nlevels(droplevels(data_long$Time_fac)) < 2) {
    message(scale_name, ": недостатньо часових точок"); return(NULL)
  }
  
  model <- tryCatch(
    nlme::lme(
      fixed     = Score ~ Baseline + Time_fac,
      random    = ~ 1 | participant_id,
      data      = data_long,
      weights   = varIdent(form = ~ 1 | Time_fac),
      na.action = na.omit,
      method    = "REML",
      control   = lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
    ),
    error = function(e) { message(scale_name, " [ПОМИЛКА]: ", e$message); NULL }
  )
  if (is.null(model)) return(NULL)
  
  coef_tbl <- as.data.frame(summary(model)$tTable) %>%
    rownames_to_column("Term")
  
  # Стандартизовані коефіцієнти (аналог stdBeta зі Stata):
  #   Baseline: beta * SD(Baseline) / SD(Score)
  #   Time:     beta / SD(Score)
  sd_y  <- sd(data_long$Score,    na.rm = TRUE)
  sd_bl <- sd(data_long$Baseline, na.rm = TRUE)
  
  coef_tbl <- coef_tbl %>%
    mutate(
      Std_Coef = case_when(
        Term == "Baseline" & sd_bl > 0 & sd_y > 0 ~ Value * sd_bl / sd_y,
        grepl("^Time_fac", Term) & sd_y > 0        ~ Value / sd_y,
        TRUE                                        ~ NA_real_
      )
    )
  
  emm <- emmeans(model, ~ Time_fac)
  
  list(
    model    = model,
    coef_tbl = coef_tbl,
    emm      = emm,
    pairs    = pairs(emm, adjust = "bonferroni"),
    n_obs    = nrow(data_long),
    n_subj   = dplyr::n_distinct(data_long$participant_id),
    sd_y     = sd_y
  )
}


# 4. ЗАПУСК ДЛЯ ВСІХ ШКАЛ

scales <- list(
  list(suffix = "PSS.10",     name = "PSS-10",     pss_mode = TRUE),
  list(suffix = "PHQ.9",      name = "PHQ-9",      pss_mode = FALSE),
  list(suffix = "GAD.7",      name = "GAD-7",      pss_mode = FALSE),
  list(suffix = "PCL.5",      name = "PCL-5",      pss_mode = FALSE),
  list(suffix = "CD.RISC.10", name = "CD-RISC-10", pss_mode = FALSE),
  list(suffix = "WHO.5",      name = "WHO-5",       pss_mode = FALSE),
  list(suffix = "WHODAS",     name = "WHODAS",      pss_mode = FALSE),
  list(suffix = "CRAFFT",     name = "CRAFFT",      pss_mode = FALSE)
)

results <- list()
for (sc in scales) {
  long_df <- tryCatch(
    make_long(itt_data, sc$suffix, sc$pss_mode),
    error = function(e) { message(sc$name, ": ", e$message); NULL }
  )
  if (!is.null(long_df) && nrow(long_df) > 0)
    results[[sc$name]] <- run_lmm(long_df, sc$name)
}

results


# 5. ЗВЕДЕНА ТАБЛИЦЯ 
table1 <- bind_rows(lapply(names(results), function(nm) {
  res <- results[[nm]]
  if (is.null(res)) return(NULL)
  res$coef_tbl %>%
    filter(grepl("^Baseline$|^Time_fac", Term)) %>%
    mutate(Scale  = nm,
           N_obs  = res$n_obs,
           N_subj = res$n_subj) %>%
    select(Scale, Term, N_obs, N_subj,
           Coefficient = Value,
           SE          = Std.Error,
           t_value     = `t-value`,
           p_value     = `p-value`,
           Std_Coef)
})) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

table1
#write.csv(table1, "lmm_table1.csv", row.names = FALSE)

# ============================================================
# 6. МОДЕРАТОРНИЙ АНАЛІЗ (LRT)
# ============================================================
# Модель основна (без взаємодії):
#   Score_ij = β0 + β1*Baseline_i + β2*Time_ij + β3*Moderator_i
#              + u_i + ε_ij
#
# Модель з взаємодією:
#   Score_ij = β0 + β1*Baseline_i + β2*Time_ij + β3*Moderator_i
#              + β4*(Time_ij × Moderator_i) + u_i + ε_ij
#
# де:
#   i         — учасник, j — часова точка
#   u_i       ~ N(0, σ²_u) — випадковий ефект учасника
#   ε_ij      ~ N(0, σ²)   — залишкова похибка
#   β4        — ефект модерації: чи відрізняється динаміка
#               змін у часі між рівнями модератора?
#
# Перевірка взаємодії (Time × Moderator) виконується через LRT:
#   anova(m_main, m_inter) — порівняння logLik двох моделей
#   Значущий LRT (p < .05) → динаміка змін залежить від модератора
#
# Метод = ML (не REML) для коректного порівняння моделей через LRT
# ============================================================
run_moderator_lrt <- function(data_long, moderator_var, scale_name = "Scale") {
  
  if (!moderator_var %in% names(data_long)) {
    message(scale_name, " / ", moderator_var, ": змінна не знайдена")
    return(NULL)
  }
  
  d <- data_long %>%
    filter(!is.na(.data[[moderator_var]])) %>%
    mutate(Moderator = as.factor(.data[[moderator_var]])) %>%
    droplevels()
  
  if (nlevels(d$Moderator) < 2) {
    message(scale_name, " / ", moderator_var, ": < 2 рівнів модератора")
    return(NULL)
  }
  if (nlevels(droplevels(d$Time_fac)) < 2) {
    message(scale_name, " / ", moderator_var, ": < 2 часових точок")
    return(NULL)
  }
  
  ctrl <- lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
  wgt  <- varIdent(form = ~ 1 | Time_fac)
  
  m_main <- tryCatch(
    nlme::lme(
      fixed     = Score ~ Baseline + Time_fac + Moderator,
      random    = ~ 1 | participant_id,
      data      = d, weights = wgt, na.action = na.omit,
      method    = "ML", control = ctrl
    ),
    error = function(e) {
      message(scale_name, " / ", moderator_var, " [main]: ", e$message); NULL
    }
  )
  
  m_inter <- tryCatch(
    nlme::lme(
      fixed     = Score ~ Baseline + Time_fac * Moderator,
      random    = ~ 1 | participant_id,
      data      = d, weights = wgt, na.action = na.omit,
      method    = "ML", control = ctrl
    ),
    error = function(e) {
      message(scale_name, " / ", moderator_var, " [interaction]: ", e$message); NULL
    }
  )
  
  if (is.null(m_main) || is.null(m_inter)) return(NULL)
  anova(m_main, m_inter)
}

moderators <- c(
  "T1_University" = "Університет / місто",
  "T1_Q5"         = "Стать",
  "T1_Q9"         = "Статус ВПО",
  "T1_Q15"        = "Сімейний стан",
  "T1_Q16"        = "Курс навчання",
  "T1_Q18"        = "Дохід сім'ї"
)

moderator_results <- list()

for (sc in scales) {
  long_df <- tryCatch(
    make_long(itt_data, sc$suffix, sc$pss_mode),
    error = function(e) { message(sc$name, ": ", e$message); NULL }
  )
  if (is.null(long_df) || nrow(long_df) == 0) next
  
  moderator_results[[sc$name]] <- lapply(
    setNames(names(moderators), names(moderators)),
    function(var) run_moderator_lrt(long_df, var, sc$name)
  )
}

moderator_table <- bind_rows(
  lapply(names(moderator_results), function(scale_nm) {
    scale_res <- moderator_results[[scale_nm]]
    
    bind_rows(lapply(names(scale_res), function(var) {
      lrt <- scale_res[[var]]
      if (is.null(lrt)) return(NULL)
      
      # Беремо рядок з результатом порівняння (2-й рядок)
      data.frame(
        Scale      = scale_nm,
        Moderator  = moderators[[var]],   # читабельна назва
        Variable   = var,                 # технічна назва
        df_diff    = diff(lrt$df),
        L.Ratio    = round(lrt$L.Ratio[2], 4),
        p_value    = round(lrt$`p-value`[2], 4),
        Significant = ifelse(lrt$`p-value`[2] < .05, "✓", "")
      )
    }))
  })
)

print(moderator_table, row.names = FALSE)




# ============================================================
# 7. ТАБЛИЦІ EMM ТА POST-HOC (для диплому / статті)
# ============================================================
# EMM (Estimated Marginal Means) — скориговані середні по хвилях,
# усереднені по всіх учасниках при контролі Baseline.
#
# pairs() — попарні порівняння між хвилями (Bonferroni):
#   PSS-10:       T1-T2, T1-T3, T2-T3
#   Решта шкал:   T2-T3
#
# Cohen's d = estimate / SD(Score) — наближена оцінка розміру ефекту
# (ділення на загальне SD по всіх хвилях; вказати це в тексті диплому)
# ============================================================
make_pretty_tables <- function(res, scale_name) {
  
  if (is.null(res)) return(NULL)
  
  emm_df <- as.data.frame(res$emm) %>%
    mutate(Scale = scale_name) %>%
    select(Scale,
           Time     = Time_fac,
           Mean     = emmean,
           SE,
           Lower_CI = lower.CL,
           Upper_CI = upper.CL)
  
  pairs_df <- as.data.frame(res$pairs) %>%
    mutate(Scale   = scale_name,
           Cohen_d = estimate / res$sd_y) %>%
    select(Scale,
           Contrast = contrast,
           Estimate = estimate,
           SE,
           t_ratio  = t.ratio,
           p_value  = p.value,
           Cohen_d)
  
  list(emm = emm_df, pairs = pairs_df)
}

pretty_results <- lapply(names(results),
                         function(nm) make_pretty_tables(results[[nm]], nm))

emm_table   <- bind_rows(lapply(pretty_results, `[[`, "emm"))
pairs_table <- bind_rows(lapply(pretty_results, `[[`, "pairs"))

emm_table
pairs_table
#write.csv(emm_table,   "lmm_emmeans.csv", row.names = FALSE)
#write.csv(pairs_table, "lmm_pairs.csv",   row.names = FALSE)


# ============================================================
# ПОРІВНЯННЯ Cohen's d: прямий (effectsize) vs LMM-based
# ============================================================
# Ідея: перевірити робастність результатів через два
# незалежні методи оцінки розміру ефекту.
# ============================================================

comparison_d <- cohens_d_results %>%
  # Переводимо переходи у формат, що йде з LMM ("T2 - T1" тощо)
  mutate(Contrast = case_when(
    Transition == "T0→T1" ~ "T0 - T1",
    Transition == "T1→T2" ~ "T1 - T2",
    Transition == "T1→T3" ~ "T1 - T3",
    Transition == "T2→T3" ~ "T2 - T3",
    TRUE ~ Transition
  )) %>%
  select(Scale, Contrast, 
         d_direct = d, 
         CI_low, CI_high) %>%
  # Підтягуємо LMM-оцінку d
  left_join(
    pairs_table %>% select(Scale, Contrast, d_LMM = Cohen_d),
    by = c("Scale", "Contrast")
  ) %>%
  mutate(
    `Різниця` = round(abs(d_direct - d_LMM), 3),
    `d_direct [95% CI]` = sprintf("%.2f [%.2f, %.2f]", d_direct, CI_low, CI_high),
    d_LMM_round = round(d_LMM, 2)
  ) %>%
  select(Шкала = Scale, Контраст = Contrast, 
         `Прямий d (з CI)` = `d_direct [95% CI]`,
         `LMM d` = d_LMM_round,
         `Різниця`)

print(comparison_d)


#install.packages("effectsize")



# ============================================================
# ПОДАЛЬШИЙ АНАЛІЗ ЗНАЧУЩИХ МОДЕРАТОРІВ
# ============================================================
# Значущі взаємодії (Time × Moderator, p < .05):
#   PSS-10  × T1_University (університет/місто)   p = .012
#   PCL-5   × T1_Q9         (статус ВПО)          p = .024
#   WHO-5   × T1_University (університет/місто)   p = .024
#   WHODAS  × T1_Q9         (статус ВПО)          p = .026
#
# Для кожного: будуємо модель з взаємодією → EMM по групах
# → попарні порівняння окремо для кожної групи
# ============================================================


analyze_significant_moderator <- function(data_long,
                                          moderator_var,
                                          moderator_label,
                                          scale_name) {
  
  # Підготовка даних
  d <- data_long %>%
    filter(!is.na(.data[[moderator_var]])) %>%
    mutate(Moderator = as.factor(.data[[moderator_var]])) %>%
    droplevels()
  
  # Модель з взаємодією (REML — для оцінки параметрів)
  model_inter <- tryCatch(
    nlme::lme(
      fixed   = Score ~ Baseline + Time_fac * Moderator,
      random  = ~ 1 | participant_id,
      data    = d,
      weights = varIdent(form = ~ 1 | Time_fac),
      method  = "REML",
      control = lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
    ),
    error = function(e) { message("Помилка: ", e$message); NULL }
  )
  if (is.null(model_inter)) return(NULL)
  
  # EMM окремо для кожної групи модератора
  # ~ Time_fac | Moderator → скориговані середні по хвилях для кожної групи
  emm_by_group <- emmeans(model_inter, ~ Time_fac | Moderator)
  
  # Попарні порівняння хвиль окремо для кожної групи
  pairs_by_group <- pairs(emm_by_group, adjust = "bonferroni")
  
  # Порівняння груп в кожній хвилі (чи відрізняються міста у T1? у T2? у T3?)
  emm_by_time <- emmeans(model_inter, ~ Moderator | Time_fac)
  pairs_by_time <- pairs(emm_by_time, adjust = "bonferroni")
  
  # Таблиця EMM для збереження
  emm_df <- as.data.frame(emm_by_group) %>%
    mutate(Scale = scale_name, Moderator_var = moderator_label) %>%
    select(Scale, Moderator_var,
           Group    = Moderator,
           Time     = Time_fac,
           Mean     = emmean,
           SE,
           Lower_CI = lower.CL,
           Upper_CI = upper.CL)
  
  # Таблиця попарних порівнянь по групах
  pairs_df <- as.data.frame(pairs_by_group) %>%
    mutate(Scale = scale_name, Moderator_var = moderator_label) %>%
    select(Scale, Moderator_var,
           Group    = Moderator,
           Contrast = contrast,
           Estimate = estimate,
           SE,
           t_ratio  = t.ratio,
           p_value  = p.value)
  
  list(
    model          = model_inter,
    emm_by_group   = emm_by_group,
    pairs_by_group = pairs_by_group,
    emm_by_time    = emm_by_time,
    pairs_by_time  = pairs_by_time,
    emm_df         = emm_df,
    pairs_df       = pairs_df
  )
}

# 1. PSS-10 × Університет / місто
pss_long <- make_long(itt_data, "PSS.10", pss_mode = TRUE)

mod_pss_university <- analyze_significant_moderator(
  data_long       = pss_long,
  moderator_var   = "T1_University",
  moderator_label = "Університет / місто",
  scale_name      = "PSS-10"
)

#cat("\n=== PSS-10 × Університет / місто ===\n")
#cat("\nEMM по хвилях для кожного міста:\n")
print(mod_pss_university$emm_by_group)

#cat("\nПорівняння хвиль окремо для кожного міста (Bonferroni):\n")
print(mod_pss_university$pairs_by_group)

#cat("\nПорівняння міст окремо для кожної хвилі (Bonferroni):\n")
print(mod_pss_university$pairs_by_time)

# 2. PCL-5 × Статус ВПО
pcl_long <- make_long(itt_data, "PCL.5", pss_mode = FALSE)

mod_pcl_vpo <- analyze_significant_moderator(
  data_long       = pcl_long,
  moderator_var   = "T1_Q9",
  moderator_label = "Статус ВПО",
  scale_name      = "PCL-5"
)

#cat("\n=== PCL-5 × Статус ВПО ===\n")
#cat("\nEMM по хвилях для кожної групи ВПО:\n")
print(mod_pcl_vpo$emm_by_group)

#cat("\nПорівняння хвиль окремо для кожної групи (Bonferroni):\n")
print(mod_pcl_vpo$pairs_by_group)

#cat("\nПорівняння груп ВПО окремо для кожної хвилі (Bonferroni):\n")
print(mod_pcl_vpo$pairs_by_time)

# 3. WHO-5 × Університет / місто
who5_long <- make_long(itt_data, "WHO.5", pss_mode = FALSE)

mod_who5_university <- analyze_significant_moderator(
  data_long       = who5_long,
  moderator_var   = "T1_University",
  moderator_label = "Університет / місто",
  scale_name      = "WHO-5"
)

#cat("\n=== WHO-5 × Університет / місто ===\n")
#cat("\nEMM по хвилях для кожного міста:\n")
print(mod_who5_university$emm_by_group)

#cat("\nПорівняння хвиль окремо для кожного міста (Bonferroni):\n")
print(mod_who5_university$pairs_by_group)

#cat("\nПорівняння міст окремо для кожної хвилі (Bonferroni):\n")
print(mod_who5_university$pairs_by_time)

# 4. WHODAS × Статус ВПО
whodas_long <- make_long(itt_data, "WHODAS", pss_mode = FALSE)

mod_whodas_vpo <- analyze_significant_moderator(
  data_long       = whodas_long,
  moderator_var   = "T1_Q9",
  moderator_label = "Статус ВПО",
  scale_name      = "WHODAS"
)

#cat("\n=== WHODAS × Статус ВПО ===\n")
#cat("\nEMM по хвилях для кожної групи ВПО:\n")
print(mod_whodas_vpo$emm_by_group)

#cat("\nПорівняння хвиль окремо для кожної групи (Bonferroni):\n")
print(mod_whodas_vpo$pairs_by_group)

#cat("\nПорівняння груп ВПО окремо для кожної хвилі (Bonferroni):\n")
print(mod_whodas_vpo$pairs_by_time)

# ============================================================
# ЗВЕДЕНІ ТАБЛИЦІ для диплому
# ============================================================
sig_emm_table <- bind_rows(
  mod_pss_university$emm_df,
  mod_pcl_vpo$emm_df,
  mod_who5_university$emm_df,
  mod_whodas_vpo$emm_df
) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))

sig_pairs_table <- bind_rows(
  mod_pss_university$pairs_df,
  mod_pcl_vpo$pairs_df,
  mod_who5_university$pairs_df,
  mod_whodas_vpo$pairs_df
) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))

sig_emm_table
sig_pairs_table

#write.csv(sig_emm_table,   "moderator_sig_emmeans.csv", row.names = FALSE)
#write.csv(sig_pairs_table, "moderator_sig_pairs.csv",   row.names = FALSE)





library(dplyr)
library(ggplot2)
library(psych)


# ============================================================
# АЛЬФА КРОНБАХА
# ============================================================
# Внутрішня узгодженість шкал перевіряється за коефіцієнтом
# альфа Кронбаха окремо для кожної хвилі (T1, T2, T3).
# Використовуємо valid_data — 386 учасників, які пройшли всі
# три хвилі і мали помірний рівень стресу на скринінгу.
#
# Оскільки пункти зберігаються як текст ("Ніколи", "Іноді" тощо),
# спочатку перекодовуємо їх у числа, а потім передаємо в alpha().

# --- Допоміжні функції перекодування -----------------------

r_pss <- function(x)
  recode(x, "Ніколи"=0L, "Майже ніколи"=1L, "Іноді"=2L,
         "Досить часто"=3L, "Дуже часто"=4L, .default=NA_integer_)

r_pss_rev <- function(x)   # зворотні пункти: Q23, Q24, Q26, Q27
  recode(x, "Ніколи"=4L, "Майже ніколи"=3L, "Іноді"=2L,
         "Досить часто"=1L, "Дуже часто"=0L, .default=NA_integer_)

r_phq <- function(x)
  recode(x, "ніколи"=0L, "кілька днів"=1L,
         "більше половини часу"=2L, "майже щодня"=3L, .default=NA_integer_)

r_gad <- function(x)
  recode(x, "Зовсім не турбували "=0L, "Декілька днів "=1L,
         "Більше половини усіх днів"=2L, "Майже щоденно"=3L, .default=NA_integer_)

r_pcl <- function(x)
  recode(x, "Жодним чином"=0L, "Зовсім трохи "=1L, "Трохи "=2L,
         "Досить відчутно"=3L, "Значним чином"=4L, .default=NA_integer_)

r_cdr <- function(x)
  recode(x, "Зовсім невірно"=0L, "Дуже рідко вірно"=1L, "Іноді вірно"=2L,
         "Часто вірно"=3L, "Майже завжди вірно"=4L, .default=NA_integer_)

r_who5 <- function(x)
  recode(x, "Весь час"=5L, "Більшість часу"=4L, "Більше половини часу"=3L,
         "Менше половини часу"=2L, "Певний час"=1L, "Ніколи"=0L, .default=NA_integer_)

r_whodas <- function(x)
  recode(x, "Зовсім ні"=0L, "Трохи"=1L, "Досить"=2L, "Дуже"=3L,
         "Неймовірно складно або не можу зробити"=4L, .default=NA_integer_)


# --- PSS-10 (Q20–Q29) --------------------------------------

pss_items <- function(d, p) data.frame(
  Q20 = r_pss    (d[[paste0(p,"_Q20")]]),
  Q21 = r_pss    (d[[paste0(p,"_Q21")]]),
  Q22 = r_pss    (d[[paste0(p,"_Q22")]]),
  Q23 = r_pss_rev(d[[paste0(p,"_Q23")]]),
  Q24 = r_pss_rev(d[[paste0(p,"_Q24")]]),
  Q25 = r_pss    (d[[paste0(p,"_Q25")]]),
  Q26 = r_pss_rev(d[[paste0(p,"_Q26")]]),
  Q27 = r_pss_rev(d[[paste0(p,"_Q27")]]),
  Q28 = r_pss    (d[[paste0(p,"_Q28")]]),
  Q29 = r_pss    (d[[paste0(p,"_Q29")]])
)

alpha_pss_t1 <- psych::alpha(pss_items(valid_data, "T1"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha
alpha_pss_t2 <- psych::alpha(pss_items(valid_data, "T2"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha
alpha_pss_t3 <- psych::alpha(pss_items(valid_data, "T3"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha

cat(sprintf("PSS-10:  T1 = %.3f | T2 = %.3f | T3 = %.3f\n",
            alpha_pss_t1, alpha_pss_t2, alpha_pss_t3))


# --- PHQ-9 (Q31–Q39) ---------------------------------------

phq_items <- function(d, p) data.frame(
  Q31 = r_phq(d[[paste0(p,"_Q31")]]), Q32 = r_phq(d[[paste0(p,"_Q32")]]),
  Q33 = r_phq(d[[paste0(p,"_Q33")]]), Q34 = r_phq(d[[paste0(p,"_Q34")]]),
  Q35 = r_phq(d[[paste0(p,"_Q35")]]), Q36 = r_phq(d[[paste0(p,"_Q36")]]),
  Q37 = r_phq(d[[paste0(p,"_Q37")]]), Q38 = r_phq(d[[paste0(p,"_Q38")]]),
  Q39 = r_phq(d[[paste0(p,"_Q39")]])
)

alpha_phq_t1 <- psych::alpha(phq_items(valid_data, "T1"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha
alpha_phq_t2 <- psych::alpha(phq_items(valid_data, "T2"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha
alpha_phq_t3 <- psych::alpha(phq_items(valid_data, "T3"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha

cat(sprintf("PHQ-9:   T1 = %.3f | T2 = %.3f | T3 = %.3f\n",
            alpha_phq_t1, alpha_phq_t2, alpha_phq_t3))


# --- GAD-7 (Q42–Q48) ---------------------------------------

gad_items <- function(d, p) data.frame(
  Q42 = r_gad(d[[paste0(p,"_Q42")]]), Q43 = r_gad(d[[paste0(p,"_Q43")]]),
  Q44 = r_gad(d[[paste0(p,"_Q44")]]), Q45 = r_gad(d[[paste0(p,"_Q45")]]),
  Q46 = r_gad(d[[paste0(p,"_Q46")]]), Q47 = r_gad(d[[paste0(p,"_Q47")]]),
  Q48 = r_gad(d[[paste0(p,"_Q48")]])
)

alpha_gad_t1 <- psych::alpha(gad_items(valid_data, "T1"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha
alpha_gad_t2 <- psych::alpha(gad_items(valid_data, "T2"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha
alpha_gad_t3 <- psych::alpha(gad_items(valid_data, "T3"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha

cat(sprintf("GAD-7:   T1 = %.3f | T2 = %.3f | T3 = %.3f\n",
            alpha_gad_t1, alpha_gad_t2, alpha_gad_t3))


# --- PCL-5 (Q51–Q70) ---------------------------------------

pcl_items <- function(d, p) data.frame(
  Q51 = r_pcl(d[[paste0(p,"_Q51")]]), Q52 = r_pcl(d[[paste0(p,"_Q52")]]),
  Q53 = r_pcl(d[[paste0(p,"_Q53")]]), Q54 = r_pcl(d[[paste0(p,"_Q54")]]),
  Q55 = r_pcl(d[[paste0(p,"_Q55")]]), Q56 = r_pcl(d[[paste0(p,"_Q56")]]),
  Q57 = r_pcl(d[[paste0(p,"_Q57")]]), Q58 = r_pcl(d[[paste0(p,"_Q58")]]),
  Q59 = r_pcl(d[[paste0(p,"_Q59")]]), Q60 = r_pcl(d[[paste0(p,"_Q60")]]),
  Q61 = r_pcl(d[[paste0(p,"_Q61")]]), Q62 = r_pcl(d[[paste0(p,"_Q62")]]),
  Q63 = r_pcl(d[[paste0(p,"_Q63")]]), Q64 = r_pcl(d[[paste0(p,"_Q64")]]),
  Q65 = r_pcl(d[[paste0(p,"_Q65")]]), Q66 = r_pcl(d[[paste0(p,"_Q66")]]),
  Q67 = r_pcl(d[[paste0(p,"_Q67")]]), Q68 = r_pcl(d[[paste0(p,"_Q68")]]),
  Q69 = r_pcl(d[[paste0(p,"_Q69")]]), Q70 = r_pcl(d[[paste0(p,"_Q70")]])
)

alpha_pcl_t1 <- psych::alpha(pcl_items(valid_data, "T1"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha
alpha_pcl_t2 <- psych::alpha(pcl_items(valid_data, "T2"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha
alpha_pcl_t3 <- psych::alpha(pcl_items(valid_data, "T3"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha

cat(sprintf("PCL-5:   T1 = %.3f | T2 = %.3f | T3 = %.3f\n",
            alpha_pcl_t1, alpha_pcl_t2, alpha_pcl_t3))


# --- CD-RISC-10 (Q72–Q81) ----------------------------------

cdr_items <- function(d, p) data.frame(
  Q72 = r_cdr(d[[paste0(p,"_Q72")]]), Q73 = r_cdr(d[[paste0(p,"_Q73")]]),
  Q74 = r_cdr(d[[paste0(p,"_Q74")]]), Q75 = r_cdr(d[[paste0(p,"_Q75")]]),
  Q76 = r_cdr(d[[paste0(p,"_Q76")]]), Q77 = r_cdr(d[[paste0(p,"_Q77")]]),
  Q78 = r_cdr(d[[paste0(p,"_Q78")]]), Q79 = r_cdr(d[[paste0(p,"_Q79")]]),
  Q80 = r_cdr(d[[paste0(p,"_Q80")]]), Q81 = r_cdr(d[[paste0(p,"_Q81")]])
)

alpha_cdr_t1 <- psych::alpha(cdr_items(valid_data, "T1"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha
alpha_cdr_t2 <- psych::alpha(cdr_items(valid_data, "T2"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha
alpha_cdr_t3 <- psych::alpha(cdr_items(valid_data, "T3"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha

cat(sprintf("CD-RISC: T1 = %.3f | T2 = %.3f | T3 = %.3f\n",
            alpha_cdr_t1, alpha_cdr_t2, alpha_cdr_t3))


# --- WHO-5 (Q93–Q97) ----------------------------------------

who5_items <- function(d, p) data.frame(
  Q93 = r_who5(d[[paste0(p,"_Q93")]]), Q94 = r_who5(d[[paste0(p,"_Q94")]]),
  Q95 = r_who5(d[[paste0(p,"_Q95")]]), Q96 = r_who5(d[[paste0(p,"_Q96")]]),
  Q97 = r_who5(d[[paste0(p,"_Q97")]])
)

alpha_who5_t1 <- psych::alpha(who5_items(valid_data, "T1"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha
alpha_who5_t2 <- psych::alpha(who5_items(valid_data, "T2"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha
alpha_who5_t3 <- psych::alpha(who5_items(valid_data, "T3"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha

cat(sprintf("WHO-5:   T1 = %.3f | T2 = %.3f | T3 = %.3f\n",
            alpha_who5_t1, alpha_who5_t2, alpha_who5_t3))


# --- WHODAS (Q100–Q107) ------------------------------------

whodas_items <- function(d, p) data.frame(
  Q100 = r_whodas(d[[paste0(p,"_Q100")]]), Q101 = r_whodas(d[[paste0(p,"_Q101")]]),
  Q102 = r_whodas(d[[paste0(p,"_Q102")]]), Q103 = r_whodas(d[[paste0(p,"_Q103")]]),
  Q104 = r_whodas(d[[paste0(p,"_Q104")]]), Q105 = r_whodas(d[[paste0(p,"_Q105")]]),
  Q106 = r_whodas(d[[paste0(p,"_Q106")]]), Q107 = r_whodas(d[[paste0(p,"_Q107")]])
)

alpha_whodas_t1 <- psych::alpha(whodas_items(valid_data, "T1"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha
alpha_whodas_t2 <- psych::alpha(whodas_items(valid_data, "T2"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha
alpha_whodas_t3 <- psych::alpha(whodas_items(valid_data, "T3"), check.keys=TRUE, warnings=FALSE)$total$raw_alpha

cat(sprintf("WHODAS:  T1 = %.3f | T2 = %.3f | T3 = %.3f\n",
            alpha_whodas_t1, alpha_whodas_t2, alpha_whodas_t3))


# --- Зведена таблиця альф ----------------------------------

alpha_table <- data.frame(
  Шкала    = c("PSS-10","PHQ-9","GAD-7","PCL-5","CD-RISC-10","WHO-5","WHODAS"),
  alpha_T1 = round(c(alpha_pss_t1, alpha_phq_t1, alpha_gad_t1, alpha_pcl_t1,
                     alpha_cdr_t1, alpha_who5_t1, alpha_whodas_t1), 3),
  alpha_T2 = round(c(alpha_pss_t2, alpha_phq_t2, alpha_gad_t2, alpha_pcl_t2,
                     alpha_cdr_t2, alpha_who5_t2, alpha_whodas_t2), 3),
  alpha_T3 = round(c(alpha_pss_t3, alpha_phq_t3, alpha_gad_t3, alpha_pcl_t3,
                     alpha_cdr_t3, alpha_who5_t3, alpha_whodas_t3), 3)
)

print(alpha_table, row.names = FALSE)
#write.csv(alpha_table, "cronbach_alpha.csv", row.names = FALSE)


# ============================================================
# RCI — Reliable Change Index (Jacobson & Truax, 1991)
# ============================================================
# На відміну від LMM, який показує середній ефект по групі,
# RCI оцінює зміну на рівні кожного учасника окремо і дозволяє
# визначити, чи є ця зміна клінічно надійною — тобто такою,
# що перевищує похибку самого вимірювального інструменту.
#
# Формула: RCI = (X2 - X1) / SE_diff
#   SE_diff = SD_T1 * sqrt(2 * (1 - α))
#
# Інтерпретація (для шкал де вищий бал = гірше):
#   RCI < -1.96  → надійне покращення
#   RCI > +1.96  → надійне погіршення
#   між ними     → стабільно (зміна в межах шуму вимірювання)
#
# Для CD-RISC-10 та WHO-5 знак інвертується, оскільки
# вищий бал означає кращий стан.


# Альфи з власних обчислень вище (T1 як базова хвиля)
alpha_values <- list(
  PSS.10     = alpha_pss_t1,
  PHQ.9      = alpha_phq_t1,
  GAD.7      = alpha_gad_t1,
  PCL.5      = alpha_pcl_t1,
  CD.RISC.10 = alpha_cdr_t1,
  WHO.5      = alpha_who5_t1,
  WHODAS     = alpha_whodas_t1
)


# --- Обчислення RCI для одного учасника --------------------

compute_rci <- function(x1, x2, sd_t1, alpha, higher_is_better = FALSE) {
  se_diff <- sd_t1 * sqrt(2 * (1 - alpha))
  rci     <- (x2 - x1) / se_diff
  if (higher_is_better) rci <- -rci
  rci
}


# --- Класифікація за порогом ±1.96 -------------------------

classify_rci <- function(rci) {
  case_when(
    rci < -1.96 ~ "Покращення",
    rci >  1.96 ~ "Погіршення",
    TRUE        ~ "Стабільно"
  )
}


# --- Список шкал із напрямком ------------------------------

scales_rci <- list(
  list(name = "PSS-10",     col = "PSS.10",     better = FALSE),
  list(name = "PHQ-9",      col = "PHQ.9",      better = FALSE),
  list(name = "GAD-7",      col = "GAD.7",      better = FALSE),
  list(name = "PCL-5",      col = "PCL.5",      better = FALSE),
  list(name = "CD-RISC-10", col = "CD.RISC.10", better = TRUE),
  list(name = "WHO-5",      col = "WHO.5",      better = TRUE),
  list(name = "WHODAS",     col = "WHODAS",     better = FALSE)
)


# --- Функція: RCI для однієї шкали і одного переходу -------

run_rci <- function(data, scale_info, t_from = "T1", t_to = "T3") {
  
  col_from <- paste0(t_from, "_", scale_info$col)
  col_to   <- paste0(t_to,   "_", scale_info$col)
  
  if (!col_from %in% names(data) || !col_to %in% names(data)) {
    message("Не знайдено: ", col_from, " або ", col_to)
    return(NULL)
  }
  
  df <- data %>%
    select(
      participant_id = Q3,
      Group          = T1_University,
      x1             = all_of(col_from),
      x2             = all_of(col_to)
    ) %>%
    filter(!is.na(x1), !is.na(x2))
  
  sd_t1 <- sd(df$x1, na.rm = TRUE)
  a     <- alpha_values[[scale_info$col]]
  
  df %>% mutate(
    Scale      = scale_info$name,
    Transition = paste0(t_from, "→", t_to),
    RCI        = compute_rci(x1, x2, sd_t1, a, scale_info$better),
    Category   = classify_rci(RCI),
    Change     = x2 - x1
  )
}


# --- Запуск для всіх шкал і двох переходів -----------------

rci_results <- bind_rows(lapply(scales_rci, function(sc) {
  bind_rows(
    run_rci(valid_data, sc, "T1", "T2"),
    run_rci(valid_data, sc, "T1", "T3"),
    run_rci(valid_data, sc, "T2", "T3")
  )
}))


# --- Зведена таблиця % за категоріями ----------------------

rci_summary <- rci_results %>%
  group_by(Scale, Transition, Category) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Scale, Transition) %>%
  mutate(
    total = sum(n),
    pct   = round(100 * n / total, 1)
  ) %>%
  ungroup()

print(rci_summary)
#write.csv(rci_summary, "rci_summary.csv", row.names = FALSE)


alpha_by_wave <- list(
  PSS.10     = c(T1 = alpha_pss_t1,   T2 = alpha_pss_t2,   T3 = alpha_pss_t3),
  PHQ.9      = c(T1 = alpha_phq_t1,   T2 = alpha_phq_t2,   T3 = alpha_phq_t3),
  GAD.7      = c(T1 = alpha_gad_t1,   T2 = alpha_gad_t2,   T3 = alpha_gad_t3),
  PCL.5      = c(T1 = alpha_pcl_t1,   T2 = alpha_pcl_t2,   T3 = alpha_pcl_t3),
  CD.RISC.10 = c(T1 = alpha_cdr_t1,   T2 = alpha_cdr_t2,   T3 = alpha_cdr_t3),
  WHO.5      = c(T1 = alpha_who5_t1,  T2 = alpha_who5_t2,  T3 = alpha_who5_t3),
  WHODAS     = c(T1 = alpha_whodas_t1,T2 = alpha_whodas_t2,T3 = alpha_whodas_t3)
)


# --- Поріг надійної зміни для всіх шкал і всіх хвиль ------

thresholds <- bind_rows(lapply(scales_rci, function(sc) {
  bind_rows(lapply(c("T1", "T2", "T3"), function(wave) {
    col <- paste0(wave, "_", sc$col)
    if (!col %in% names(valid_data)) return(NULL)
    
    sd_w      <- sd(valid_data[[col]], na.rm = TRUE)
    alpha_w   <- alpha_by_wave[[sc$col]][[wave]]
    se_diff_w <- sd_w * sqrt(2 * (1 - alpha_w))
    threshold <- 1.96 * se_diff_w
    
    data.frame(
      Шкала   = sc$name,
      Хвиля   = wave,
      SD      = round(sd_w,      2),
      Alpha   = round(alpha_w,   3),
      SE_diff = round(se_diff_w, 2),
      Поріг   = round(threshold, 1)
    )
  }))
}))

print(thresholds, row.names = FALSE)


# ============================================================
# ВІЗУАЛІЗАЦІЯ
# ============================================================

# --- 1. Waterfall plot — PSS-10, кожен учасник окремо ------
# Відсортовані смуги від найбільшого покращення до найбільшого
# погіршення. Пунктир = поріг ±1.96.

rci_pss_t3 <- rci_results %>%
  filter(Scale == "PSS-10", Transition == "T1→T3") %>%
  arrange(RCI) %>%
  mutate(rank = row_number())

ggplot(rci_pss_t3, aes(x = rank, y = RCI, fill = Category)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c(
    "Покращення" = "#1D9E75",
    "Стабільно"  = "#888780",
    "Погіршення" = "#D85A30"
  )) +
  labs(
    title    = "RCI PSS-10: T1 → T3",
    subtitle = "Кожна смуга = один учасник. Пунктир = ±1.96 (поріг надійної зміни)",
    x        = "Учасники (відсортовані за RCI)",
    y        = "RCI",
    fill     = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave("rci_waterfall_pss.png", width = 10, height = 5, dpi = 200)


# --- 2. Stacked bar — частки за університетами -------------

rci_results %>%
  filter(Scale == "PSS-10", !is.na(Group)) %>%
  group_by(Group, Transition, Category) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Group, Transition) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ggplot(aes(x = Group, y = pct, fill = Category)) +
  geom_col(position = "stack") +
  facet_wrap(~ Transition) +
  scale_fill_manual(values = c(
    "Покращення" = "#1D9E75",
    "Стабільно"  = "#888780",
    "Погіршення" = "#D85A30"
  )) +
  labs(
    title = "PSS-10: частка учасників за категоріями RCI",
    x = NULL, y = "% учасників", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "top")

ggsave("rci_stacked_by_group.png", width = 9, height = 5, dpi = 200)


# --- 3. Jacobson-Truax plot — PSS-10: T1 vs T3 -------------
# Класичний графік для RCI: бал на T1 (вісь X) проти T3 (вісь Y).
# Суцільна діагональ = нуль змін.
# Паралельні пунктири = поріг надійної зміни (±1.96 × SE_diff).
# Точки нижче зеленої лінії — надійне покращення,
# вище помаранчевої — надійне погіршення.

rci_pss_plot <- valid_data %>%
  filter(!is.na(T1_PSS.10), !is.na(T3_PSS.10)) %>%
  mutate(
    RCI      = compute_rci(T1_PSS.10, T3_PSS.10,
                           sd(T1_PSS.10, na.rm = TRUE),
                           alpha_values[["PSS.10"]], FALSE),
    Category = classify_rci(RCI)
  )

se_d <- sd(valid_data$T1_PSS.10, na.rm = TRUE) *
  sqrt(2 * (1 - alpha_values[["PSS.10"]]))

ggplot(rci_pss_plot, aes(x = T1_PSS.10, y = T3_PSS.10, color = Category)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_abline(intercept = 0,           slope = 1, color = "gray50") +
  geom_abline(intercept =  1.96 * se_d, slope = 1, linetype = "dashed",
              color = "#D85A30", linewidth = 0.7) +
  geom_abline(intercept = -1.96 * se_d, slope = 1, linetype = "dashed",
              color = "#1D9E75", linewidth = 0.7) +
  scale_color_manual(values = c(
    "Покращення" = "#1D9E75",
    "Стабільно"  = "#888780",
    "Погіршення" = "#D85A30"
  )) +
  labs(
    title    = "Jacobson-Truax plot: PSS-10 T1 vs T3",
    subtitle = "Суцільна лінія = без змін; пунктир = поріг надійної зміни (RCI ±1.96)",
    x        = "PSS-10 на T1 (базовий рівень)",
    y        = "PSS-10 на T3 (3 місяці після курсу)",
    color    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave("rci_scatter_pss_t1_t3.png", width = 7, height = 6, dpi = 200)


# ============================================================
# ПІДСУМОК ДЛЯ ТЕКСТУ ДИПЛОМНОЇ РОБОТИ
# ============================================================

for (sc in c("PSS-10","PHQ-9","GAD-7","PCL-5","CD-RISC-10","WHO-5","WHODAS")) {
  for (tr in c("T1→T2", "T1→T3", "T2→T3")) {
    s    <- filter(rci_summary, Scale == sc, Transition == tr)
    imp  <- s %>% filter(Category == "Покращення") %>% pull(pct)
    stab <- s %>% filter(Category == "Стабільно")  %>% pull(pct)
    det  <- s %>% filter(Category == "Погіршення") %>% pull(pct)
    tot  <- if (nrow(s) > 0) max(s$total) else 0
    if (!length(imp))  imp  <- 0
    if (!length(stab)) stab <- 0
    if (!length(det))  det  <- 0
    cat(sprintf(
      "%s | %s | N=%d | покращення: %s%% | стабільно: %s%% | погіршення: %s%%\n",
      sc, tr, tot, imp, stab, det
    ))
  }
}
