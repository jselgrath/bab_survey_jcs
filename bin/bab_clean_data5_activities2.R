# California Ocean Access: Benefits and Barriers (bab)
# Jennifer Selgrath 
# California Marine Sanctuary Foundation

# goal: translate comments and clean, then pull activities out of comments. create columns related to dogs.

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(dplyr)
library(stringr)
library(stringi)

# note in cases where write in text fits two categories, then it is assigned the first one that matches

# --------------------------------------------------------------------------
# load data -----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

# comments - for activities and dogs
d1<-read_csv("./results/data_long4.csv")%>%
  glimpse()



# ask gemini for code to clean and translate comments based on actual responses
writeClipboard(as.character(d1$Comments))
#these are the comments from the ocean access survey. I want to create a code to make cleaned values that clean typos and translate comments into english ($comment2), a column (binary - 0,1) for any mention of dogs ($comment_dog_b), a binary column ($comment_dog_positive) if the comment about the dog is positive (1) or negative (0), a new binary column $comment_dog_walking (1= walk dog), and a column that extracts any activities mentioned in the comments into a comma separated column (if more than 1: $comment_activity: [paste comments here]

writeClipboard(d1$QImportant_Activities)
# can you update the activities to use the ones in this other column and then create new ones as needed if they do not fit. I wonder if there should be physical/mental health one of that should get grouped with the relaxing/meditation category

# --------------------------------------------------------------------------
# Translation Lookup: Spanish & Chinese
# --------------------------------------------------------------------------
translations <- c(
  "需要更多的保护海洋的动植物" = "Need more protection for marine flora and fauna.",
  "Me gusta este lago es muy familiar" = "I like this lake; it is very family-friendly.",
  "Tener más recursos para cuidarlo" = "Have more resources to take care of it.",
  "Las playas son para todos." = "Beaches are for everyone.",
  "Pienso que las arias costeras deberían ser públicas y que todos nos debemos beneficiar de ellas con amor y respeto. No solo los ricos o los que pueden pagar. Me parece que las playas en California están casi privatizada y que se ven como un gran negocio. A pesar de que que los ricos pueden vivir en las costas ellos son a la larga los más dañados que le hacen porque pienso que sus desperdicio y su basura termina en esas agua." = "I think coastal areas should be public and that we should all benefit from them with love and respect. Not just the rich or those who can pay. It seems to me that California beaches are almost privatized and seen as big business. Even though the wealthy can live on the coasts, in the long run they cause the most harm because their waste and trash end up in those waters.",
  "Lo que estas haciendo con las incuestas esta muy bien 10 de 10" = "What you are doing with the surveys is very good, 10 out of 10.",
  "Las playas son muy lindas" = "The beaches are very pretty.",
  "Los amo son las mejores playas para relajarse" = "I love them; they are the best beaches to relax.",
  "Hagan mas aeras protegidas" = "Make more protected areas.",
  "Estuvira bien si huviera mas gente interesada proteger los arrecifes de coral." = "It would be good if there were more people interested in protecting coral reefs.",
  "Que las playas tienen mucha basura, en Venice hay problemas de drogadiccion y mucha Gente viviendo en la calle" = "The beaches have a lot of trash; in Venice there are drug addiction issues and many unhoused people living on the street.",
  "La verdad me gustaría ir más al océano pero no me gusta manejar en tráfico y lamentablemente en Los Ángeles existe mucho tráfico." = "Honestly I would like to go to the ocean more, but I dislike driving in traffic, and unfortunately there is a lot of traffic in Los Angeles.",
  "El tráfico de carros que existe en Los Ángeles me causa no querer ir a lugares en los océanos." = "Car traffic in Los Angeles makes me not want to visit ocean spots.",
  "Estar en el océano es una tranquilidad y paz" = "Being by the ocean brings tranquility and peace.",
  "Me gusta pescar y mirar el mar" = "I like to fish and look at the sea.",
  "Caminar al aire libre" = "Walking in the open air.",
  "Que se cuide el oceano" = "Take care of the ocean.",
  "California es maravilloso , pero hay mucho descuido a menudo en las zonas turísticas basura y contaminación del agua" = "California is wonderful, but there is often neglect in tourist zones: trash and water contamination.",
  "Es muy agradable venir alas costas de california pero todo es muy caro y difícil de frecuentarlos" = "It is very pleasant to visit California coasts, but everything is very expensive and hard to visit frequently.",
  "Hay que genera mas empleos y educar" = "Need to create more jobs and educate.",
  "Me encanta el mar" = "I love the sea.",
  "Si, necesitamos mas albercas para q nuestros hijos aprendan a nadar porque las classes de natacion se llenan en Cinco minutos despues de q abren inscripciones. ESO limita a nuestros generations a appender a surf.n Cinco minutos despues" = "Yes, we need more pools so our children learn to swim because classes fill within five minutes after registration opens. That restricts our generations from learning how to surf.",
  "Estamos destruyendo el planeta y no Hay otro mas los politicos y las grandes corporations estan vendiendo y destruyendo el planeta el futuro de nuestro hijos esta amenazado y mas con Trum si gana tenemos que votar al 1000%yseguirluchando" = "We are destroying the planet and there is no backup. Politicians and large corporations are selling and destroying the planet; the future of our children is threatened. We must vote and keep fighting.",
  "Q hay q mantener Los océanos limpios y así  cuidamos los animales q hay ayi" = "We need to keep oceans clean to protect the animals living there.",
  "Lugar maravilloso para estar en familia" = "Wonderful place to spend time with family.",
  "Me gusta mucho disfrutar el océano pero se necesita limpieza en la playa" = "I like enjoying the ocean very much, but beach cleaning is needed.",
  "Me gusta mucho" = "I like it very much.",
  "Me gustaría más limpieza del agua como recolección de basura" = "I would like cleaner water and trash collection.",
  "Muy bonito clima El mejor" = "Very beautiful weather, the best.",
  "que se necesita mas conciencia y no tirar basura en las playas" = "More awareness is needed, and do not litter on beaches.",
  "es muy importante en la vida de las familias el océano limpio y tanto para los vivientes en el océano" = "A clean ocean is vital for families as well as for marine life.",
  "Las zonas de baños muy deterioradas" = "Restroom facilities are very run down.",
  "Mas tiempo de pesca por las noches y todo muy bien" = "More night fishing time allowed, and everything else is very good.",
  "Me gusta mucho la pesca" = "I like fishing very much.",
  "No por ahora" = "Not right now.",
  "Disfruto El oceano" = "I enjoy the ocean.",
  "TRATAR DE NADAR Y CHOCAR CON LAS OLAS. EL PETROLEO UN POCO EN LOS PIES" = "Trying to swim and encounter the waves. A bit of oil on the feet.",
  "Me encantaria que se inicie un movimiento que ayude a limpiar las playas para perros (Hendrys beach). La comunidad es fuerte y seguramente functionary." = "I would love to see a movement started to help clean dog beaches (Hendry's Beach). The community is strong and would surely make it work.",
  "Mantener mucho mejor nuestras playas." = "Keep our beaches in much better shape.",
  "Me parece que mantienen los océanos en muy buena condición, que si contamos con una buena seguridad a la hora de que llegara a pasar algún tipo de accidente, me parece que mantienen las areas muy limpias yo no tengo nada negativo que opinar al respecto." = "I think they maintain the oceans in very good condition and have good safety response if an accident occurs; they keep areas very clean and I have nothing negative to say.",
  "Hermoso! Ojalá todos tuviesen la consciencia de cuidar mucho los recursos naturales del mar, playa y océano!" = "Beautiful! If only everyone had the awareness to care for marine, beach, and ocean resources!",
  "Solo que es inportante cuidar de los oceanos para las nuevas jeneraciones" = "It is important to care for the oceans for future generations.",
  "Mu bonita las plallas" = "The beaches are very pretty.",
  "Me gustaria quecuidaramos mas las playas como no tirar basura para protegee a lo animales" = "I would like us to care for beaches better, such as not littering to protect animals.",
  "Me encanta El color del mar,deseo que siga limpias sus playas." = "I love the color of the sea; I hope its beaches stay clean.",
  "Me gusta pescar. Que están haciendo buen uso para cuidar de las playas" = "I like fishing. They are doing good work taking care of the beaches.",
  "Mejista el sonido delas olas el aygre ber los tiburones" = "I like the sound of the waves, the breeze, and seeing sharks.",
  "Me gusta la tranquilidad que brindan, y su aire limpio" = "I like the tranquility they provide and the clean air.",
  "Los areas estan muy limpias y bonitas." = "The areas are very clean and pretty.",
  "Las playas son muy Ricas en vida marina" = "The beaches are very rich in marine life.",
  "Está limpio y tiene buenos servicios como baño y agua" = "It is clean and has good amenities like restrooms and water.",
  "减少对海洋的污染" = "Reduce ocean pollution.",
  "Fue un éxito" = "It was a success.",
  "Hermosos paisajes" = "Beautiful scenery.",
  "Gracias" = "Thank you.",
  "Experiencia inolvidable" = "Unforgettable experience.",
  "Deve de aver mas accesos libres para los turistas y menos privados" = "There should be more free access points for visitors and fewer private ones.",
  "Me gusta andar en ferry" = "I like riding the ferry.",
  "Mayor acceso" = "Greater access.",
  "Más limpieza" = "More cleaning.",
  "Los banos estan muy sucios y ya no funcionan o estan celdrades. Los showers no funcionan. Mucha basura." = "Restrooms are very dirty and out of service or closed. Showers do not work. Lots of trash.",
  "Puden poner algunas sombras para El sol ?" = "Could you install some shade structures for the sun?",
  "Me gusta mucho ir en familia" = "I like going with my family very much.",
  "Deberia haber anuncios mas claros sobre lo qué está extrictamente prohibido hacer, porque siempre voy con el temor de meterme en problemas por hacer algo sin saber que es ilegal." = "There should be clearer signage about what is strictly forbidden, because I always fear getting into trouble for doing something illegal without knowing.",
  "Nada" = "Nothing.",
  "Son muy bellas pero les hace falta ser más promocionadas." = "They are very beautiful but need more promotion.",
  "Me gusta océano" = "I like the ocean.",
  "Me gusta mucha el agua. Pero por no poder nadar no me meto mucho al ocean." = "I like the water very much, but because I cannot swim I don't go far into the ocean.",
  "Las playas de California son demasiado hermosas🥰." = "California beaches are overly gorgeous.",
  "El océano es un lugar muy pacífico tranquilo que merece la mayor atención y cuidado" = "The ocean is a very peaceful and tranquil place that deserves the utmost care and attention.",
  "保护优先与人类使用之间需要平衡。我支持以科学为基础的管理决策，确保濒危物种和关键栖息地得到保护，同时允许公众进行可持续的观光和教育活动。" = "A balance is needed between conservation priority and human use. I support science-based decisions that preserve endangered species and key habitats while permitting sustainable sightseeing and education.",
  "Los océanos Son muy importantes para nuestra futura generación y hay que cuidarlos lo más posible." = "Oceans are vital for our future generation and must be protected as much as possible.",
  "Que las personas Sean mas cuidadosaa, cuidar El ambiente y no TIRAR BASURA EN EL OCEANO" = "People should be more careful, care for the environment, and not throw trash in the ocean.",
  "tanto los oceanos como ves costor, son muy importantes tanto por la economica y noturnieza climas y contor persumas en todos los aspectos." = "Both oceans and coasts are very important for the economy, natural climate, and people in every aspect.",
  "Muchas gracias por su trabajo" = "Thank you very much for your work.",
  "没" = "None."
)

# --------------------------------------------------------------------------
# Clean Comments Function
# --------------------------------------------------------------------------
f_clean_comment <- function(txt) {
  if (is.na(txt)) return(NA_character_)
  
  t <- str_trim(txt)
  
  # 1. Strip survey metadata blocks (e.g. "DATE: ...; LOCATION: 418 Project - AAPI Festival; INITIALS: ...")
  t <- str_remove(t, "(?i)^DATE:\\s*\\d+;.*?(COMMENT:\\s*|$)")
  t <- str_remove(t, "(?i)^PAPER_DATA_ENTRY[^;]*;.*?COMMENT:\\s*")
  t <- str_remove(t, "(?i)^PAPER_DATA_ENTRY.*?;\\s*")
  t <- str_remove(t, "(?i);?\\s*PAPER_DATA_ENTRY.*$")
  t <- str_remove(t, "(?i)\\bPaper survey\\b.*$")
  t <- str_remove(t, "(?i)\\bPaper Entry\\b.*$")
  t <- str_remove(t, "\\(Paper Survey[^)]*\\)")
  
  # 2. Check for empty or generic non-responses
  na_regex <- "^(?i)(na|n/a|n-a|none|no comment|no comments|nothing|nothing else|nope|no|not at this time|not at the moment|not right now|lol|z|k|m|\\.|-)$"
  if (str_detect(str_trim(t), na_regex) || str_trim(t) == "") return(NA_character_)
  
  # 3. Typo corrections and phrase normalizations
  t <- str_replace_all(t, "\\s+,", ",")                       # Fix spaces before commas: " ," -> ","
  t <- str_replace_all(t, "(?i)\\bwheel\\s+chair\\b", "wheelchair") # "wheel chair" -> "wheelchair"
  t <- str_replace_all(t, "(?i)\\btypose\\b", "typos")
  t <- str_replace_all(t, "(?i)\\bthrash\\b", "trash")
  t <- str_replace_all(t, "(?i)\\bopertuinty ah\\b", "opportunity has")
  t <- str_replace_all(t, "(?i)\\bcostal\\b", "coastal")
  t <- str_replace_all(t, "(?i)\\bpllz|plz\\b", "please")
  t <- str_replace_all(t, "(?i)\\bteh\\b", "the")
  t <- str_replace_all(t, "(?i)\\balot\\b", "a lot")
  t <- str_replace_all(t, "(?i)\\btoprotect\\b", "to protect")
  t <- str_replace_all(t, "(?i)\\bperal\\b", "peril")
  t <- str_replace_all(t, "(?i)\\bwatwrs\\b", "waters")
  t <- str_replace_all(t, "(?i)\\bthe the\\b", "the")
  
  # 4. Dictionary translation lookup
  trimmed_key <- str_trim(t)
  if (trimmed_key %in% names(translations)) {
    t <- translations[[trimmed_key]]
  }
  
  t <- str_squish(t)
  if (t == "" || str_detect(t, na_regex)) return(NA_character_)
  return(t)
}

# --------------------------------------------------------------------------
# Activity Patterns
# --------------------------------------------------------------------------
activity_patterns <- list(
  # Combined walking, hiking, running, and dog walking
  "Walking/Running/Hiking" = 
    "(?i)\\b(walk|walking|stroll|strolling|hike|hiking|backpacked|backpacking|caminar|dog walking|walk.*?(?:dog|puppy|perro)|walking the dog)\\b|(?<!long\\s)\\b(run|running)\\b(?!\\s+(down|off|out|into))",
  
  # Excludes "cannot swim", "can't swim", "don't swim", "no poder nadar", and access/pool advocacy
  "Swimming/Bodysurfing" = 
    "(?i)(?<!cannot |can't |cant |don't |dont |do not |unable to |no poder |not able to |learn to |learning to |classes to |need more pools to )\\b(swim|swimming|swam|swimmer|swimmers|bodysurf|bodysurfing|body surf|wade|wading|nadar|natacion)\\b",
  
  # Excludes "learn to surf", "surf zone/report/forecast"
  "Surfing" = 
    "(?i)(?<!learn to |learning to |appender a )\\b(surf|surfing|surfer|surfers|surfboard|surf coach|line-up|lineup)\\b(?!\\s*(zone|advisory|warning|forecast|report))",
  
  # Excludes "Department of Fish and Game", "fish and wildlife"
  "Fishing or collecting food" = 
    "(?i)\\b(?<!department of )(?:fish(ing|er|erman|ermen)?|angling|anglers|spearfish(?:ing)?|speargun|abalone|lobster|crabbing|clamming|foraging|pescar|pesca)\\b(?!\\s+and\\s+(?:game|wildlife))",
  
  "Snorkeling/Scuba Diving" = 
    "(?i)\\b(scuba|scuba dive|diving|diver|divers|snorkel|snorkeling|freedive|freediver|freediving)\\b",
  
  "Paddleboarding/Kiteboarding/Kayaking/Canoeing" = 
    "(?i)\\b(kayak|kayaking|paddleboard|paddleboarding|paddle board|canoe|canoeing|outrigging|kiteboard|kiteboarding|paddling)\\b",
  
  # Excludes idiomatic "smooth sailing"
  "Sailing/Boating" = 
    "(?i)(?<!smooth\\s)\\b(boating|sail|sailing|sailboat|powerboat|boater|yacht|ferry|motorboat|jetski|jet skiing)\\b(?!\\s*through)",
  
  "Bicycling/Roller skating/Skateboarding" = 
    "(?i)\\b(bike|biking|bicycle|bicycling|skate|skating|skateboard|skateboarding|roller skate|roller skating)\\b|(?<!no\\s|ban\\s|prohibit\\s|anti[- ])\\b(e-scooters?)\\b",
  
  "Beach games/Sports/Yoga" = 
    "(?i)\\b(volleyball|yoga|beach games|archery|sports|exercise)\\b",
  
  "Enjoying views/sunsets" = 
    "(?i)\\b(sunset|sunsets|sunrise|sunrises|scenery|ocean breeze|paisajes|watching waves|look at the sea|mirar el mar)\\b",
  
  "Meditation/Reading/Relaxing/Art/Health" = 
    "(?i)\\b(relax|relaxing|relaxation|meditate|meditation|meditating|read|reading|art|painter|painting|plein air|draw|drawing|books|therapy|therapeutic|mental health|peace|peaceful|calming|rejuvenat\\w*|reset|distract|tranquilidad|relajarse|benefits of coastal life|coastal living|well-being|wellbeing|quality of life)\\b",
  
  "Nature Observing/Photographing/Education/Research" = 
    "(?i)\\b(photograph|photography|photos|tide pool|tidepool|tidepooling|whale watching|birding|birds|docent|field trips?|school tour|nature class|marine research)\\b",
  
  "Group/Family gatherings or activities" = 
    "(?i)\\b(picnic|picnicking|bbq|bbqs|barbecue|gather|gathering|en familia|family outing)\\b",
  
  "Paid work" = 
    "(?i)\\b(paid work|commercial fisher|commercial fishing|charter captain|work on boats|lifeguard|deckhand)\\b",
  
  "Volunteering" = 
    "(?i)\\b(volunteer|volunteering|volunteered|beach cleanup|clean-up|cleanups)\\b",
  
  "Festivals" = 
    "(?i)\\b(festival|festivals|concert|concerts|parade|celebration)\\b",
  
  "Cultural or religious ceremonies" = 
    "(?i)\\b(ceremony|ceremonies|church|religion|spiritual|sacred|tribal tradition|native culture|ancestral)\\b",

  "Group/Family gatherings or activities" = # was Camping but very small
    "(?i)\\b(recreational camping|camped|tent camping|camp spots|campsite)\\b",
  
  "Volunteering" = # was Habitat restoration / Conservation action, but very small category
    "(?i)\\b(culling urchins|kelp restoration|dune restoration|outplanting|clearing urchins|restoration project)\\b"
)

# --------------------------------------------------------------------------
# Extract Activities Function
# --------------------------------------------------------------------------
f_extract_activities <- function(txt) {
  if (is.na(txt) || str_trim(txt) == "") return(NA_character_)
  
  matched <- c()
  
  for (act in names(activity_patterns)) {
    pat <- activity_patterns[[act]]
    
    if (str_detect(txt, regex(pat, ignore_case = TRUE))) {
      # Screening out non-participatory swimming/surfing comments:
      # 1) Inability to swim: "cannot swim", "no poder nadar", "don't know how to swim"
      # 2) Swim/surf lessons/pools advocacy for children
      is_inability_or_advocacy <- (
        str_detect(txt, regex("(?i)\\b(cannot swim|can't swim|cant swim|not able to swim|don't know how to swim|no poder nadar|need(s)? more pools|classes fill|limits? our generations? from learning|limita a|appender a)\\b")) && 
        act %in% c("Swimming/Bodysurfing", "Surfing")
      )
      
      if (!is_inability_or_advocacy) {
        matched <- c(matched, act)
      }
    }
  }
  
  if (length(matched) == 0) return(NA_character_)
  paste(unique(matched), collapse = ", ")
}

# Dog walking regex helper
dog_walk_regex <- regex(
  "\\b(dog walk|dog walking|walk.*?(?:dog|puppy|perro)|walking my dog|take the dogs?|exercising.*?(?:dog|dogs)|off-leash dog|perros? en la playa)\\b", 
  ignore_case = TRUE
)

# -------------------------------------------------------------------------
# Transformation Pipeline
# -------------------------------------------------------------------------
d2 <- d1 %>%
  mutate(
    # 1. Cleaned, standardized, and translated comment column
    comment2 = sapply(Comments, f_clean_comment),
    
    # 2. Binary: Any mention of dogs (0 or 1)
    comment_dog_b = if_else(
      !is.na(comment2) & str_detect(comment2, regex("\\b(dog|dogs|puppy|perro|perros|off-leash|leash)\\b", ignore_case = TRUE)),
      1L, 0L
    ),
    
    # 3. Binary: Specific mention of dog walking / recreation (0 or 1)
    comment_dog_walking = if_else(
      comment_dog_b == 1L & str_detect(comment2, dog_walk_regex),
      1L, 0L
    ),
    
    # 4. Binary: Dog sentiment (1 = positive / dog-friendly, 0 = negative / waste / issues)
    comment_dog_positive = case_when(
      comment_dog_b == 0L ~ NA_integer_,
      str_detect(comment2, regex("waste|disrespect|frustrate|disruption|leash laws|fines|banned|bite|poop|feces", ignore_case = TRUE)) & 
        !str_detect(comment2, regex("more dog friendly|more places for off leash|dog beaches please", ignore_case = TRUE)) ~ 0L,
      TRUE ~ 1L
    ),
    
    # 5. Comma-separated extracted activities
    comment_activity = sapply(comment2, f_extract_activities)
  )

# Inspect results
d2 %>% 
  select(comment2, comment_dog_b, comment_dog_walking, comment_dog_positive, comment_activity) %>%
  # view()
  glimpse()

# Save
write_csv(d2, "./results/data_long5.csv")