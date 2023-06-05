
####Tipo de bosque #####

#Para aplicar a cada parcela, con los arboles que se consideren (p.e. Vivos),
#la clase para definir el tipo de bosque (p.e. tipo funcional de las sp) 
#y la variable de cantidad previamente definidos

forest_classify <- function (quantity, class, 
                             threshold = 50) {
  
  dffortype <- tibble(quantity = quantity, class = class, threshold = threshold) %>% 
    mutate(total = sum(quantity, na.rm = T)) %>% 
    group_by(class) %>% 
    summarise(qu_gr = sum(quantity, na.rm = T) * 100 / total) %>% 
    ungroup() %>%
    mutate(main_class = max(qu_gr)) %>% 
    filter(qu_gr == main_class) %>% 
    slice(1) %>% 
    mutate(type = ifelse(qu_gr > threshold,
                         class,
                         "mixed")) %>% 
    select(type) 
  
  return(dffortype$type[1])     
  
}

# Pruebas con la tabla de pma_clean de IFN3
# plotprueba <- tree3 %>% filter(IDPC3 == "430303A1") #Pinus halepensis
# plotprueba <- tree3 %>% filter(IDPC3 == "80402A1") #Pinus sylvestris + Quercus faginea
# plotprueba <- tree3 %>% filter(IDPC3 == "22115NN") #Mezcla de bldec
# 
# forest_classify(quantity = plotprueba$AB3m2ha,
#                 class = plotprueba$Tipo,
#                 threshold = 50)


