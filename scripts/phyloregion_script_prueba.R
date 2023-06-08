tree <- read.tree("C:/Users/veruk/OneDrive - Harvard University/data_forestrestoration/Plant_diversity/Daru_etal_data/DATA/phylogeny/WRLD_phylo.tre")

matrices <- taxon3 %>%
  mutate(species = str_replace_all(taxon_clean_bdc, " ", "_")) %>%
  filter(rescaled_measurement > 0) %>%
  select(id_comm, species) %>%
  rename(grids = id_comm) %>%
  group_by(grids, .add = TRUE) %>%
  group_split()

alpha_historical <- function(m, phy) {
  n <- subset(m, m$species %in% intersect(phy$tip.label, m$species))
  subphy <- match_phylo_comm(phy, long2sparse(n))$phy
  submat <- match_phylo_comm(phy, long2sparse(n))$comm
  return(list(subphy = subphy, submat = submat))
}

p <- NULL
for (i in 1:length(matrices)) {
  print(i)
  mi <- matrices[[i]]
  skip_to_next <- FALSE
  tryCatch(p[[i]] <- alpha_historical(m = mi, phy = tree),
           error = function(e) { skip_to_next <<- TRUE})
  if(skip_to_next) { next }
}
beepr::beep()

save(p, file = "phylo.RData")
load(file = "phylo.RData")

#Nulo donde solo hay una especie (p.e. Calvino-Cancela et al. 2012) #Se puede poner 0
#Donde todos los nombres son de una palabra (p.e. Dalling 1998_Herbs_FOS, DeWalt 2003_Tree genus)
#o hay alguno de 2 palabras pero no lo pilla? (p.e. Purata 1986_1 2R Herbs (5% cover) random plots or quadrats_Maize17)
#No tengo nombres de especies (Gillison et al. 2003)
#Los briofitos y los liquenes no los pilla (Hylander and Weibull 2012_Bryophytes 2009, Li et al. 2011_1 2R Lichen random plots or quadrats)

p2 <- lapply(p, function(x) {
  if (length(x) == 0) {data.frame(PD = NA, SR = NA)}
  else {
    picante::pd(samp = as.matrix(x$submat), tree = x$subphy)
  }
})

pd <- do.call(rbind, p2)
pd$id_comm <- row.names(pd)