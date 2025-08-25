### bigdf_genera ###

#Run Script1_bigdf_cleaned first!

#Aconitum
aa <- apply(is.na(bigdf[,c("Aconitum", "Aconitum.lycoctonum")]),1,all)
f_sum <- rowSums(bigdf[,c("Aconitum", "Aconitum.lycoctonum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Aconitum", "Aconitum.lycoctonum")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Aconitum <- new_sum
bigdf <- subset(bigdf, select = -c(Aconitum.lycoctonum))

#Alisma
aa <- apply(is.na(bigdf[,c("Alisma", "Alisma.plantago.aquatica")]),1,all)
f_sum <- rowSums(bigdf[,c("Alisma", "Alisma.plantago.aquatica")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Alisma", "Alisma.plantago.aquatica")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Alisma <- new_sum
bigdf <- subset(bigdf, select = -c(Alisma.plantago.aquatica))

#Alnus
aa <- apply(is.na(bigdf[,c("Alnus", "Alnus.glutinosa", "Alnus.incana", "Alnus.viridis")]),1,all)
f_sum <- rowSums(bigdf[,c("Alnus", "Alnus.glutinosa", "Alnus.incana", "Alnus.viridis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Alnus", "Alnus.glutinosa", "Alnus.incana", "Alnus.viridis")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Alnus <- new_sum
bigdf <- subset(bigdf, select = -c(Alnus.glutinosa, Alnus.incana, Alnus.viridis))

#Anagallis
aa <- apply(is.na(bigdf[,c("Anagallis.arvensis", "Anagallis.tenella")]),1,all)
f_sum <- rowSums(bigdf[,c("Anagallis.arvensis", "Anagallis.tenella")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Anagallis.arvensis", "Anagallis.tenella")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Anagallis <- new_sum
bigdf <- subset(bigdf, select = -c(Anagallis.arvensis, Anagallis.tenella))

#Anemone
aa <- apply(is.na(bigdf[,c("Anemone", "Anemone.nemorosa")]),1,all)
f_sum <- rowSums(bigdf[,c()],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c()])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Alnus <- new_sum
bigdf <- subset(bigdf, select = -c(Anemone.nemorosa))

#Arctostaphylos
aa <- apply(is.na(bigdf[,c("Arctostaphylos", "Arctostaphylos.uva.ursi")]),1,all)
f_sum <- rowSums(bigdf[,c("Arctostaphylos", "Arctostaphylos.uva.ursi")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Arctostaphylos", "Arctostaphylos.uva.ursi")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Arctostaphylos <- new_sum
bigdf <- subset(bigdf, select = -c(Arctostaphylos.uva.ursi))

#Artemisia
aa <- apply(is.na(bigdf[,c("Artemisia", "Artemisia.norvegica")]),1,all)
f_sum <- rowSums(bigdf[,c("Artemisia", "Artemisia.norvegica")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Artemisia", "Artemisia.norvegica")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Artemisia <- new_sum
bigdf <- subset(bigdf, select = -c(Artemisia.norvegica))

#Astragalus
aa <- apply(is.na(bigdf[,c("Astragalus.glycyphyllos", "Astragalus.alpinus", "Astragalus.frigidus")]),1,all)
f_sum <- rowSums(bigdf[,c("Astragalus.glycyphyllos", "Astragalus.alpinus", "Astragalus.frigidus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Astragalus.glycyphyllos", "Astragalus.alpinus", "Astragalus.frigidus")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Astragalus <- new_sum
bigdf <- subset(bigdf, select = -c(Astragalus.glycyphyllos, Astragalus.alpinus, Astragalus.frigidus))

#Athyrium
aa <- apply(is.na(bigdf[,c("Athyrium.alpestre", "Athyrium.filix.femina")]),1,all)
f_sum <- rowSums(bigdf[,c("Athyrium.alpestre", "Athyrium.filix.femina")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Athyrium.alpestre", "Athyrium.filix.femina")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Athyrium <- new_sum
bigdf <- subset(bigdf, select = -c(Athyrium.alpestre, Athyrium.filix.femina))

bigdf <- rename(bigdf, Adoxa = Adoxa.moschatellina)
bigdf <- rename(bigdf, Arctous = Arctous.alpina)

#Betula
aa <- apply(is.na(bigdf[,c("Betula", "Betula.fruticosa", "Betula.humilis", "Betula.nana", "Betula.pendula", "Betula.pubescens")]),1,all)
f_sum <- rowSums(bigdf[,c("Betula", "Betula.fruticosa", "Betula.humilis", "Betula.nana", "Betula.pendula", "Betula.pubescens")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Betula", "Betula.fruticosa", "Betula.humilis", "Betula.nana", "Betula.pendula", "Betula.pubescens")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Betula <- new_sum
bigdf <- subset(bigdf, select = -c(Betula.fruticosa, Betula.humilis, Betula.nana, Betula.pendula, Betula.pubescens))

#Bistorta
aa <- apply(is.na(bigdf[,c("Bistorta", "Bistorta.officinalis", "Bistorta.officinalis.B..vivipara", "Bistorta.vivipara")]),1,all)
f_sum <- rowSums(bigdf[,c("Bistorta", "Bistorta.officinalis", "Bistorta.officinalis.B..vivipara", "Bistorta.vivipara")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Bistorta", "Bistorta.officinalis", "Bistorta.officinalis.B..vivipara", "Bistorta.vivipara")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Bistorta <- new_sum
bigdf <- subset(bigdf, select = -c(Bistorta.officinalis, Bistorta.officinalis.B..vivipara, Bistorta.vivipara))

#Botrychium
aa <- apply(is.na(bigdf[,c("Botrychium", "Botrychium.lunaria")]),1,all)
f_sum <- rowSums(bigdf[,c("Botrychium", "Botrychium.lunaria")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Botrychium", "Botrychium.lunaria")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Botrychium <- new_sum
bigdf <- subset(bigdf, select = -c(Botrychium.lunaria))

bigdf <- rename(bigdf, Bassia = Bassia.laniflora)
bigdf <- rename(bigdf, Blechnum = Blechnum.spicant)

#Calluna
aa <- apply(is.na(bigdf[,c("Calluna", "Calluna.vulgaris")]),1,all)
f_sum <- rowSums(bigdf[,c("Calluna", "Calluna.vulgaris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Calluna", "Calluna.vulgaris")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Calluna <- new_sum
bigdf <- subset(bigdf, select = -c(Calluna.vulgaris))

#Caltha
aa <- apply(is.na(bigdf[,c("Caltha", "Caltha.palustris")]),1,all)
f_sum <- rowSums(bigdf[,c("Caltha", "Caltha.palustris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Caltha", "Caltha.palustris")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Caltha <- new_sum
bigdf <- subset(bigdf, select = -c(Caltha.palustris))

#Cannabis
aa <- apply(is.na(bigdf[,c("Cannabis", "Cannabis.sativa")]),1,all)
f_sum <- rowSums(bigdf[,c("Cannabis", "Cannabis.sativa")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Cannabis", "Cannabis.sativa")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Cannabis <- new_sum
bigdf <- subset(bigdf, select = -c(Cannabis.sativa))

#Carpinus
aa <- apply(is.na(bigdf[,c("Carpinus", "Carpinus.betulus")]),1,all)
f_sum <- rowSums(bigdf[,c("Carpinus", "Carpinus.betulus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Carpinus", "Carpinus.betulus")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Carpinus <- new_sum
bigdf <- subset(bigdf, select = -c(Carpinus.betulus))

#Centaurea
aa <- apply(is.na(bigdf[,c("Centaurea.cyanus", "Centaurea.jacea", "Centaurea.nigra", "Centaurea.scabiosa")]),1,all)
f_sum <- rowSums(bigdf[,c("Centaurea.cyanus", "Centaurea.jacea", "Centaurea.nigra", "Centaurea.scabiosa")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Centaurea.cyanus", "Centaurea.jacea", "Centaurea.nigra", "Centaurea.scabiosa")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Centaurea <- new_sum
bigdf <- subset(bigdf, select = -c(Centaurea.cyanus, Centaurea.jacea, Centaurea.nigra, Centaurea.scabiosa))

#Cerastium
aa <- apply(is.na(bigdf[,c("Cerastium", "Cerastium.cerastoides", "Cerastium.fontanum")]),1,all)
f_sum <- rowSums(bigdf[,c("Cerastium", "Cerastium.cerastoides", "Cerastium.fontanum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Cerastium", "Cerastium.cerastoides", "Cerastium.fontanum")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Cerastium <- new_sum
bigdf <- subset(bigdf, select = -c(Cerastium.cerastoides, Cerastium.fontanum))

#Convallaria
aa <- apply(is.na(bigdf[,c("Convallaria", "Convallaria.majalis")]),1,all)
f_sum <- rowSums(bigdf[,c("Convallaria", "Convallaria.majalis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Convallaria", "Convallaria.majalis")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Convallaria <- new_sum
bigdf <- subset(bigdf, select = -c(Convallaria.majalis))

#Cornus
aa <- apply(is.na(bigdf[,c("Cornus", "Cornus.mas.C..suecica", "Cornus.sanguinea", "Cornus.suecica")]),1,all)
f_sum <- rowSums(bigdf[,c("Cornus", "Cornus.mas.C..suecica", "Cornus.sanguinea", "Cornus.suecica")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Cornus", "Cornus.mas.C..suecica", "Cornus.sanguinea", "Cornus.suecica")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Cornus <- new_sum
bigdf <- subset(bigdf, select = -c(Cornus.mas.C..suecica, Cornus.sanguinea, Cornus.suecica))

#Corylus
aa <- apply(is.na(bigdf[,c("Corylus", "Corylus.avellana")]),1,all)
f_sum <- rowSums(bigdf[,c("Corylus", "Corylus.avellana")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Corylus", "Corylus.avellana")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Corylus <- new_sum
bigdf <- subset(bigdf, select = -c(Corylus.avellana))

#Cryptogramma
aa <- apply(is.na(bigdf[,c("Cryptogramma", "Cryptogramma.crispa")]),1,all)
f_sum <- rowSums(bigdf[,c("Cryptogramma", "Cryptogramma.crispa")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Cryptogramma", "Cryptogramma.crispa")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Cryptogramma <- new_sum
bigdf <- subset(bigdf, select = -c(Cryptogramma.crispa))

bigdf <- rename(bigdf, Cladium = Cladium.mariscus)
bigdf <- rename(bigdf, Cystopteris = Cystopteris.fragilis)

#Diapensia
aa <- apply(is.na(bigdf[,c("Diapensia", "Diapensia.lapponica")]),1,all)
f_sum <- rowSums(bigdf[,c("Diapensia", "Diapensia.lapponica")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Diapensia", "Diapensia.lapponica")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Diapensia <- new_sum
bigdf <- subset(bigdf, select = -c(Diapensia.lapponica))

#Diphasiastrum
aa <- apply(is.na(bigdf[,c("Diphasiastrum", "Diphasiastrum.alpinum", "Diphasiastrum.alpinum.D..complanatum", "Diphasiastrum.complanatum", "Diphasiastrum.tristachyum")]),1,all)
f_sum <- rowSums(bigdf[,c()],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c()])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Diphasiastrum <- new_sum
bigdf <- subset(bigdf, select = -c(Diphasiastrum.alpinum, Diphasiastrum.alpinum.D..complanatum, Diphasiastrum.complanatum, Diphasiastrum.tristachyum))

#Drosera
aa <- apply(is.na(bigdf[,c("Drosera", "Drosera.anglica", "Drosera.intermedia", "Drosera.rotundifolia", "Drosera.rotundifolia.D..anglica")]),1,all)
f_sum <- rowSums(bigdf[,c("Drosera", "Drosera.anglica", "Drosera.intermedia", "Drosera.rotundifolia", "Drosera.rotundifolia.D..anglica")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Drosera", "Drosera.anglica", "Drosera.intermedia", "Drosera.rotundifolia", "Drosera.rotundifolia.D..anglica")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Drosera <- new_sum
bigdf <- subset(bigdf, select = -c(Drosera.anglica, Drosera.intermedia, Drosera.rotundifolia, Drosera.rotundifolia.D..anglica))

#Dryopteris
aa <- apply(is.na(bigdf[,c("Dryopteris", "Dryopteris.carthusiana", "Dryopteris.dilatata", "Dryopteris.filix.mas")]),1,all)
f_sum <- rowSums(bigdf[,c("Dryopteris", "Dryopteris.carthusiana", "Dryopteris.dilatata", "Dryopteris.filix.mas")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Dryopteris", "Dryopteris.carthusiana", "Dryopteris.dilatata", "Dryopteris.filix.mas")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Dryopteris <- new_sum
bigdf <- subset(bigdf, select = -c(Dryopteris.carthusiana, Dryopteris.dilatata, Dryopteris.filix.mas))

bigdf <- rename(bigdf, Dryas = Dryas.octopetala)

#Echium
aa <- apply(is.na(bigdf[,c("Echium", "Echium.vulgare")]),1,all)
f_sum <- rowSums(bigdf[,c("Echium", "Echium.vulgare")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Echium", "Echium.vulgare")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Echium <- new_sum
bigdf <- subset(bigdf, select = -c(Echium.vulgare))

#Empetrum
aa <- apply(is.na(bigdf[,c("Empetrum", "Empetrum.nigrum")]),1,all)
f_sum <- rowSums(bigdf[,c("Empetrum", "Empetrum.nigrum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Empetrum", "Empetrum.nigrum")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Empetrum <- new_sum
bigdf <- subset(bigdf, select = -c(Empetrum.nigrum))

#Ephedra
aa <- apply(is.na(bigdf[,c("Ephedra", "Ephedra.distachya", "Ephedra.fragilis")]),1,all)
f_sum <- rowSums(bigdf[,c("Ephedra", "Ephedra.distachya", "Ephedra.fragilis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Ephedra", "Ephedra.distachya", "Ephedra.fragilis")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Ephedra <- new_sum
bigdf <- subset(bigdf, select = -c(Ephedra.distachya, Ephedra.fragilis))

#Erica
aa <- apply(is.na(bigdf[,c("Erica.arborea", "Erica.cinerea.E..tetralix")]),1,all)
f_sum <- rowSums(bigdf[,c("Erica.arborea", "Erica.cinerea.E..tetralix")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Erica.arborea", "Erica.cinerea.E..tetralix")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Erica <- new_sum
bigdf <- subset(bigdf, select = -c(Erica.arborea, Erica.cinerea.E..tetralix))

#Fagopyrum
aa <- apply(is.na(bigdf[,c("Fagopyrum", "Fagopyrum.esculentum")]),1,all)
f_sum <- rowSums(bigdf[,c("Fagopyrum", "Fagopyrum.esculentum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Fagopyrum", "Fagopyrum.esculentum")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Fagopyrum <- new_sum
bigdf <- subset(bigdf, select = -c(Fagopyrum.esculentum))

#Fagus
aa <- apply(is.na(bigdf[,c("Fagus", "Fagus.sylvatica")]),1,all)
f_sum <- rowSums(bigdf[,c("Fagus", "Fagus.sylvatica")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Fagus", "Fagus.sylvatica")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Fagus <- new_sum
bigdf <- subset(bigdf, select = -c(Fagus.sylvatica))

#Frangula
aa <- apply(is.na(bigdf[,c("Frangula", "Frangula.alnus")]),1,all)
f_sum <- rowSums(bigdf[,c("Frangula", "Frangula.alnus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Frangula", "Frangula.alnus")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Frangula <- new_sum
bigdf <- subset(bigdf, select = -c(Frangula.alnus))

#Fraxinus
aa <- apply(is.na(bigdf[,c("Fraxinus", "Fraxinus.excelsior")]),1,all)
f_sum <- rowSums(bigdf[,c("Fraxinus", "Fraxinus.excelsior")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Fraxinus", "Fraxinus.excelsior")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Fraxinus <- new_sum
bigdf <- subset(bigdf, select = -c(Fraxinus.excelsior))

#Gentiana
aa <- apply(is.na(bigdf[,c("Gentiana", "Gentiana.campestris", "Gentiana.nivalis", "Gentiana.pneumonanthe", "Gentiana.purpurea")]),1,all)
f_sum <- rowSums(bigdf[,c("Gentiana", "Gentiana.campestris", "Gentiana.nivalis", "Gentiana.pneumonanthe", "Gentiana.purpurea")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Gentiana", "Gentiana.campestris", "Gentiana.nivalis", "Gentiana.pneumonanthe", "Gentiana.purpurea")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Gentiana <- new_sum
bigdf <- subset(bigdf, select = -c(Gentiana.campestris, Gentiana.nivalis, Gentiana.pneumonanthe, Gentiana.purpurea))

#Gentianella
aa <- apply(is.na(bigdf[,c("Gentianella.campestris", "Gentianella.detonsa")]),1,all)
f_sum <- rowSums(bigdf[,c("Gentianella.campestris", "Gentianella.detonsa")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Gentianella.campestris", "Gentianella.detonsa")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Gentianella <- new_sum
bigdf <- subset(bigdf, select = -c(Gentianella.campestris, Gentianella.detonsa))

#Geranium
aa <- apply(is.na(bigdf[,c("Geranium", "Geranium.sylvaticum")]),1,all)
f_sum <- rowSums(bigdf[,c("Geranium", "Geranium.sylvaticum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Geranium", "Geranium.sylvaticum")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Geranium <- new_sum
bigdf <- subset(bigdf, select = -c(Geranium.sylvaticum))

#Geum
aa <- apply(is.na(bigdf[,c("Geum", "Geum.rivale")]),1,all)
f_sum <- rowSums(bigdf[,c("Geum", "Geum.rivale")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Geum", "Geum.rivale")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Geum <- new_sum
bigdf <- subset(bigdf, select = -c(Geum.rivale))

#Gymnocarpium
aa <- apply(is.na(bigdf[,c("Gymnocarpium", "Gymnocarpium.dryopteris")]),1,all)
f_sum <- rowSums(bigdf[,c("Gymnocarpium", "Gymnocarpium.dryopteris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Gymnocarpium", "Gymnocarpium.dryopteris")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Gymnocarpium <- new_sum
bigdf <- subset(bigdf, select = -c(Gymnocarpium.dryopteris))

#Hedera
aa <- apply(is.na(bigdf[,c("Hedera", "Hedera.helix")]),1,all)
f_sum <- rowSums(bigdf[,c("Hedera", "Hedera.helix")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Hedera", "Hedera.helix")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Hedera <- new_sum
bigdf <- subset(bigdf, select = -c(Hedera.helix))

#Hippophaë
aa <- apply(is.na(bigdf[,c("Hippophaë", "Hippophaë.rhamnoides")]),1,all)
f_sum <- rowSums(bigdf[,c("Hippophaë", "Hippophaë.rhamnoides")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Hippophaë", "Hippophaë.rhamnoides")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Hippophaë <- new_sum
bigdf <- subset(bigdf, select = -c(Hippophaë.rhamnoides))

#Hippuris
aa <- apply(is.na(bigdf[,c("Hippuris", "Hippuris.vulgaris")]),1,all)
f_sum <- rowSums(bigdf[,c("Hippuris", "Hippuris.vulgaris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Hippuris", "Hippuris.vulgaris")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Hippuris <- new_sum
bigdf <- subset(bigdf, select = -c(Hippuris.vulgaris))

#Humulus
aa <- apply(is.na(bigdf[,c("Humulus", "Humulus.lupulus")]),1,all)
f_sum <- rowSums(bigdf[,c("Humulus", "Humulus.lupulus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Humulus", "Humulus.lupulus")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Humulus <- new_sum
bigdf <- subset(bigdf, select = -c(Humulus.lupulus))

#Huperzia
aa <- apply(is.na(bigdf[,c("Huperzia", "Huperzia.selago")]),1,all)
f_sum <- rowSums(bigdf[,c("Huperzia", "Huperzia.selago")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Huperzia", "Huperzia.selago")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Huperzia <- new_sum
bigdf <- subset(bigdf, select = -c(Huperzia.selago))

#Hypericum
aa <- apply(is.na(bigdf[,c("Hypericum", "Hypericum.calycinum", "Hypericum.perforatum", "Hypericum.pulchrum")]),1,all)
f_sum <- rowSums(bigdf[,c("Hypericum", "Hypericum.calycinum", "Hypericum.perforatum", "Hypericum.pulchrum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Hypericum", "Hypericum.calycinum", "Hypericum.perforatum", "Hypericum.pulchrum")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Hypericum <- new_sum
bigdf <- subset(bigdf, select = -c(Hypericum.calycinum, Hypericum.perforatum, Hypericum.pulchrum))

bigdf <- rename(bigdf, Hottonia = Hottonia.palustris)
bigdf <- rename(bigdf, Hydrocotyle = Hydrocotyle.vulgaris)

#Jasione
aa <- apply(is.na(bigdf[,c("Jasione", "Jasione.montana")]),1,all)
f_sum <- rowSums(bigdf[,c("Jasione", "Jasione.montana")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Jasione", "Jasione.montana")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Jasione <- new_sum
bigdf <- subset(bigdf, select = -c(Jasione.montana))

#Juglans
aa <- apply(is.na(bigdf[,c("Juglans", "Juglans.regia")]),1,all)
f_sum <- rowSums(bigdf[,c("Juglans", "Juglans.regia")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Juglans", "Juglans.regia")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Juglans <- new_sum
bigdf <- subset(bigdf, select = -c(Juglans.regia))

#Juniperus
aa <- apply(is.na(bigdf[,c("Juniperus", "Juniperus.communis")]),1,all)
f_sum <- rowSums(bigdf[,c("Juniperus", "Juniperus.communis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Juniperus", "Juniperus.communis")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Juniperus <- new_sum
bigdf <- subset(bigdf, select = -c(Juniperus.communis))

bigdf <- rename(bigdf, Knautia = Knautia.arvensis)
bigdf <- rename(bigdf, Krascheninnikovia = Krascheninnikovia.ceratoides)

#Lonicera
aa <- apply(is.na(bigdf[,c("Lonicera", "Lonicera.periclymenum", "Lonicera.xylosteum")]),1,all)
f_sum <- rowSums(bigdf[,c("Lonicera", "Lonicera.periclymenum", "Lonicera.xylosteum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Lonicera", "Lonicera.periclymenum", "Lonicera.xylosteum")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Lonicera <- new_sum
bigdf <- subset(bigdf, select = -c(Lonicera.periclymenum, Lonicera.xylosteum))

bigdf <- rename(bigdf, Lingulodinium = Lingulodinium.machaerophorum)

#Lotus
aa <- apply(is.na(bigdf[,c("Lotus", "Lotus.pedunculatus", "Lotus.corniculatus")]),1,all)
f_sum <- rowSums(bigdf[,c("Lotus", "Lotus.pedunculatus", "Lotus.corniculatus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Lotus", "Lotus.pedunculatus", "Lotus.corniculatus")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Lotus <- new_sum
bigdf <- subset(bigdf, select = -c(Lotus.pedunculatus, Lotus.corniculatus))

#Lycopodium
aa <- apply(is.na(bigdf[,c("Lycopodium", "Lycopodium.annotinum", "Lycopodium.clavatum")]),1,all)
f_sum <- rowSums(bigdf[,c("Lycopodium", "Lycopodium.annotinum", "Lycopodium.clavatum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Lycopodium", "Lycopodium.annotinum", "Lycopodium.clavatum")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Lycopodium <- new_sum
bigdf <- subset(bigdf, select = -c(Lycopodium.annotinum, Lycopodium.clavatum))

#Lysimachia
aa <- apply(is.na(bigdf[,c("Lysimachia", "Lysimachia.vulgaris")]),1,all)
f_sum <- rowSums(bigdf[,c("Lysimachia", "Lysimachia.vulgaris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Lysimachia", "Lysimachia.vulgaris")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Lysimachia <- new_sum
bigdf <- subset(bigdf, select = -c(Lysimachia.vulgaris))

#Lythrum
aa <- apply(is.na(bigdf[,c("Lythrum", "Lythrum.salicaria")]),1,all)
f_sum <- rowSums(bigdf[,c("Lythrum", "Lythrum.salicaria")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Lythrum", "Lythrum.salicaria")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Lythrum <- new_sum
bigdf <- subset(bigdf, select = -c(Lythrum.salicaria))

bigdf <- rename(bigdf, Limosella = Limosella.aquatica)
bigdf <- rename(bigdf, Linnea = Linnaea.borealis)
bigdf <- rename(bigdf, Linum = Linum.usitatissimum)
bigdf <- rename(bigdf, Lobelia = Lobelia.dortmanna)
bigdf <- rename(bigdf, Lycopodiella = Lycopodiella.inundata)
bigdf <- rename(bigdf, Lycopsis = Lycopsis.arvensis)

#Matteuccia
aa <- apply(is.na(bigdf[,c("Matteuccia", "Matteuccia.struthiopteris")]),1,all)
f_sum <- rowSums(bigdf[,c("Matteuccia", "Matteuccia.struthiopteris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Matteuccia", "Matteuccia.struthiopteris")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Matteuccia <- new_sum
bigdf <- subset(bigdf, select = -c(Matteuccia.struthiopteris))

#Mercurialis
aa <- apply(is.na(bigdf[,c("Mercurialis", "Mercurialis.perennis")]),1,all)
f_sum <- rowSums(bigdf[,c("Mercurialis", "Mercurialis.perennis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Mercurialis", "Mercurialis.perennis")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Mercurialis <- new_sum
bigdf <- subset(bigdf, select = -c(Mercurialis.perennis))

#Micranthes
aa <- apply(is.na(bigdf[,c("Micranthes.nivalis", "Micranthes.nivalis.M..tenuis", "Micranthes.stellaris")]),1,all)
f_sum <- rowSums(bigdf[,c("Micranthes.nivalis", "Micranthes.nivalis.M..tenuis", "Micranthes.stellaris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Micranthes.nivalis", "Micranthes.nivalis.M..tenuis", "Micranthes.stellaris")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Micranthes <- new_sum
bigdf <- subset(bigdf, select = -c(Micranthes.nivalis, Micranthes.nivalis.M..tenuis, Micranthes.stellaris))

#Myrica
aa <- apply(is.na(bigdf[,c("Myrica", "Myrica.gale")]),1,all)
f_sum <- rowSums(bigdf[,c("Myrica", "Myrica.gale")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Myrica", "Myrica.gale")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Myrica <- new_sum
bigdf <- subset(bigdf, select = -c(Myrica.gale))

#Myricaria
aa <- apply(is.na(bigdf[,c("Myricaria", "Myricaria.germanica")]),1,all)
f_sum <- rowSums(bigdf[,c("Myricaria", "Myricaria.germanica")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Myricaria", "Myricaria.germanica")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Myricaria <- new_sum
bigdf <- subset(bigdf, select = -c(Myricaria.germanica))

#Myriophyllum
aa <- apply(is.na(bigdf[,c("Myriophyllum", "Myriophyllum.alterniflorum", "Myriophyllum.spicatum", "Myriophyllum.spicatum.M..verticillatum", "Myriophyllum.verticillatum")]),1,all)
f_sum <- rowSums(bigdf[,c("Myriophyllum", "Myriophyllum.alterniflorum", "Myriophyllum.spicatum", "Myriophyllum.spicatum.M..verticillatum", "Myriophyllum.verticillatum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Myriophyllum", "Myriophyllum.alterniflorum", "Myriophyllum.spicatum", "Myriophyllum.spicatum.M..verticillatum", "Myriophyllum.verticillatum")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Myriophyllum <- new_sum
bigdf <- subset(bigdf, select = -c(Myriophyllum.alterniflorum, Myriophyllum.spicatum, Myriophyllum.spicatum.M..verticillatum, Myriophyllum.verticillatum))

bigdf <- rename(bigdf, Menyanthes = Menyanthes.trifoliata)

#Nuphar
aa <- apply(is.na(bigdf[,c("Nuphar", "Nuphar.pumila")]),1,all)
f_sum <- rowSums(bigdf[,c("Nuphar", "Nuphar.pumila")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Nuphar", "Nuphar.pumila")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Nuphar <- new_sum
bigdf <- subset(bigdf, select = -c(Nuphar.pumila))

#Nymphaea
aa <- apply(is.na(bigdf[,c("Nymphaea", "Nymphaea.alba")]),1,all)
f_sum <- rowSums(bigdf[,c("Nymphaea", "Nymphaea.alba")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Nymphaea", "Nymphaea.alba")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Nymphaea <- new_sum
bigdf <- subset(bigdf, select = -c(Nymphaea.alba))

bigdf <- rename(bigdf, Narthecium = Narthecium.ossifragum)

#Ophioglossum
aa <- apply(is.na(bigdf[,c("Ophioglossum", "Ophioglossum.vulgatum")]),1,all)
f_sum <- rowSums(bigdf[,c("Ophioglossum", "Ophioglossum.vulgatum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Ophioglossum", "Ophioglossum.vulgatum")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Ophioglossum <- new_sum
bigdf <- subset(bigdf, select = -c(Ophioglossum.vulgatum))

#Oxyria
aa <- apply(is.na(bigdf[,c("Oxyria", "Oxyria.digyna")]),1,all)
f_sum <- rowSums(bigdf[,c("Oxyria", "Oxyria.digyna")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Oxyria", "Oxyria.digyna")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Oxyria <- new_sum
bigdf <- subset(bigdf, select = -c(Oxyria.digyna))

bigdf <- rename(bigdf, Operculodinium = Operculodinium.centrocarpum)
bigdf <- rename(bigdf, Osmunda = Osmunda.regalis)

#Papaver
aa <- apply(is.na(bigdf[,c("Papaver.radicatum", "Papaver.rhoeas")]),1,all)
f_sum <- rowSums(bigdf[,c("Papaver.radicatum", "Papaver.rhoeas")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Papaver.radicatum", "Papaver.rhoeas")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Papaver <- new_sum
bigdf <- subset(bigdf, select = -c(Papaver.radicatum, Papaver.rhoeas))

#Parnassia
aa <- apply(is.na(bigdf[,c("Parnassia", "Parnassia.palustris")]),1,all)
f_sum <- rowSums(bigdf[,c("Parnassia", "Parnassia.palustris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Parnassia", "Parnassia.palustris")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Parnassia <- new_sum
bigdf <- subset(bigdf, select = -c(Parnassia.palustris))

#Pedicularis
aa <- apply(is.na(bigdf[,c("Pedicularis", "Pedicularis.palustris")]),1,all)
f_sum <- rowSums(bigdf[,c("Pedicularis", "Pedicularis.palustris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Pedicularis", "Pedicularis.palustris")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Pedicularis <- new_sum
bigdf <- subset(bigdf, select = -c(Pedicularis.palustris))

#Persicaria
aa <- apply(is.na(bigdf[,c("Persicaria.amphibia", "Persicaria.maculosa")]),1,all)
f_sum <- rowSums(bigdf[,c("Persicaria.amphibia", "Persicaria.maculosa")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Persicaria.amphibia", "Persicaria.maculosa")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Persicaria <- new_sum
bigdf <- subset(bigdf, select = -c(Persicaria.amphibia, Persicaria.maculosa))

#Picea
aa <- apply(is.na(bigdf[,c("Picea", "Picea.abies")]),1,all)
f_sum <- rowSums(bigdf[,c("Picea", "Picea.abies")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Picea", "Picea.abies")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Picea <- new_sum
bigdf <- subset(bigdf, select = -c(Picea.abies))

#Pimpinella
aa <- apply(is.na(bigdf[,c("Pimpinella", "Pimpinella.saxifraga")]),1,all)
f_sum <- rowSums(bigdf[,c("Pimpinella", "Pimpinella.saxifraga")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Pimpinella", "Pimpinella.saxifraga")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Pimpinella <- new_sum
bigdf <- subset(bigdf, select = -c(Pimpinella.saxifraga))

#Pinus
aa <- apply(is.na(bigdf[,c("Pinus", "Pinus.sylvestris")]),1,all)
f_sum <- rowSums(bigdf[,c("Pinus", "Pinus.sylvestris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Pinus", "Pinus.sylvestris")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Pinus <- new_sum
bigdf <- subset(bigdf, select = -c(Pinus.sylvestris))

#Plantago
aa <- apply(is.na(bigdf[,c("Plantago", "Plantago.coronopus", "Plantago.lanceolata", "Plantago.major", "Plantago.major.P..media", "Plantago.maritima", "Plantago.media", "Plantago.uniflora")]),1,all)
f_sum <- rowSums(bigdf[,c("Plantago", "Plantago.coronopus", "Plantago.lanceolata", "Plantago.major", "Plantago.major.P..media", "Plantago.maritima", "Plantago.media", "Plantago.uniflora")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Plantago", "Plantago.coronopus", "Plantago.lanceolata", "Plantago.major", "Plantago.major.P..media", "Plantago.maritima", "Plantago.media", "Plantago.uniflora")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Plantago <- new_sum
bigdf <- subset(bigdf, select = -c(Plantago, Plantago.coronopus, Plantago.lanceolata, Plantago.major, Plantago.major.P..media, Plantago.maritima, Plantago.media, Plantago.uniflora))

#Polygonatum
aa <- apply(is.na(bigdf[,c("Polygonatum", "Polygonatum.verticillatum")]),1,all)
f_sum <- rowSums(bigdf[,c("Polygonatum", "Polygonatum.verticillatum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Polygonatum", "Polygonatum.verticillatum")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Polygonatum <- new_sum
bigdf <- subset(bigdf, select = -c(Polygonatum.verticillatum))

#Polypodium
aa <- apply(is.na(bigdf[,c("Polypodium", "Polypodium.vulgare")]),1,all)
f_sum <- rowSums(bigdf[,c("Polypodium", "Polypodium.vulgare")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Polypodium", "Polypodium.vulgare")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Polypodium <- new_sum
bigdf <- subset(bigdf, select = -c(Polypodium.vulgare))

#Populus
aa <- apply(is.na(bigdf[,c("Populus", "Populus.tremula")]),1,all)
f_sum <- rowSums(bigdf[,c("Populus", "Populus.tremula")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Populus", "Populus.tremula")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Populus <- new_sum
bigdf <- subset(bigdf, select = -c(Populus.tremula))

#Potentilla
aa <- apply(is.na(bigdf[,c("Potentilla", "Potentilla.recta")]),1,all)
f_sum <- rowSums(bigdf[,c("Potentilla", "Potentilla.recta")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Potentilla", "Potentilla.recta")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Potentilla <- new_sum
bigdf <- subset(bigdf, select = -c(Potentilla.recta))

#Primula
aa <- apply(is.na(bigdf[,c("Primula", "Primula.farinosa")]),1,all)
f_sum <- rowSums(bigdf[,c("Primula", "Primula.farinosa")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Primula", "Primula.farinosa")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Primula <- new_sum
bigdf <- subset(bigdf, select = -c(Primula.farinosa))

#Prunus
aa <- apply(is.na(bigdf[,c("Prunus", "Prunus.padus")]),1,all)
f_sum <- rowSums(bigdf[,c("Prunus", "Prunus.padus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Prunus", "Prunus.padus")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Prunus <- new_sum
bigdf <- subset(bigdf, select = -c(Prunus.padus))

#Pteridium
aa <- apply(is.na(bigdf[,c("Pteridium", "Pteridium.aquilinum")]),1,all)
f_sum <- rowSums(bigdf[,c("Pteridium", "Pteridium.aquilinum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Pteridium", "Pteridium.aquilinum")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Pteridium <- new_sum
bigdf <- subset(bigdf, select = -c(Pteridium.aquilinum))

bigdf <- rename(bigdf, Phegopteris = Phegopteris.connectilis)
bigdf <- rename(bigdf, Polygonum = Polygonum.aviculare)

#Ranunculus
aa <- apply(is.na(bigdf[,c("Ranunculus", "Ranunculus.acris", "Ranunculus.glacialis", "Ranunculus.trichophyllus")]),1,all)
f_sum <- rowSums(bigdf[,c("Ranunculus", "Ranunculus.acris", "Ranunculus.glacialis", "Ranunculus.trichophyllus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Ranunculus", "Ranunculus.acris", "Ranunculus.glacialis", "Ranunculus.trichophyllus")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Ranunculus <- new_sum
bigdf <- subset(bigdf, select = -c(Ranunculus.acris, Ranunculus.glacialis, Ranunculus.trichophyllus))

#Rhamnus
aa <- apply(is.na(bigdf[,c("Rhamnus", "Rhamnus.cathartica")]),1,all)
f_sum <- rowSums(bigdf[,c("Rhamnus", "Rhamnus.cathartica")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Rhamnus", "Rhamnus.cathartica")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Rhamnus <- new_sum
bigdf <- subset(bigdf, select = -c(Rhamnus.cathartica))

#Rhynchospora
aa <- apply(is.na(bigdf[,c("Rhynchospora", "Rhynchospora.alba")]),1,all)
f_sum <- rowSums(bigdf[,c("Rhynchospora", "Rhynchospora.alba")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Rhynchospora", "Rhynchospora.alba")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Rhynchospora <- new_sum
bigdf <- subset(bigdf, select = -c(Rhynchospora.alba))

#Rubus
aa <- apply(is.na(bigdf[,c("Rubus", "Rubus.chamaemorus", "Rubus.fruticosus", "Rubus.idaeus", "Rubus.saxatilis")]),1,all)
f_sum <- rowSums(bigdf[,c("Rubus", "Rubus.chamaemorus", "Rubus.fruticosus", "Rubus.idaeus", "Rubus.saxatilis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Rubus", "Rubus.chamaemorus", "Rubus.fruticosus", "Rubus.idaeus", "Rubus.saxatilis")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Rubus <- new_sum
bigdf <- subset(bigdf, select = -c(Rubus.chamaemorus, Rubus.fruticosus, Rubus.idaeus, Rubus.saxatilis))

#Rumex
aa <- apply(is.na(bigdf[,c("Rumex", "Rumex.acetosa", "Rumex.acetosa.R..acetosella", "Rumex.acetosella", "Rumex.longifolius")]),1,all)
f_sum <- rowSums(bigdf[,c("Rumex", "Rumex.acetosa", "Rumex.acetosa.R..acetosella", "Rumex.acetosella", "Rumex.longifolius")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Rumex", "Rumex.acetosa", "Rumex.acetosa.R..acetosella", "Rumex.acetosella", "Rumex.longifolius")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Rumex <- new_sum
bigdf <- subset(bigdf, select = -c(Rumex.acetosa, Rumex.acetosa.R..acetosella, Rumex.acetosella, Rumex.longifolius))

bigdf <- rename(bigdf, Rhododendron = Rhododendron.tomentosum)

#Sambucus
aa <- apply(is.na(bigdf[,c("Sambucus", "Sambucus.nigra.S..racemosa")]),1,all)
f_sum <- rowSums(bigdf[,c("Sambucus", "Sambucus.nigra.S..racemosa")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Sambucus", "Sambucus.nigra.S..racemosa")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Sambucus <- new_sum
bigdf <- subset(bigdf, select = -c(Sambucus.nigra.S..racemosa))

#Sanguisorba
aa <- apply(is.na(bigdf[,c("Sanguisorba", "Sanguisorba.officinalis")]),1,all)
f_sum <- rowSums(bigdf[,c("Sanguisorba", "Sanguisorba.officinalis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Sanguisorba", "Sanguisorba.officinalis")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Sanguisorba <- new_sum
bigdf <- subset(bigdf, select = -c(Sanguisorba.officinalis))

#Saussurea
aa <- apply(is.na(bigdf[,c("Saussurea", "Saussurea.nuda")]),1,all)
f_sum <- rowSums(bigdf[,c("Saussurea", "Saussurea.nuda")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Saussurea", "Saussurea.nuda")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Saussurea <- new_sum
bigdf <- subset(bigdf, select = -c(Saussurea.nuda))

#Saxifraga
aa <- apply(is.na(bigdf[,c("Saxifraga", "Saxifraga.cernua", "Saxifraga.cernua.S..rivularis", "Saxifraga.granulata", "Saxifraga.cespitosa", "Saxifraga.hirculus", "Saxifraga.oppositifolia", "Saxifraga.stellaris")]),1,all)
f_sum <- rowSums(bigdf[,c("Saxifraga", "Saxifraga.cernua", "Saxifraga.cernua.S..rivularis", "Saxifraga.granulata", "Saxifraga.cespitosa", "Saxifraga.hirculus", "Saxifraga.oppositifolia", "Saxifraga.stellaris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Saxifraga", "Saxifraga.cernua", "Saxifraga.cernua.S..rivularis", "Saxifraga.granulata", "Saxifraga.cespitosa", "Saxifraga.hirculus", "Saxifraga.oppositifolia", "Saxifraga.stellaris")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Saxifraga <- new_sum
bigdf <- subset(bigdf, select = -c(Saxifraga.cernua, Saxifraga.cernua.S..rivularis, Saxifraga.granulata, Saxifraga.cespitosa, Saxifraga.hirculus, Saxifraga.oppositifolia, Saxifraga.stellaris))

#Scheuchzeria
aa <- apply(is.na(bigdf[,c("Scheuchzeria", "Scheuchzeria.palustris")]),1,all)
f_sum <- rowSums(bigdf[,c("Scheuchzeria", "Scheuchzeria.palustris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Scheuchzeria", "Scheuchzeria.palustris")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Scheuchzeria <- new_sum
bigdf <- subset(bigdf, select = -c(Scheuchzeria.palustris))

#Secale
aa <- apply(is.na(bigdf[,c("Secale", "Secale.cereale")]),1,all)
f_sum <- rowSums(bigdf[,c("Secale", "Secale.cereale")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Secale", "Secale.cereale")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Secale <- new_sum
bigdf <- subset(bigdf, select = -c(Secale.cereale))

#Sedum
aa <- apply(is.na(bigdf[,c("Sedum", "Sedum.acre")]),1,all)
f_sum <- rowSums(bigdf[,c("Sedum", "Sedum.acre")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Sedum", "Sedum.acre")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Sedum <- new_sum
bigdf <- subset(bigdf, select = -c(Sedum.acre))

#Selaginella
aa <- apply(is.na(bigdf[,c("Selaginella", "Selaginella.selaginoides")]),1,all)
f_sum <- rowSums(bigdf[,c("Selaginella", "Selaginella.selaginoides")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Selaginella", "Selaginella.selaginoides")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Selaginella <- new_sum
bigdf <- subset(bigdf, select = -c(Selaginella.selaginoides))

#Silene
aa <- apply(is.na(bigdf[,c("Silene", "Silene.acaulis", "Silene.dioica", "Silene.flos.cuculi", "Silene.viscaria", "Silene.vulgaris")]),1,all)
f_sum <- rowSums(bigdf[,c("Silene", "Silene.acaulis", "Silene.dioica", "Silene.flos.cuculi", "Silene.viscaria", "Silene.vulgaris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Silene", "Silene.acaulis", "Silene.dioica", "Silene.flos.cuculi", "Silene.viscaria", "Silene.vulgaris")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Silene <- new_sum
bigdf <- subset(bigdf, select = -c(Silene.acaulis, Silene.dioica, Silene.flos.cuculi, Silene.viscaria, Silene.vulgaris))

#Sorbus
aa <- apply(is.na(bigdf[,c("Sorbus", "Sorbus.aria", "Sorbus.aucuparia")]),1,all)
f_sum <- rowSums(bigdf[,c("Sorbus", "Sorbus.aria", "Sorbus.aucuparia")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Sorbus", "Sorbus.aria", "Sorbus.aucuparia")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Sorbus <- new_sum
bigdf <- subset(bigdf, select = -c(Sorbus.aria, Sorbus.aucuparia))

#Sparganium
aa <- apply(is.na(bigdf[,c("Sparganium", "Sparganium.emersum", "Sparganium.erectum")]),1,all)
f_sum <- rowSums(bigdf[,c("Sparganium", "Sparganium.emersum", "Sparganium.erectum")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Sparganium", "Sparganium.emersum", "Sparganium.erectum")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Sparganium <- new_sum
bigdf <- subset(bigdf, select = -c(Sparganium.emersum, Sparganium.erectum))

#Spergula
aa <- apply(is.na(bigdf[,c("Spergula", "Spergula.arvensis")]),1,all)
f_sum <- rowSums(bigdf[,c("Spergula", "Spergula.arvensis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Spergula", "Spergula.arvensis")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Spergula <- new_sum
bigdf <- subset(bigdf, select = -c(Spergula.arvensis))

#Stachys
aa <- apply(is.na(bigdf[,c("Stachys", "Stachys.sylvatica")]),1,all)
f_sum <- rowSums(bigdf[,c("Stachys", "Stachys.sylvatica")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Stachys", "Stachys.sylvatica")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Stachys <- new_sum
bigdf <- subset(bigdf, select = -c(Stachys.sylvatica))

#Succisa
aa <- apply(is.na(bigdf[,c("Succisa", "Succisa.pratensis")]),1,all)
f_sum <- rowSums(bigdf[,c("Succisa", "Succisa.pratensis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Succisa", "Succisa.pratensis")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Succisa <- new_sum
bigdf <- subset(bigdf, select = -c(Succisa.pratensis))

#Taxus
aa <- apply(is.na(bigdf[,c("Taxus", "Taxus.baccata")]),1,all)
f_sum <- rowSums(bigdf[,c("Taxus", "Taxus.baccata")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Taxus", "Taxus.baccata")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Taxus <- new_sum
bigdf <- subset(bigdf, select = -c(Taxus.baccata))

#Tilia
aa <- apply(is.na(bigdf[,c("Tilia", "Tilia.cordata")]),1,all)
f_sum <- rowSums(bigdf[,c("Tilia", "Tilia.cordata")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Tilia", "Tilia.cordata")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Tilia <- new_sum
bigdf <- subset(bigdf, select = -c(Tilia.cordata))

#Tofieldia
aa <- apply(is.na(bigdf[,c("Tofieldia", "Tofieldia.pusilla")]),1,all)
f_sum <- rowSums(bigdf[,c("Tofieldia", "Tofieldia.pusilla")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Tofieldia", "Tofieldia.pusilla")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Tofieldia <- new_sum
bigdf <- subset(bigdf, select = -c(Tofieldia.pusilla))

#Trifolium
aa <- apply(is.na(bigdf[,c("Trifolium", "Trifolium.pratense", "Trifolium.repens")]),1,all)
f_sum <- rowSums(bigdf[,c("Trifolium", "Trifolium.pratense", "Trifolium.repens")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Trifolium", "Trifolium.pratense", "Trifolium.repens")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Trifolium <- new_sum
bigdf <- subset(bigdf, select = -c(Trifolium.pratense, Trifolium.repens))

#Trollius
aa <- apply(is.na(bigdf[,c("Trollius", "Trollius.europaeus")]),1,all)
f_sum <- rowSums(bigdf[,c("Trollius", "Trollius.europaeus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Trollius", "Trollius.europaeus")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Trollius <- new_sum
bigdf <- subset(bigdf, select = -c(Trollius.europaeus))

#Typha
aa <- apply(is.na(bigdf[,c("Typha", "Typha.angustifolia", "Typha.latifolia")]),1,all)
f_sum <- rowSums(bigdf[,c("Typha", "Typha.angustifolia", "Typha.latifolia")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Typha", "Typha.angustifolia", "Typha.latifolia")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Typha <- new_sum
bigdf <- subset(bigdf, select = -c(Typha, Typha.angustifolia, Typha.latifolia))

#Ulmus
aa <- apply(is.na(bigdf[,c("Ulmus", "Ulmus.glabra")]),1,all)
f_sum <- rowSums(bigdf[,c("Ulmus", "Ulmus.glabra")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Ulmus", "Ulmus.glabra")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Ulmus <- new_sum
bigdf <- subset(bigdf, select = -c(Ulmus.glabra))

#Valeriana
aa <- apply(is.na(bigdf[,c("Valeriana", "Valeriana.excelsa", "Valeriana.officinalis")]),1,all)
f_sum <- rowSums(bigdf[,c("Valeriana", "Valeriana.excelsa", "Valeriana.officinalis")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Valeriana", "Valeriana.excelsa", "Valeriana.officinalis")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Valeriana <- new_sum
bigdf <- subset(bigdf, select = -c(Valeriana.excelsa, Valeriana.officinalis))

#Viburnum
aa <- apply(is.na(bigdf[,c("Viburnum", "Viburnum.opulus")]),1,all)
f_sum <- rowSums(bigdf[,c("Viburnum", "Viburnum.opulus")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Viburnum", "Viburnum.opulus")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Viburnum <- new_sum
bigdf <- subset(bigdf, select = -c(Viburnum.opulus))

#Vicia
aa <- apply(is.na(bigdf[,c("Vicia", "Vicia.cracca", "Vicia.sylvatica")]),1,all)
f_sum <- rowSums(bigdf[,c("Vicia", "Vicia.cracca", "Vicia.sylvatica")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Vicia", "Vicia.cracca", "Vicia.sylvatica")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Vicia <- new_sum
bigdf <- subset(bigdf, select = -c(Vicia.cracca, Vicia.sylvatica))

#Viscum
aa <- apply(is.na(bigdf[,c("Viscum", "Viscum.album")]),1,all)
f_sum <- rowSums(bigdf[,c("Viscum", "Viscum.album")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Viscum", "Viscum.album")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Viscum <- new_sum
bigdf <- subset(bigdf, select = -c(Viscum.album))

#Salix
aa <- apply(is.na(bigdf[,c("Salix", "Salix.herbacea")]),1,all)
f_sum <- rowSums(bigdf[,c("Salix", "Salix.herbacea")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Salix", "Salix.herbacea")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Salix <- new_sum
bigdf <- subset(bigdf, select = -c(Salix.herbacea))

#Isoetes
aa <- apply(is.na(bigdf[,c("Isoetes", "Isoetes.lacustris")]),1,all)
f_sum <- rowSums(bigdf[,c("Isoetes", "Isoetes.lacustris")],na.rm=TRUE)
t_sum <- rowSums(bigdf[,c("Isoetes", "Isoetes.lacustris")])
new_sum = f_sum # create a new vector the same size as f_sum
for(i in 1:length(aa)){
  if(aa[i]){
    new_sum[i] = t_sum[i]
  }
  else{
    new_sum[i] = f_sum[i]
  }
}
bigdf$Isoetes <- new_sum
bigdf <- subset(bigdf, select = -c(Isoetes.lacustris))


bigdf <- rename(bigdf, Schoenoplectus = Schoenoplectus.lacustris)
bigdf <- rename(bigdf, Tetraedron = Tetraëdron.minimum)
bigdf <- rename(bigdf, Thelypteris = Thelypteris.palustris)
bigdf <- rename(bigdf, Trientalis = Trientalis.europaea)
bigdf <- rename(bigdf, Scleranthus = Scleranthus.annuus)
bigdf <- rename(bigdf, Viola = Viola.palustris)


### Save the file ###
bigdf_genera <- bigdf
save(bigdf_genera,file="bigdf_genera.Rda")
load("bigdf_genera.Rda")
