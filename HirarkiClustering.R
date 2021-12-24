#install library
library(cluster)
library(factoextra)
library(tidyverse)
library(NbClust)

#membuat dataahc
dataahc=read.delim("clipboard")
#atau
dataahc <- read.csv(file.choose(), header = TRUE) 
View(dataahc)

#mengecek dataahc yang kosong
summary(dataahc)

#mengisi dataahc kosong dengan median
dataahc$Stok.5=ifelse(is.na(dataahc$Stok.5), mean(dataahc$Stok.5, na.rm = TRUE), dataahc$Stok.5)
dataahc$Sstok.7=ifelse(is.na(dataahc$Stok.7), mean(dataahc$Stok.7, na.rm = TRUE), dataahc$Stok.7)
summary(dataahc)

#transformasi dengan scale
cobaahc=scale(dataahc[,3:15])
view(cobaahc)

#menentukan korelasi menggunakan metode hirarki
jarak=dist(dataahc)
jarak
hc.res=hclust(jarak, method = "ave")
hc.res

plot(hc.res, hc.cut$cluster, labels = hc.cut$cluster)
rect.hclust(hc.res, 6)


hc.cut <- hcut(cobaahc, k = 10, hc_method = "ave")
summary(hc.cut)
# Visualize silhouhette information
fviz_silhouette(hc.cut)
#untuk melihat index silhoutte
summary(hc.cut)
si.ahc <- summary(hc.cut)
#total average silhouette
si.ahc$avg.width

#membentuk hasil cluster di tabel
hasilhc=dataahc.frame(id=hc.cut$cluster, cutree(hc.res, k=6))
view(hasilhc)

#melihat deskripsi dataahc
dataahcukt[, 1:37]%>%
  mutate(cluster=anggotasingle[, 2])%>%
  group_by(cluster)%>%
  summarise_all("mean")

# Hierarchical clustering
hc.res <- eclust(mydataahcukt, "hclust", k = 6, hc_metric = "euclidean", 
                 hc_method = "ward.D2", graph = FALSE)

# Visualize dendrograms
fviz_dend(hc.res, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)
