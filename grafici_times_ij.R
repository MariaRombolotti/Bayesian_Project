#load donazioni_full
donazioni <- donazioni_full[,-c(8,9,10,11)] 
#tolgo colonne che spinelli definisce non significative o con troppe risposte mancanti
i <- 1
j <- 2
k <- 1
r <- matrix(nrow=34864,ncol=2)
ripetizioni <- 1
#creo una matrice dove nella prima colonna ho il numero di ripetizione dell'ID e nella seconda l'ID 
#(l'ID della seconda colonna viene ripetuto tante volte quanto il numero nella stessa riga della prima colonna)
 while (j < 34864 && i < 34864){
   if(donazioni[i,1]==donazioni[j,1]){
     ripetizioni <- ripetizioni+1
     r[k,1] <- ripetizioni
     r[k,2] <- donazioni[i,"CAI"]
   }
   if(donazioni[i,1]!=donazioni[j,1]){
     k <- k+1
     i=i+ripetizioni
     ripetizioni <- 1
     r[k,1] <- ripetizioni
     r[k,2] <- donazioni[i-ripetizioni+1,"CAI"]
   }
   j=j+1
 } 

#cancello le righe che mi davano NA 
i <- 1
while (i <= dim(r)[1]){
  if(is.na(r[i,1]))
    r <- r[-i,]
  else 
    i=i+1
} 

#voglio ora eliminare tutte le righe con ripetizione=1, dato che il nostro modello è ricorrente
x <- r[r[,1]==1,]
j <- 1
i <- 1
while (i <= dim(donazioni)[1] && j <= dim(x)[1] ){
    if(donazioni[i,1] == x[j,2]){
      donazioni <- donazioni[-i,]
    }
    if(donazioni[i,1] < x[j,2])
      i=i+1
    if(donazioni[i,1] > x[j,2])
      j=j+1
} #ci mette tanto
donazioni <- donazioni[-31627,] #lo elimino perchè non tornava mai e non entrava nel mio ciclo precedente

#frequenze delle persone che tornano tot volte:

# che tornano 0 volte:
#sommo tutti quelli che hanno ripetizione=1 perchè vuol dire che non sono mai tornati
n0 <- sum(r[,1]==1)/dim(r)[1] # 0.352845
#che tornano da 1 a 5 volte:
n1_5 <- (sum(r[,1]==2)+sum(r[,1]==3)+sum(r[,1]==4)+sum(r[,1]==5)+sum(r[,1]==6))/dim(r)[1] # 0.4800523
#che tornano 6-10 volte:
n6_10 <- (sum(r[,1]==7)+sum(r[,1]==8)+sum(r[,1]==9)+sum(r[,1]==10)+sum(r[,1]==11))/dim(r)[1] # 0.1102027
#che tornano 11-15 volte:
n11_15 <- (sum(r[,1]==12)+sum(r[,1]==13)+sum(r[,1]==14)+sum(r[,1]==15)+sum(r[,1]==16))/dim(r)[1] # 0.03640724
#che tornano 16-20 volte:
n16_20 <- (sum(r[,1]==17)+sum(r[,1]==18)+sum(r[,1]==19)+sum(r[,1]==20)+sum(r[,1]==21))/dim(r)[1] #  0.01547853
#che tornano 21-25 volte:
n21_25 <- (sum(r[,1]==22)+sum(r[,1]==23)+sum(r[,1]==24)+sum(r[,1]==25)+sum(r[,1]==26))/dim(r)[1] #  0.004578156
#che tornano 26-30 volte:
n26_30 <- (sum(r[,1]==27)+sum(r[,1]==28)+sum(r[,1]==29)+sum(r[,1]==30)+sum(r[,1]==31))/dim(r)[1] #  0.0004360148

n_piu10 <- n11_15+n16_20+n21_22+n26_30 #0.05689993
#tornano più spesso quelli che fumano o no?

n <- c(n0, n1_5, n6_10, n11_15, n16_20, n21_25, n26_30)
ripetizioni <- r[,1]-1 #numero di ritorni di ciascuno dopo la prima volta

#in questo modo toglierei da "ripetizioni" tutti quelli che non sono tornati (quelli con ripetizioni=0),
#ma non voglio toglierli perchè l'istogramma mi permette di vedere che circa il 35% di persone non ritornano
#e quindi è giusto che le eliminiamo perchè non possiamo valutarli come recurrent
#k <- 1
#while(k<=length(ripetizioni)){
#  if(ripetizioni[k]==0)
#    ripetizioni <- ripetizioni[-k]
#  else
#    k=k+1
#}
#dati.hist <- hist(ripetizioni, breaks=c(0,5,10,15,20,25,30), freq=FALSE,main = paste("Histogram of repetition"),col=c("blue4","dodgerblue4","deepskyblue4","deepskyblue3","deepskyblue2","deepskyblue1"))
#str(dati.hist)

hist(ripetizioni, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,29), freq=FALSE,main = paste("Histogram of repetition"),col="darkblue",border="white",right=FALSE)

#voglio vedere come varia la media dei giorni di gap time in relazione al numero di volte che torno a donare
count <- 1
c <- 0
g <- 0
giorni <- 0
rpp<- 0 #ritorni per persona
for (i in 1:31626){
  if(!is.na(donazioni[i,16])){
    giorni <- c(giorni, donazioni[i,16])
    count <- count+1
    c=c(c,count)
  }
  else {
    rpp <- c(rpp,count)
    count=0
    giorni <- c(giorni, 0)
    c=c(c,count)
  }
} 
#ogni volta che il gap time != NA --> cioè che non è la prima volta che il donatore va a donare, salvo in giorni il
#suo gap time e aumento di 1 count cioè aumento le volte in cui il donatore ha donato
#se invece ho un NA significa che per il donatore è la prima volta e perciò metto count a 0 e inserisco lo 0 sia in giorni
#che in c


#rpp mi dice per ogni persona tutte le volte che questa è tornata (calcolando la prima volta=0)
giorni <- giorni[-1] #tolgo poi a giorni e c la prima riga perchè è uno zero extra
c <- c[-1]
plot(giorni[1:4],c[1:4])
media <- sum(giorni[which(c==1)])/length(c[which(c==1)]) #calcolo con un ciclo la media di giorni
#sommando tutti i gap time dei donatori tornati la prima volta e divido per il numero di persone che sono 
#tornate 1 volta e faccio lo stesso per il numero di volte superiori
for (i in 2:20)
  media <- c(media, sum(giorni[which(c==i)])/length(c[which(c==i)]))
  
plot(media,col="darkblue",pch=16,type="b",lty="dashed",xlab="Repetition",ylab="Gap Times") #plotto la media

#voglio vedere come varia il numero di giorni di gap time in relazione agli anni

count <- 1
c <- 0
anno <- 1
g <- 0
giorni <- 0
a <- 1
p <- 1
x <- matrix(c(0,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9),ncol = 2, nrow = 9)
for (i in 1:31626){
  if(!is.na(donazioni[i,16])){
    giorni <- giorni+donazioni[i,16]
    g <- c(g, donazioni[i,16])
    count <- count+1
    c=c(c,count)
    if (giorni>365){
      if ((giorni/365) > floor(giorni/365)){
        a <- a+floor(giorni/365)
        anno[i] <- a
      }
      else
        anno <- c(anno,floor(giorni/365))
      giorni <- giorni-365*floor(giorni/365)
    }
    else{
      anno[i] <- a
    }
    if(anno[i]!=anno[i-1]){
      x[a,1] <- x[a,1]+1
    }
  }
  else {
    count=0
    giorni <- 0
    a <- 1
    c=c(c,count)
    g <- c(g, 0)
    anno[i] <- a
    x[1,1] <- x[1,1]+1
  }
}
g <- g[-1]
media_anno <- sum(g[which(anno==1)])/x[1,1]
for (i in 2:9)
  media_anno <- c(media_anno, sum(g[which(anno==i)]/x[i,1]))
plot(media_anno,col="darkblue",pch=16,type="b",lty="dashed",xlab="Year Repetition",ylab="Gap Times") #plotto la media



#costruisco la matrice times righe = numero di donatori (m), colonne = giorni dalla prima donazione alla donazione n
times <- matrix(ncol = 29, nrow= 5937)
for (n in 1:29){
  for (m in 1: 5937){
    times[m,n]=0
  }
}
i <- 1
j <- 1
times[1,1] <- giorni[2]
j <- 2
k <- 3
while (k <= length(giorni)){
  while(giorni[k]!=0){
    times[i,j] <- times[i,j-1] + giorni[k]
    k <- k+1
    j <- j+1
  }
  if(k < length(giorni)){
    i <- i+1
    j <- 1
    times[i,j] <- giorni[k+1]
    j <- 2
    k <- k+1
  }
  k <- k+1
}

for (n in 1:29){
  for (m in 1: 5937){
    if(times[m,n]==0)
	times[m,n]=-1
  }
}
save(times, file="tempo.Rdata")


year <- 0
y <- 0
i <- 1
oss <- 0
sesso <- 0
al <- 0
fumo <- 0
alcool <- 0
bloodt <- 0
rf <- 0
while(i <= dim(donazioni)[1]){
  if(!is.na(donazioni[i,16]))
   year <- year+donazioni[i,16]
  else{
   y <- c(y,year)
   oss <- c(oss,donazioni[i,18])
   sesso <- c(sesso,donazioni[i,4])
   al <- c(al,donazioni[i,8])
   fumo <- c(fumo,donazioni[i,6])
   alcool <- c(alcool,donazioni[i,7])
   bloodt <- c(bloodt,donazioni[i,11])
   rf <- c(rf,donazioni[i,12])
   year <- 0
  }
  i <- i+1
}

y <- y[-c(1,2)]
y <- y/360
oss <- oss[-1]
sesso <- sesso[-1]
alcool <- alcool[-1]
fumo <- fumo[-1]
rf <- rf[-1]
bloodt <- bloodt[-1]
al <- al[-1]
oss <- oss/365
rpp <- rpp[-c(1,2)]
rpp <- c(rpp,1)
rpp2 <- rpp+1
Rate <- rpp2/oss
hist(Rate,freq=FALSE, xlim=c(0,4),ylim=c(0,0.5),breaks=100,col="darkblue",border="white")

boxplot(rate ~ sesso,ylim=c(0,6),col=c("darkblue","pink"),xlab="Sex",ylab="Donation Rate")


# Histogram of the gap times
data <- get(load("donazioni_full.RData"))
attach(data)
head(data)

## log(gap_times)
gap_t<-na.omit(as.numeric(gap_time))
hist(gap_t,main = "Histogram of the gap times",breaks=175,xlab="gap times",las=1,border="white",xlim=c(90,500),
     col="darkblue",right=FALSE)
abline(v=90,col='red',lwd=2)
abline(v=180,col='red',lwd=2)

plot(as.numeric(gap_time2))