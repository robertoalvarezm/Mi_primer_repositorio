library("Biostrings")
library(ggplot2)

genoma<-readDNAStringSet("~/Downloads/NC_001802.fna")
genome$chr21

x<-c()
y<-c()

omega_mat<-function(v1,v2){
  
  mat<-matrix(c(0.5,0,0,0.5),nrow=2)
  v1<-matrix(v1,nrow=2)
  v2<-matrix(v2,nrow=2)
  return(mat %*% v1 + v2/2)
  
}

chaos_game<-function(genoma){
  x<-c()
  y<-c()
  x[1]<-runif(1)
  y[1]<-runif(1)
  a<-matrix(c(0,0),nrow = 2)
  c<-matrix(c(0,1),nrow=2)
  g<-matrix(c(1,1),nrow=2)
  t<-matrix(c(1,0),nrow=2)
  
  #for(i in 1:length(genome)){
    for(j in 1:length(genoma[[1]])){
      if (genoma[[1]][j]==DNAString("A")){
        x[j+1]<-omega_mat(matrix(c(x[j],y[j]),nrow=2),a)[1,]
        y[j+1]<-omega_mat(matrix(c(x[j],y[j]),nrow=2),a)[2,]
      }else if(genoma[[1]][j]==DNAString("C")){
        x[j+1]<-omega_mat(matrix(c(x[j],y[j]),nrow=2),c)[1,]
        y[j+1]<-omega_mat(matrix(c(x[j],y[j]),nrow=2),c)[2,]
      }else if(genoma[[1]][j]==DNAString("G")){
        x[j+1]<-omega_mat(matrix(c(x[j],y[j]),nrow=2),g)[1,]
        y[j+1]<-omega_mat(matrix(c(x[j],y[j]),nrow=2),g)[2,]
      }else if (genoma[[1]][j]==DNAString("T")){ 
      x[j+1]<-omega_mat(matrix(c(x[j],y[j]),nrow=2),t)[1,]
      y[j+1]<-omega_mat(matrix(c(x[j],y[j]),nrow=2),t)[2,]
      }else{
        
      }
      #print(j)
      #plot(x,y)
      df<-data.frame(x=x,y=y)
    }
  
  #plot(x,y,cex=0.1,col="orange")
  
  opt <-  theme(legend.position  = "none",
                panel.background = element_rect(fill="black", color="white"),
                plot.background  = element_rect(fill="black"),
                axis.ticks       = element_blank(),
                panel.grid       = element_blank(),
                axis.title       = element_blank(),
                axis.text        = element_blank())
  
  p1 <- ggplot(df) + geom_point(aes(x = x, y = y), shape = 46, alpha = 100, color = "white") + coord_fixed() +  opt
  ggsave(paste(names(genoma),"2.pdf"), p1, height = 8, width =8 , units = 'in')
 # }
  
}
