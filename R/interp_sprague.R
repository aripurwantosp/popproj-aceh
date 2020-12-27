interp_sprague <- function(dta,end = 75){
  
  #____________________________________________________________________________
  #Fungsi untuk disagregasi/interpolasi umur tunggal metode Sprague
  #
  #Ditulis oleh:
  # Ari Purwanto Sarwo Prasojo
  # 2020
  #
  #Input
  # dta : vektor jumlah penduduk umur 5 kelompok umur 5 tahunan
  # end : umur terbuka (kelompok umur terakhir)
  #Output
  # vektor jumlah penduduk umur 1 tahunan
  #____________________________________________________________________________
  
  if(end %% 5 != 0){
    stop("Umur terbuka tidak sesuai, harus kelipatan 5")
  } 
  
  grp5 <- c(paste(seq(0, end-5, by=5),
                  seq(0+5-1, end-1, by=5), sep = "-"),
            paste0(end,"+")) 
  
  ngrp <- length(dta)
  
  if(length(grp5) != ngrp){
    stop("Banyaknya kelompok umur tidak sesuai dengan umur terbuka")
  }
  
  #koefisien sprague
  koef <- 
    data.frame(G1 = c(0.3616, 0.264, 0.184, 0.12, 
                      0.0704, 0.0336, 0.008, -0.008, -0.016, -0.0176, -0.0128, 
                      -0.0016, 0.0064, 0.0064, 0.0016, rep(0, 10)),
               G2 = c(-0.2768, 
                      -0.096, 0.04, 0.136, 0.1968, 0.2272, 0.232, 0.216, 0.184, 
                      0.1408, 0.0848, 0.0144, -0.0336, -0.0416, -0.024, -0.0144, 
                      -0.008, 0, 0.008, 0.0144, 0.0176, 0.016, 0.008, -0.008, 
                      -0.0336),
               G3 = c(0.1488, 0.04, -0.032, -0.072, -0.0848, 
                      -0.0752, -0.048, -0.008, 0.04, 0.0912, 0.1504, 0.2224, 
                      0.2544, 0.2224, 0.1504, 0.0912, 0.04, -0.008, -0.048, 
                      -0.0752, -0.0848, -0.072, -0.032, 0.04, 0.1488),
               G4 = c(-0.0336, 
                      -0.008, 0.008, 0.016, 0.0176, 0.0144, 0.008, 0, -0.008, 
                      -0.0144, -0.024, -0.0416, -0.0336, 0.0144, 0.0848, 0.1408, 
                      0.184, 0.216, 0.232, 0.2272, 0.1968, 0.136, 0.04, -0.096, 
                      -0.2768),
               G5 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0016, 
                      0.0064, 0.0064, -0.0016, -0.0128, -0.0176, -0.016, -0.008, 
                      0.008, 0.0336, 0.0704, 0.12, 0.184, 0.264, 0.3616))
  
  #-panel
  #-1: pertama
  #-2: dekat dengan panel pertama
  #-3: tengah
  #-4: dekat panel terakhir
  #-5: terakhir
  koef <- cbind(panel = rep(1:5, each = 5), koef)
  
  #pop umur open-ended
  popend <- dta[ngrp]
  
  #interpolasi
  interp <- sapply(1:(ngrp-1),FUN=function(i){
    
    if(i==1){
      #panel pertama
      mult <- koef %>% filter(panel==1) %>% select(-panel)
      Ns <- dta[seq(i,i+4,1)]
      
      #dekat panel pertama  
    }else if(i==2){
      mult <- koef %>% filter(panel==2) %>% select(-panel)
      Ns <- dta[seq(i-1,i+3,1)]
      
      #dekat panel terakhir
    }else if(i==(ngrp-2)){
      mult <- koef %>% filter(panel==4) %>% select(-panel)
      Ns <- dta[seq(i-3,i+1,1)]
      
      #panel terakhir
    }else if(i==(ngrp-1)){
      mult <- koef %>% filter(panel==5) %>% select(-panel)
      Ns <- dta[seq(i-4,i,1)]
      
      #panel tengah
    }else{
      mult <- koef %>% filter(panel==3) %>% select(-panel)
      Ns <- dta[seq(i-2,i+2,1)]
    }
    
    #interpolasi
    mult <- as.matrix(mult)
    Ns <- matrix(Ns,ncol=1)
    pop <- mult%*%Ns %>% as.vector
    
  }) %>% as.vector
  
  pop <- c(interp,popend)
  names(pop) <- c(0:(end-1), paste0(end,"+"))
  
  return(pop)
  
}