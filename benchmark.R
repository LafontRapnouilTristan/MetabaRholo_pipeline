library(ggplot2)
library(ggpubr)

bench_files <- list.files("data/benchmarks/",full.names = T)
for(j in 1:length(bench_files)){
  bench <- do.call(rbind, lapply(list.files(bench_files[j],full.names = T), read.table, sep="\t",header=T))
  bench$steps <- gsub(list.files(bench_files[j]),pattern=".txt",replacement = "")
  
  
  
  plot_list <- list()
  log_list <- list()
  log_list <- list()
  for (i in 1:(ncol(bench)-1)){
    log_list[[i]] <- local({
      i <- i
      if (i>2&&i<7){
        val <- log(bench[,i]+1)
        name <- paste0("log_",names(bench)[i])
      }
      else{
        val <- bench[,i]
        name <- names(bench)[i]
      } 
      
      ggplot(bench,aes(y=val, x=steps,color=steps)) +
        geom_point()+
        ylab(name)
      
    })  
    
    
    plot_list[[i]] <- local({
      i <- i
      val <- bench[,i]
      name <- names(bench)[i]
      ggplot(bench,aes(y=val, x=steps,color=steps)) +
        geom_point()+
        theme(panel.grid.major.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 45))+
        theme(axis.title.x = element_blank())+
        ylab(name)
    })
  }
  plot_bench <- ggarrange(plotlist = plot_list,ncol = 3,nrow=ceiling(10/3),common.legend = T)
  plot_log <- ggarrange(plotlist = log_list,ncol = 3,nrow=ceiling(10/3),common.legend = T)
  ggsave(plot_bench,filename = paste0("figures/bench_raw_",basename(bench_files[j]),".jpg"), device = "jpg", height = 10, width = 10)
  ggsave(plot_log,filename = paste0("figures/bench_log_",basename(bench_files[j]),".jpg"), device = "jpg", height = 10, width = 10)
}

