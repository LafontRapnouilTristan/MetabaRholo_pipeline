library(microeco)
library(microViz)


meco_mo16s <- file2meco::phyloseq2meco(physeqlists$mother_16s)

meco_mo16s$sample_sums() %>% range;sum(meco_mo16s$sample_sums())
# 
# meco_mo16s$filter_pollution(taxa = c("mitochondria", "chloroplast"))
# meco_mo16s$tidy_dataset()
# 
# meco_mo16s$sample_sums() %>% range;sum(meco_mo16s$sample_sums())


meco_mo16s$tax_table <- meco_mo16s$tax_table[,c(1,18:23)]
meco_mo16s$cal_abund(rel = T)
filtered_16s <- clone(meco_mo16s)

# filtered_16s$filter_taxa(rel_abund = 0.00001, freq = 0.01)

t1_16smo <- trans_abund$new(dataset = filtered_16s, taxrank = "Family_conf", ntaxa = 12)
t1_16smo$plot_bar(others_color = "grey70",facet = c("project","organ"), xtext_keep = FALSE, legend_text_italic = FALSE)

grep("Mitochondria",meco_mo16s$tax_table)
meco_mo16s$tax_table[760,]


filtered_16s2 <- clone(meco_mo16s)
filtered_16s2$filter_pollution(taxa = c("mitochondria", "chloroplast"))
filtered_16s2$tidy_dataset()

grep("Mitochondria",filtered_16s2$tax_table)
grep("mitochondria",filtered_16s2$tax_table)

filtered_16s2$cal_abund(rel = T)
t1_16smo2 <- trans_abund$new(dataset = filtered_16s2, taxrank = "Family_conf", ntaxa = 12)
t1_16smo2$plot_bar(others_color = "grey70",facet = c("project","organ"), xtext_keep = FALSE, legend_text_italic = FALSE)

View(filtered_16s2$taxa_abund$Family_conf)
grep('itochondria',rownames(filtered_16s2$taxa_abund$Family_conf))


filtered_16s_phy <- file2meco::meco2phyloseq(filtered_16s2)
library(ggplot2)
ggsave(plot_bar(filtered_16s_phy,fill="Order_conf"),"../../tset.png",height=15,width=20)



## get seq


mito <- meco_mo16s$tax_table[,1][which(grepl("Mitochondria",meco_mo16s$tax_table$Family_conf))]
mito <- gsub("s__","",mito)
motu <- rownames(meco_mo16s$tax_table)[which(grepl("Mitochondria",meco_mo16s$tax_table$Family_conf))]

fasta <- c(rbind(paste0(">",motu),mito))
write(fasta,"../../mito.fasta")


all <- meco_mo16s$tax_table[,1]
all <- gsub("s__","",all)
motuall <- rownames(meco_mo16s$tax_table)

fasta2 <- c(rbind(paste0(">",motuall),all))
write(fasta2,"../../all.fasta")


##

grep("Rickettsiales",meco_mo16s$tax_table[,5]) # Rickettsiale as order
grep("Rickettsiales",meco_mo16s$tax_table[,6]) # Rickettsiales as family (non mito)
grep("Mitochondria",meco_mo16s$tax_table[,6]) # Mitochondria as family
grep("Rickettsiales",meco_mo16s$tax_table[,5])%in%grep("Mitochondria",meco_mo16s$tax_table[,6]) # Non mito rickettsiales
sum(grep("Rickettsiales",meco_mo16s$tax_table[,5])%in%grep("Mitochondria",meco_mo16s$tax_table[,6])) #175 mitochondria
sum(!(grep("Rickettsiales",meco_mo16s$tax_table[,5])%in%grep("Mitochondria",meco_mo16s$tax_table[,6])))  #76 rickett non mito

rickett_motu <- rownames(meco_mo16s$tax_table)[grep("Rickettsiales",meco_mo16s$tax_table[,5])]
rickett_mito <- rownames(meco_mo16s$tax_table)[grep("Rickettsiales",meco_mo16s$tax_table[,5])][grep("Rickettsiales",meco_mo16s$tax_table[,5])%in%grep("Mitochondria",meco_mo16s$tax_table[,6])]
rickett_non_mito <- rownames(meco_mo16s$tax_table)[grep("Rickettsiales",meco_mo16s$tax_table[,5])][!(grep("Rickettsiales",meco_mo16s$tax_table[,5])%in%grep("Mitochondria",meco_mo16s$tax_table[,6]))]

View(meco_mo16s$otu_table)
rickett_mito_count <- rowSums(meco_mo16s$otu_table[rownames(meco_mo16s$otu_table)%in%rickett_mito,])
rickett_motu_count <- rowSums(meco_mo16s$otu_table[rownames(meco_mo16s$otu_table)%in%rickett_motu,])
rickett_non_mito_count <- rowSums(meco_mo16s$otu_table[rownames(meco_mo16s$otu_table)%in%rickett_non_mito,])

sum(rickett_mito_count)
sum(rickett_motu_count)
sum(rickett_non_mito_count)


rickett_mito_count[which.max(rickett_mito_count)]
meco_mo16s$tax_table[which(rownames(meco_mo16s$tax_table)=="MOTU_09996"),]

plot(rickett_mito_count)
plot(rickett_motu_count)
plot(rickett_non_mito_count)
