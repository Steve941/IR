#WD
# setwd("E:/Dropbox/IR-Phase FIM-Statistik/Data/Sentix")
setwd(file.path(getwd(), "..", "Data", "Sentix")) ### relative Referenzierung

#package for reading the excel sheets
# install.packages("openxlsx")
library(openxlsx)

#sheet names -> "DAX": 1 month sentiment; "DAXm": 6 month sentiment ("m" for medium term)
sheets=c("DAX","DAXm","TEC","TECm","ESX50","ESX50m","SP5","SP5m","NASDAQ","NASDAQm","NIKKEI","NIKKEIm","BUND","BUNDm","TBOND","TBONDm")
relevant_rows=c("Datum","P+","Pn","P-","I+","In","I-","G+","Gn","G-")

## call directly by name (and not via counting variable)
# sentix=list()
# 
# for(i in 1:length(sheets)){
# 	
# 	#xlsx version of the excel sheet, as version from sentix is in xls-format, we need xlsx
# 	sentix[[i]]=read.xlsx("sentix_anzahlen_bis_02092016xlsx.xlsx",sheet=sheets[i],colNames=T,rowNames=F,detectDates=T)
# 	sentix[[i]]=sentix[[i]][,relevant_rows]
# 	sentix[[i]]=sentix[[i]][order(sentix[[i]][,1]),]
# 
# 	next_col=ncol(sentix[[i]])+1
# 	
# 	for(j in 1:nrow(sentix[[i]])){
# 		
# 		#compute dispersion measures
# 		sentix[[i]][j,next_col]=var(c(rep(1,sentix[[i]][j,"P+"]),rep(0,sentix[[i]][j,"Pn"]),rep(-1,sentix[[i]][j,"P-"])))
# 		sentix[[i]][j,(next_col+1)]=var(c(rep(1,sentix[[i]][j,"I+"]),rep(0,sentix[[i]][j,"In"]),rep(-1,sentix[[i]][j,"I-"])))
# 		sentix[[i]][j,(next_col+2)]=var(c(rep(1,sentix[[i]][j,"G+"]),rep(0,sentix[[i]][j,"Gn"]),rep(-1,sentix[[i]][j,"G-"])))
# 		
# 		# compute Modified Herfindahl Index of Li & Li (2014)
# 		sentix[[i]][j,(next_col+3)]=(-1)*((sentix[[i]][j,"P+"]/sum(sentix[[i]][j,c("P+","Pn","P-")]))^2+2*((sentix[[i]][j,"Pn"]/sum(sentix[[i]][j,c("P+","Pn","P-")]))^2)+(sentix[[i]][j,"P-"]/sum(sentix[[i]][j,c("P+","Pn","P-")]))^2)
# 		sentix[[i]][j,(next_col+4)]=(-1)*((sentix[[i]][j,"I+"]/sum(sentix[[i]][j,c("I+","In","I-")]))^2+2*((sentix[[i]][j,"In"]/sum(sentix[[i]][j,c("I+","In","I-")]))^2)+(sentix[[i]][j,"I-"]/sum(sentix[[i]][j,c("I+","In","I-")]))^2)
# 		sentix[[i]][j,(next_col+5)]=(-1)*((sentix[[i]][j,"G+"]/sum(sentix[[i]][j,c("G+","Gn","G-")]))^2+2*((sentix[[i]][j,"Gn"]/sum(sentix[[i]][j,c("G+","Gn","G-")]))^2)+(sentix[[i]][j,"G-"]/sum(sentix[[i]][j,c("G+","Gn","G-")]))^2)
# 		
# 		colnames(sentix[[i]])[next_col:(next_col+5)]=c("P_disp","I_disp","G_disp","P_herf","I_herf","G_herf")
# 	}
# 	
# }


sentix=list()

for(i in sheets){
    
    #xlsx version of the excel sheet, as version from sentix is in xls-format, we need xlsx
    sentix[[i]]=read.xlsx("sentix_anzahlen_bis_02092016xlsx.xlsx",sheet=i,colNames=T,rowNames=F,detectDates=T)
    sentix[[i]]=sentix[[i]][,relevant_rows]
    sentix[[i]]=sentix[[i]][order(sentix[[i]][,1]),]
    
    next_col=ncol(sentix[[i]])+1
    
    for(j in 1:nrow(sentix[[i]])){
        
        #compute dispersion measures
        sentix[[i]][j,next_col]=var(c(rep(1,sentix[[i]][j,"P+"]),rep(0,sentix[[i]][j,"Pn"]),rep(-1,sentix[[i]][j,"P-"])))
        sentix[[i]][j,(next_col+1)]=var(c(rep(1,sentix[[i]][j,"I+"]),rep(0,sentix[[i]][j,"In"]),rep(-1,sentix[[i]][j,"I-"])))
        sentix[[i]][j,(next_col+2)]=var(c(rep(1,sentix[[i]][j,"G+"]),rep(0,sentix[[i]][j,"Gn"]),rep(-1,sentix[[i]][j,"G-"])))
        
        # compute Modified Herfindahl Index of Li & Li (2014)
        sentix[[i]][j,(next_col+3)]=(-1)*((sentix[[i]][j,"P+"]/sum(sentix[[i]][j,c("P+","Pn","P-")]))^2+2*((sentix[[i]][j,"Pn"]/sum(sentix[[i]][j,c("P+","Pn","P-")]))^2)+(sentix[[i]][j,"P-"]/sum(sentix[[i]][j,c("P+","Pn","P-")]))^2)
        sentix[[i]][j,(next_col+4)]=(-1)*((sentix[[i]][j,"I+"]/sum(sentix[[i]][j,c("I+","In","I-")]))^2+2*((sentix[[i]][j,"In"]/sum(sentix[[i]][j,c("I+","In","I-")]))^2)+(sentix[[i]][j,"I-"]/sum(sentix[[i]][j,c("I+","In","I-")]))^2)
        sentix[[i]][j,(next_col+5)]=(-1)*((sentix[[i]][j,"G+"]/sum(sentix[[i]][j,c("G+","Gn","G-")]))^2+2*((sentix[[i]][j,"Gn"]/sum(sentix[[i]][j,c("G+","Gn","G-")]))^2)+(sentix[[i]][j,"G-"]/sum(sentix[[i]][j,c("G+","Gn","G-")]))^2)
        
        colnames(sentix[[i]])[next_col:(next_col+5)]=c("P_disp","I_disp","G_disp","P_herf","I_herf","G_herf")
    }
    
}


#check for NAs
nas=0
for(a in sentix){
	nas[a]=sum(is.na(sentix[[a]]))
}

nas
#->seems that for TBOND und TBONDm no private investors are surveyed

save(sentix, file = "SentixCalculated")