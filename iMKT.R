run <- function(geneid="", pop="", CEU="",GBR="",FIN="",IBS="",TSI="",ESN="",GWD="",LWK="",MSL="",YRI="",ACB="",ASW="",CDX="",CHB="",CHS="",JPT="",KHV="",BEB="",GIH="",ITU="",PJL="",STU="",CLM="",MXL="",PEL="",PUR="", ...) {

# Print header of HTML
out("<!DOCTYPE html><html><head><meta charset='UTF-8'><link rel='stylesheet' type='text/css' href='http://pophuman.uab.cat/css/custom_QA.css' /><style>body {max-width: 1000px;}")
#################Generate for loop to generate the CSS for the checkboxes##############
#popil <- c("CEU","GBR","FIN","IBS","TSI","YRI","CDX","CHB","CHS","JPT","KHV","BEB","GIH","ITU","PJL","STU")
popil <- c("CEU","GBR","FIN","IBS","TSI","ESN","GWD","LWK","MSL","YRI","ACB","ASW","CDX","CHB","CHS","JPT","KHV","BEB","GIH","ITU","PJL","STU","CLM","MXL","PEL","PUR")
mycols<-c("#2d74b2","#5691c4", "#6fa6d6", "#8ab9e3","#acd1f2", "#f5ec05", "#f7f14a", "#faf56b", "#f5f287", "#f5f39f", "#faf8b9","#fafabe", "#008f00", "#33b033", "#37b337", "#6dd66d", "#90de90", "#8e3ea3", "#a965ba", "#c587d6", "#d5a3e3", "#d3a2de", "#ab3b35", "#ba5852", "#cc7570", "#de9d99")

out(".control {
    font-family: arial;
    display: block;
    position: relative;
    padding-left: 30px;
    margin-bottom: 15px;
    padding-top: 3px;
    cursor: pointer;
    font-size: 16px;
    width: 4%;
    float: left;
}
    .control input {
        position: absolute;
        z-index: -1;
        opacity: 0;
    }")
for (e in 1:length(popil)) {
  out(paste0(".", popil[e], " {
    position: absolute;
    top: 2px;
    left: 0;
    height: 20px;
    width: 20px;
    background: ", mycols[e],";
    border: 0px solid #000000;
}", ".control input:checked ~ .", popil[e], " {
    background: ", mycols[e],";}.", popil[e], ":after {
    box-sizing: unset;
    content: '';
    position: absolute;
    display: none;
}", ".control input:checked ~ .", popil[e], ":after {
    display: block;
}", ".control-", popil[e], " .", popil[e], ":after {
    left: 8px;
    top: 4px;
    width: 3px;
    height: 8px;
    border: solid #000000;
    border-width: 0 2px 2px 0;
    transform: rotate(45deg);
}"))
}
out("</style></head><body><div class='classDiv_content'>")

# Stop execution if geneid is NULL
if (is.na(geneid) || geneid == '') {
ohead("This annotation is not a RefSeq coding gene. Integrative MKT is not available.", level=3)
} else {

# Prepare variables
ID_num<-as.integer(geneid)  		# oprint(ID_num)

# Print the selector form
#popil <- c("CEU","GBR","FIN","IBS","TSI","ESN","GWD","LWK","MSL","YRI","ACB","ASW","CDX","CHB","CHS","JPT","KHV","BEB","GIH","ITU","PJL","STU","CLM","MXL","PEL","PUR")
#popil <- c("CEU","GBR","FIN","IBS","TSI","ESN","GWD","LWK","MSL","YRI","ACB","ASW","CDX","CHB","CHS","JPT","KHV","BEB","GIH","ITU","PJL","STU","CLM","MXL","PEL","PUR")
#mycols<-c("#2d74b2","#5691c4", "#6fa6d6", "#8ab9e3","#acd1f2", "#f5ec05", "#f7f14a", "#faf56b", "#f5f287", "#f5f39f", "#faf8b9","#fafabe", "#008f00", "#33b033", "#37b337", "#6dd66d", "#90de90", "#8e3ea3", "#a965ba", "#c587d6", "#d5a3e3", "#d3a2de", "#ab3b35", "#ba5852", "#cc7570", "#de9d99")

#popil <- sort(popil)

out("<form><iframe allowfullscreen src='//e.infogram.com/pophuman-20205262?src=embed' title='PopHuman' width='950' height='370' scrolling='no' frameborder='0' style='border:none;'></iframe>")
out("<br><b>Select one or more populations:</b><br>")
oinput("geneid", geneid, type="hidden")

popul <- c()
out("<div>")
for (i in 1:length(popil)) {
if (get(popil[i]) == "") {
  out(paste0("<label class='control control-", popil[i], "'>"))
  out(popil[i])
#  out(paste0("<input type='checkbox' checked>"))
  oinput(popil[i], type="checkbox", checked=FALSE)
  out(paste0("<div class='", popil[i], "'></div>"))
  out("</label>")
 } else {
  out(paste0("<label class='control control-", popil[i], "'>"))
  out(popil[i])
#  out(paste0("<input type='checkbox' >"))
  oinput(popil[i], type="checkbox", checked=TRUE)
  out(paste0("<div class='", popil[i], "'></div>"))
  out("</label>")
 popul <- c(popul, popil[i])
}
}
out("</div>")
out("<br>")
osubmit(name="Display")
out("<br><br><font color='red'>Results might take some seconds to show after submitting your query. Please be patient!</font><br><br>")
out("</form>")

# Fetch the data
if (pop == "all") {
  #pop <- "CEU,GBR,FIN,IBS,TSI,ESN,GWD,LWK,MSL,YRI,ACB,ASW,CDX,CHB,CHS,JPT,KHV,BEB,GIH,ITU,PJL,STU,CLM,MXL,PEL,PUR"
  # Filter data to get rows corresponding to the geneid and populations of interest
  thisGene <- dplyr::filter(allGenes, name == ID_num)
  popul <- as.character(unique(thisGene$population))
} else if (pop != "") {
  popul <-unlist(strsplit(pop, ","))  	# oprint(pops)
  # Filter data to get rows corresponding to the geneid and populations of interest
  thisGene <- dplyr::filter(allGenes, name == ID_num & population %in% popul)
  popul <- as.character(unique(thisGene$population))

#string<-paste("grep -P 'population|", ID_num, "' /var/www/html/pophuman/files/genes/GenesData_ALL.tab", sep="\t")
#thisGene<-fread(string)
#popul<- c("YRI", "CHB", "CEU")
#thisGene<-thisGene[thisGene$population %in% popul,] ## Filtering the main data using the list popul
#oprint(as.character(popul)) ##Factor problem
} else if (length(popul) > 0) {
  thisGene <- dplyr::filter(allGenes, name == ID_num & population %in% popul)
  popul <- as.character(unique(thisGene$population))
} else {
  thisGene <- data.frame(matrix(ncol = 1, nrow = 0))
}

# Stop execution if there is no data for this geneid
if (length(popul) == 0) {
} else if (nrow(thisGene) == 0) {
ohead("This annotation is not a RefSeq coding gene. Integrative MKT is not available.", level=3)
} else {

# Start HTML output
ohead("Descriptive statistics", level=2)

## Descriptive statistics
## Generation of the table of descriptive statistics
descriptive_statistics<-function(z){
  desc_stat<-select(z, population, S, Pi, Divsites, D, K, Pi_sel_fold0, Pi_neu_fold0, K_sel_fold0, K_neu_fold0)
  desc_stat<-mutate(desc_stat, "πa/πs" = Pi_sel_fold0/Pi_neu_fold0, "Ka/Ks" = K_sel_fold0/K_neu_fold0)
  desc_stat<-select(desc_stat, population, S, Pi, Divsites, D, K, "πa/πs", "Ka/Ks")
  desc_stat<-rename(desc_stat, Population = population, "π" = Pi)
  is.num <- sapply(desc_stat, is.numeric)
  desc_stat[is.num] <- lapply(desc_stat[is.num], round, 5)
  desc_stat
}
## Function call

oprint(kable(descriptive_statistics(thisGene)))
#otable(descriptive_statistics(thisGene), tab = "class='classTable_data'", cs = '</td><td>')

### It can be done with the package DT

## DAF function graphic
DAF_ggplot<- function(x){
  DAF_raw<-select(x, population, DAF)
  DAF_only<-as.data.frame(str_split_fixed(DAF_raw$DAF, ";", 20))
  name_cols<- as.character(seq(0.05,1.0,0.05))
  colnames(DAF_only)<-name_cols
  index<-sapply(DAF_only, is.factor)
  DAF_only[index]<-lapply(DAF_only[index], function(x) as.numeric(as.character(x)))
  DAF<-cbind(DAF_raw,DAF_only)
  DAF<-select(DAF, -DAF)
  DAF_norm<-DAF_only/rowSums(DAF_only)
  DAF_norm<-cbind(select(DAF_raw, population), DAF_norm)
  mm_DAF_norm<-melt(DAF_norm, id='population')
  ordering <- c("CEU","GBR","FIN","IBS","TSI","ESN","GWD","LWK","MSL","YRI","ACB","ASW","CDX","CHB","CHS","JPT","KHV","BEB","GIH","ITU","PJL","STU","CLM","MXL","PEL","PUR")
  mm_DAF_norm <- mutate(mm_DAF_norm, population = factor(population, levels = ordering))
  mm_DAF_norm <- arrange(mm_DAF_norm, population)

  ## Generating specific colors for specific populations
  mycols<-c("#2d74b2","#5691c4", "#6fa6d6", "#8ab9e3","#acd1f2", "#f5ec05", "#f7f14a", "#faf56b", "#f5f287", "#f5f39f", "#faf8b9","#fafabe", "#008f00", "#33b033", "#37b337", "#6dd66d", "#90de90", "#8e3ea3", "#a965ba", "#c587d6", "#d5a3e3", "#d3a2de", "#ab3b35", "#ba5852", "#cc7570", "#de9d99")
  names(mycols)<-c("CEU","GBR","FIN","IBS","TSI","ESN","GWD","LWK","MSL","YRI","ACB","ASW","CDX","CHB","CHS","JPT","KHV","BEB","GIH","ITU","PJL","STU","CLM","MXL","PEL","PUR")
  
  ## Plot parameters and its generation
  ggplot(mm_DAF_norm, aes(x=variable, y=value, fill=population)) + geom_bar(stat='identity', position='dodge') + labs(x = "Frequency of derived alleles", y = "Fraction of biallelic sites", title = "Derived Allele Frequency (DAF)") + scale_fill_manual(values = mycols) + theme(legend.box = "horizontal", legend.position = "top", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=15)) + scale_y_continuous(breaks = c(seq(0,1,0.1)))
}

## Function call
p <- WebPlot(900, 600)
plot(DAF_ggplot(thisGene))
out(p)

# Start HTML output
ohead("<br>Recombination (Bhérer <i>et al.</i> 2017), cM/Mb", level=2)

## Recombination
reco<-function(r){
  reco_var<-select(r, recomb_Bherer2017_sexavg, recomb_Bherer2017_females, recomb_Bherer2017_males)
  reco_var<-reco_var[1,]
  colnames(reco_var)<-c("Sex average", "Females", "Males")
  reco_var
}

## Function call
oprint(kable(reco(thisGene)))

# Start HTML output
ohead("<br>Standard MKT (with sites)", level=2)

## Standard MKT tables
standard<-function(pops){
  sites_MKT<-select(thisGene, population, m_neu_fold0, m_sel_fold0, m_sel_UTR5, m_sel_UTR3,
                    m_sel_intron, m_sel_inter)
  sites_MKT<-rename(sites_MKT, '4-fold' = m_neu_fold0, '0-fold' = m_sel_fold0,
                    "5'UTR" = m_sel_UTR5, "3'UTR" = m_sel_UTR3,
                    Intron = m_sel_intron, Intergenic = m_sel_inter)
    ## Reduce the decimals to 5
  is.num <- sapply(sites_MKT, is.numeric)
  sites_MKT[is.num] <- lapply(sites_MKT[is.num], round, 5)

  
  
  P_standard_MKT_sites<-select(thisGene, population, Pneu_fold0, Psel_fold0, Psel_UTR5, Psel_UTR3, 
                               Psel_intron, Psel_inter)
  P_standard_MKT_sites<-rename(P_standard_MKT_sites, "4-fold" = Pneu_fold0, "0-fold" = Psel_fold0,
                               "5'UTR" = Psel_UTR5, "3'UTR" = Psel_UTR3,
                               Intron = Psel_intron, Intergenic = Psel_inter)
    ## Reduce the decimals to 5
  is.num <- sapply(P_standard_MKT_sites, is.numeric)
  P_standard_MKT_sites[is.num] <- lapply(P_standard_MKT_sites[is.num], round, 5)

  
  
  D_standard_MKT_sites<-select(thisGene, population, Dneu_fold0, Dsel_fold0, Dsel_UTR5, Dsel_UTR3,
                               Dsel_intron, Dsel_inter)
  D_standard_MKT_sites<-rename(D_standard_MKT_sites, "4-fold" = Dneu_fold0, "0-fold" = Dsel_fold0,
                               "5'UTR" = Dsel_UTR5, "3'UTR" = Dsel_UTR3,
                               Intron = Dsel_intron, Intergenic = Dsel_inter)
    ## Reduce the decimals to 5
  is.num <- sapply(D_standard_MKT_sites, is.numeric)
  D_standard_MKT_sites[is.num] <- lapply(D_standard_MKT_sites[is.num], round, 5)

  
  
  
  
  alpha_standard_MKT_sites<-select(thisGene, alpha_fold0, alpha_UTR5, alpha_UTR3, alpha_intron, 
                                   alpha_inter)
  alpha_standard_MKT_sites<-rename(alpha_standard_MKT_sites,  "0-fold" = alpha_fold0,
                                   "5'UTR" = alpha_UTR5, "3'UTR" = alpha_UTR3,
                                   Intron = alpha_intron, Intergenic = alpha_inter)
  alpha_standard_MKT_sites<-cbind("4-fold" = "", alpha_standard_MKT_sites)
  alpha_standard_MKT_sites<-cbind(select(thisGene, population), alpha_standard_MKT_sites)
    ## Reduce the decimals to 5
  is.num <- sapply(alpha_standard_MKT_sites, is.numeric)
  alpha_standard_MKT_sites[is.num] <- lapply(alpha_standard_MKT_sites[is.num], round, 5)

  
  
  
  pvalue_standard_MKT_sites<-select(thisGene, test_fold0, test_UTR5, test_UTR3, test_intron,
                                    test_inter)
  pvalue_standard_MKT_sites<-rename(pvalue_standard_MKT_sites,  "0-fold" = test_fold0,
                                    "5'UTR" = test_UTR5, "3'UTR" = test_UTR3,
                                    Intron = test_intron, Intergenic = test_inter)
  pvalue_standard_MKT_sites<-cbind("4-fold" = "", pvalue_standard_MKT_sites)
  pvalue_standard_MKT_sites<-cbind(select(thisGene, population), pvalue_standard_MKT_sites)
    ## Reduce the decimals to 5  
  is.num <- sapply(pvalue_standard_MKT_sites, is.numeric)
  pvalue_standard_MKT_sites[is.num] <- lapply(pvalue_standard_MKT_sites[is.num], round, 5)
  
  
  # variables_standard_MKT<-rbind(sites_MKT, P_standard_MKT_sites, D_standard_MKT_sites, alpha_standard_MKT_sites, pvalue_standard_MKT_sites)
  # 
  # variables_standard_MKT<-split(variables_standard_MKT, variables_standard_MKT$population)
  
  standard_MKT_population<-as.data.frame(matrix(c(sites_MKT[sites_MKT$population==pops,], 
                                                  P_standard_MKT_sites[P_standard_MKT_sites$population==pops,],
                                                  D_standard_MKT_sites[D_standard_MKT_sites$population==pops,], 
                                                  alpha_standard_MKT_sites[alpha_standard_MKT_sites$population==pops,], 
                                                  pvalue_standard_MKT_sites[pvalue_standard_MKT_sites$population==pops,]),ncol=7,nrow=5,byrow = T))
  standard_MKT_population$V1<-NULL
  standard_MKT_population<-cbind(named = c("#sites", "P", "D", "α", "p-value"), standard_MKT_population)
  colnames(standard_MKT_population)<-c(pops,"4-fold", "0-fold", "5'UTR", "3'UTR", "Intron", "Intergenic")
  

  standard_MKT_population
}

#### IMPORTANT
## Here executed the function
oprint(kable(lapply(popul,standard)))

# Start HTML output
ohead("<br>Alternative MKT (with π)", level=2)

## Alternative MKT tables
alternative<-function(pops){
  ##Cargar datos
  sites_MKT<-select(thisGene, population, m_neu_fold0, m_sel_fold0, m_sel_UTR5, m_sel_UTR3,
                    m_sel_intron, m_sel_inter)
  sites_MKT<-rename(sites_MKT, '4-fold' = m_neu_fold0, '0-fold' = m_sel_fold0,
                    "5'UTR" = m_sel_UTR5, "3'UTR" = m_sel_UTR3,
                    Intron = m_sel_intron, Intergenic = m_sel_inter)
    ## Reduce the decimals to 5
  is.num <- sapply(sites_MKT, is.numeric)
  sites_MKT[is.num] <- lapply(sites_MKT[is.num], round, 5)
  
  
  π_alternative_MKT_π<-select(thisGene, population, Pi_neu_fold0, Pi_sel_fold0, Pi_sel_UTR5, Pi_sel_UTR3, 
                              Pi_sel_intron, Pi_sel_inter)
  π_alternative_MKT_π<-rename(π_alternative_MKT_π, "4-fold" = Pi_neu_fold0, "0-fold" = Pi_sel_fold0,
                                "5'UTR" = Pi_sel_UTR5, "3'UTR" = Pi_sel_UTR3,
                                Intron = Pi_sel_intron, Intergenic = Pi_sel_inter)
    ## Reduce the decimals to 5
  is.num <- sapply(π_alternative_MKT_π, is.numeric)
  π_alternative_MKT_π[is.num] <- lapply(π_alternative_MKT_π[is.num], round, 5)

  
  K_alternative_MKT_π<-select(thisGene, population, K_neu_fold0, K_sel_fold0, K_sel_UTR5, K_sel_UTR3,
                              K_sel_intron, K_sel_inter)
  K_alternative_MKT_π<-rename(K_alternative_MKT_π, "4-fold" = K_neu_fold0, "0-fold" = K_sel_fold0,
                              "5'UTR" = K_sel_UTR5, "3'UTR" = K_sel_UTR3,
                              Intron = K_sel_intron, Intergenic = K_sel_inter)
    ## Reduce the decimals to 5
  is.num <- sapply(K_alternative_MKT_π, is.numeric)
  K_alternative_MKT_π[is.num] <- lapply(K_alternative_MKT_π[is.num], round, 5)

  
  
  alpha_alternative_MKT_π<-select(thisGene, alpha_pi_fold0, alpha_pi_UTR5, alpha_pi_UTR3, alpha_pi_intron, 
                                  alpha_pi_inter)
  alpha_alternative_MKT_π<-rename(alpha_alternative_MKT_π,  "0-fold" = alpha_pi_fold0,
                                  "5'UTR" = alpha_pi_UTR5, "3'UTR" = alpha_pi_UTR3,
                                  Intron = alpha_pi_intron, Intergenic = alpha_pi_inter)
  alpha_alternative_MKT_π<-cbind("4-fold" = "", alpha_alternative_MKT_π)
  alpha_alternative_MKT_π<-cbind(select(thisGene, population), alpha_alternative_MKT_π)
    ## Reduce the decimals to 5
  is.num <- sapply(alpha_alternative_MKT_π, is.numeric)
  alpha_alternative_MKT_π[is.num] <- lapply(alpha_alternative_MKT_π[is.num], round, 5)

  
  #crear varialbes
  alternative_MKT_population<-as.data.frame(matrix(c(sites_MKT[sites_MKT$population==pops,], 
                                                     π_alternative_MKT_π[π_alternative_MKT_π$population==pops,],
                                                     K_alternative_MKT_π[K_alternative_MKT_π$population==pops,], 
                                                     alpha_alternative_MKT_π[alpha_alternative_MKT_π$population==pops,]),ncol=7,nrow=4,byrow = T))
  alternative_MKT_population$V1<-NULL
  alternative_MKT_population<-cbind(named = c("#sites", "π", "K", "α"), alternative_MKT_population)
  colnames(alternative_MKT_population)<-c(pops,"4-fold", "0-fold", "5'UTR", "3'UTR", "Intron", "Intergenic")
  alternative_MKT_population
}

#### IMPORTANT
## Here executed the function
oprint(kable(lapply(popul,alternative)))

ohead("<br>Integrative MKT", level=2)
ohead("<i>d</i>=Strongly deleterious; <i>b</i>=Weakly deleterious; ƒ-γ=Old neutral; γ=Recently neutral; α=Adaptive divergence", level=3)

## Integrative MKT tables
integrative<-function(pops){
  Ofold_integrative_MKT<-select(thisGene, population, d_fold0, b_fold0, f_fold0, y_fold0,
                                alpha_cor_fold0, test_cor_fold0, DoS_fold0)
  Ofold_integrative_MKT<-mutate(Ofold_integrative_MKT, f_fold0=f_fold0-y_fold0)
  #Ofold_integrative_MKT<-rename(Ofold_integrative_MKT, "d" = d_fold0, "b" = b_fold0,
  #                  "f-y" = f_fold0, "y" = y_fold0,
  #                  "α" = alpha_cor_fold0, "p-value" = test_cor_fold0, "DoS" = DoS_fold0)
  ## Reduce the decimals to 5
  is.num <- sapply(Ofold_integrative_MKT, is.numeric)
  Ofold_integrative_MKT[is.num] <- lapply(Ofold_integrative_MKT[is.num], round, 5)
  
  UTR5_integrative_MKT<-select(thisGene, population, d_UTR5, b_UTR5, f_UTR5, y_UTR5,
                               alpha_cor_UTR5, test_cor_UTR5, DoS_UTR5)
  UTR5_integrative_MKT<-mutate(UTR5_integrative_MKT, f_UTR5=f_UTR5-y_UTR5)
  #UTR5_integrative_MKT<-rename(UTR5_integrative_MKT, "d" = d_UTR5, "b" = b_UTR5,
  #                              "f-y" = f_UTR5, "y" = y_UTR5,
  #                              "α" = alpha_cor_UTR5, "p-value" = test_cor_UTR5, "DoS" = DoS_UTR5)
  ## Reduce the decimals to 5
  is.num <- sapply(UTR5_integrative_MKT, is.numeric)
  UTR5_integrative_MKT[is.num] <- lapply(UTR5_integrative_MKT[is.num], round, 5)
  
  UTR3_integrative_MKT<-select(thisGene, population, d_UTR3, b_UTR3, f_UTR3, y_UTR3,
                               alpha_cor_UTR3, test_cor_UTR3, DoS_UTR3)
  UTR3_integrative_MKT<-mutate(UTR3_integrative_MKT, f_UTR3=f_UTR3-y_UTR3)
  #UTR3_integrative_MKT<-rename(UTR3_integrative_MKT, "d" = 'd_UTR3', "b" = 'b_UTR3',
  #                             "f-y" = 'f_UTR3', "y" = 'y_UTR3',
  #                             "α" = 'alpha_cor_UTR3', "p-value" = 'test_cor_UTR3', "DoS" = 'DoS_UTR3')
  ## Reduce the decimals to 5
  is.num <- sapply(UTR3_integrative_MKT, is.numeric)
  UTR3_integrative_MKT[is.num] <- lapply(UTR3_integrative_MKT[is.num], round, 5)
  
  intron_integrative_MKT<-select(thisGene, population, d_intron, b_intron, f_intron, y_intron,
                                 alpha_cor_intron, test_cor_intron, DoS_intron)
  intron_integrative_MKT<-mutate(intron_integrative_MKT, f_intron=f_intron-y_intron)
  #intron_integrative_MKT<-rename(intron_integrative_MKT, "d" = d_intron, "b" = b_intron,
  #                             "f-y" = f_intron, "y" = y_intron,
  #                             "α" = alpha_cor_intron, "p-value" = test_cor_intron, "DoS" = DoS_intron)
  ## Reduce the decimals to 5
  is.num <- sapply(intron_integrative_MKT, is.numeric)
  intron_integrative_MKT[is.num] <- lapply(intron_integrative_MKT[is.num], round, 5)

  inter_integrative_MKT<-select(thisGene, population, d_inter, b_inter, f_inter, y_inter,
                                alpha_cor_inter, test_cor_inter, DoS_inter)
  inter_integrative_MKT<-mutate(inter_integrative_MKT, f_inter=f_inter-y_inter)
  #inter_integrative_MKT<-rename(inter_integrative_MKT, "d" = d_inter, "b" = b_inter,
  #                               "f-y" = f_inter, "y" = y_inter,
  #                               "α" = alpha_cor_inter, "p-value" = test_cor_inter, "DoS" = DoS_inter)
  ## Reduce the decimals to 5
  is.num <- sapply(inter_integrative_MKT, is.numeric)
  inter_integrative_MKT[is.num] <- lapply(inter_integrative_MKT[is.num], round, 5)

  
  integrative_MKT_population<-as.data.frame(matrix(c(Ofold_integrative_MKT[Ofold_integrative_MKT$population==pops,], 
                                                     UTR5_integrative_MKT[UTR5_integrative_MKT$population==pops,],
                                                     UTR3_integrative_MKT[UTR3_integrative_MKT$population==pops,], 
                                                     intron_integrative_MKT[intron_integrative_MKT$population==pops,],
                                                     inter_integrative_MKT[inter_integrative_MKT$population==pops,]),ncol=8,nrow=5,byrow = T))
  integrative_MKT_population$V1<-NULL
  integrative_MKT_population<-cbind(pops = c("0-fold", "5'UTR", "3'UTR", "Intron", "Intergenic"), integrative_MKT_population)
  colnames(integrative_MKT_population)<-c(pops,"d","b","ƒ-γ","γ","α","p-value","DoS")
  integrative_MKT_population
}

#### IMPORTANT
## Here executed the function
oprint(kable(lapply(popul, integrative)))

## Integrative MKT graphic
graphic_integrative<-function(y){
  integrative_graphic<-select(y, population, d_fold0, b_fold0, f_fold0, y_fold0,
                              d_UTR5, b_UTR5, f_UTR5, y_UTR5,
                              d_UTR3, b_UTR3, f_UTR3, y_UTR3,
                              d_intron, b_intron, f_intron, y_intron,
                              d_inter, b_inter, f_inter, y_inter)
  integrative_graphic<-mutate(integrative_graphic, f_fold0=f_fold0-y_fold0, f_UTR5=f_UTR5-y_UTR5, f_UTR3=f_UTR3-y_UTR3,
                              f_intron=f_intron-y_intron, f_inter=f_inter-y_inter)
  integrative_graphic<-rename(integrative_graphic, "fy_0-fold"=f_fold0, "fy_5'UTR"=f_UTR5, "fy_3'UTR"=f_UTR3, "fy_Intron"=f_intron,
                              "fy_Intergenic"=f_inter, "d_0-fold"= d_fold0, "b_0-fold" = b_fold0, "y_0-fold"=y_fold0,
                              "d_5'UTR"=d_UTR5, "b_5'UTR"=b_UTR5, "y_5'UTR"=y_UTR5, "d_3'UTR"=d_UTR3, "b_3'UTR"=b_UTR3, "y_3'UTR"=y_UTR3,
                              "d_Intron"=d_intron, "b_Intron"=b_intron, "y_Intron"=y_intron, "d_Intergenic"=d_inter, "b_Intergenic"=b_inter,
                              "y_Intergenic"=y_inter)
  mm_integrative_graphic<-melt(integrative_graphic, id = "population")
  split_cols <- as.data.frame(str_split_fixed(mm_integrative_graphic$variable, "_", 2))
  colnames(split_cols) <- c("metric","type_of_site")
  mm_integrative_graphic<-cbind(mm_integrative_graphic, split_cols)
  mm_integrative_graphic[mm_integrative_graphic<0]<-0 ## It appears a warning but the values are changed
  mm_integrative_graphic$value[!is.finite(mm_integrative_graphic$value)] <- NaN ## Changing infinite values by NaN's
  df_withoutNA <- data.frame("population" =character(0),"variable"=character(0),"value"=numeric(0),"metric"=character(0), "type_of_site"=character(0))
  for( pop in unique(mm_integrative_graphic$population)){
    for(feature in unique(levels(mm_integrative_graphic$type_of_site))){
    #print(feature);print(pop)
    x <- mm_integrative_graphic[(mm_integrative_graphic$population == pop & mm_integrative_graphic$type_of_site == feature),]
      if(any(is.na(x))) {
        next
        }else{
        #print(x)
        df_withoutNA <- rbind(df_withoutNA,x)
    }
  }
  }
  if(nrow(df_withoutNA)==0){
    print('Empty data.frame')
  }else{
  	heigth <- 120 * nrow(y)
  	p <- WebPlot(900, heigth)
  	plot(ggplot(df_withoutNA, aes(fill=factor(metric,levels=c("d","b","fy","y")), y=value, x=type_of_site)) + geom_bar( stat="identity", position="fill") + scale_fill_manual(name="Selection\nregimes",breaks=c("d","b","fy","y"),labels=c("Strongly\ndeleterious","Weakly\ndeleterious","Old\nneutral","Recently\nneutral"),values=c("#bf0000", "#eb8c00", "#1cee15", "#068302")) + facet_grid(population~.,scale = 'free',space = 'free') + scale_y_continuous(breaks = c(seq(0,1,0.1))) + coord_flip() + labs(y = "", x = "") + theme(legend.box = "horizontal", legend.position = "top", text = element_text(size=15))) #, legend.key.size = unit(0.8, "cm")) # labels = percent_format(),
    out(p) 
  }
}

graphic_integrative(thisGene)
## Function call
#heigth <- 220 * nrow(thisGene)
#p <- WebPlot(1000, heigth)
#plot(suppressWarnings(graphic_integrative(thisGene)))
#out(p)

}}

out("</div></body></html>")

done()

}
