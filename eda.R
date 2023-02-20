library(glue)
library(dplyr)
library(ggplot2)
library(cowplot)
library(hrbrthemes)
library(ggthemes)
library(DescTools)
df<-read.csv('dados.csv')
attach(df)
tail(df,10)
names(df)
unique(df$UF)
unique(df$Sexo)
unique(df$Cor)
sort(unique(df$Anos.de.Estudo))
#age
glue("from {min(df$Idade)} to {max(df$Idade)} years")
glue("from {min(df$Renda)} to {max(df$Renda)} reais")

uf = c('Rondônia', 'Acre', 'Amazonas', 'Roraima', 'Pará', 'Amapá',
       'Tocantins', 'Maranhão', 'Piauí', 'Ceará', 'Rio Grande do Norte', 'Paraíba',
       'Pernambuco', 'Alagoas', 'Sergipe', 'Bahia', 'Minas Gerais', 'Espírito Santo',
       'Rio de Janeiro', 'São Paulo', 'Paraná', 'Santa Catarina', 'Rio Grande do Sul', 'Mato Grosso do Sul',
       'Mato Grosso', 'Goiás', 'Distrito Federal')
prc_uf<-data.frame(cbind("Frequency"=table(df$UF),"precent"=prop.table(table(df$UF))*100))
row.names(prc_uf)=uf
prc_uf

prect_sexo=data.frame(cbind("frequency"=table(df$Sexo),"percent"=prop.table(table(df$Sexo))*100))
row.names(prect_sexo)=c('male',"female")
prect_sexo

prect_cor=data.frame(cbind("frequency"=table(df$Cor),"Percent"=prop.table(table(df$Cor))*100))
row.names(prect_cor)=c('Indigena',"Branca","preta","Amarela","Parda")
prect_cor

options(repr.plot.width=14, repr.plot.height=6)
a<-ggplot(df, aes(x=Altura))+
  geom_histogram(bins=50, col='black', fill='yellow', alpha=.7, size=1.2)+
  theme_economist()+
  ggtitle("Altura Histogram")
b<-ggplot(df, aes(x=Altura, y=..density..))+
  geom_density(col='black', fill='yellow', alpha=.7, size=1.2)+
  theme_economist()+
  ggtitle("Altura Density")


plot_grid(a,b,nrow=1,ncol=2)

ggplot(df, aes(x=Idade))+
  geom_histogram(bins=50, col='black', fill='#00BFFF', alpha=.7, size=1.1)+
  theme_economist()+
  ggtitle("Idade Histogram")+
  ylab("Frequency")+
  theme(plot.title = element_text(size=21, hjust=.5),
        axis.title.y = element_text(size=21, vjust=2),
        axis.title.x = element_text(size=21, vjust=-1),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20)
  )






