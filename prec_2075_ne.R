library(rgdal)
library(ncdf4)
library(raster)
library(ggplot2)
library(rasterVis)
library(RColorBrewer)
library(devtools)

# Para avaliar as mudanças climáticas sobre o Nordeste do Brasil usei dados do CORDEX,
# regionalizados a partir de experimentos do
# CMIP, pois possuem resolução mais adequada para um estudo regional.
# Fonte dos dados do CORDEX: (https://esgf-index1.ceda.ac.uk/search/esgf-ceda/)

# Primeiramente, baixei um shapefile com os limites do Nordeste
# Fonte do shp: (https://www.ipea.gov.br/ipeageo/malhas.html)

shp_ne = readOGR("shp/GEOFT_REGIAO_POLITICO_ADM_2013/GEOFT_REGIAO_POLITICO_ADM_2013.shp")
shp_ne = subset(shp_ne, NM_REGIAO=="NORDESTE")

# Após o download dos dados, juntei os arquivos e selecionei os anos desejados com o CDO
# Juntar arquivos: cdo mergetime; Selecionar anos: cdo selyear
# Carregando dados de precipitação como raster
pr_hist = brick("cordex/hist_sam/pr_SAM_HadGEM2-ES_hist_1960-2005.nc")

## Precipitação - experimento Historical
pr_hist = pr_hist*86400 # Transforma para mm/dia

shp_ne_wgs84 = spTransform(shp_ne, crs(pr_hist))
pr_hist_ne = crop(pr_hist, extent(shp_ne_wgs84))
pr_hist_ne = mask(pr_hist_ne, shp_ne_wgs84)

# Calculando média temporal para toda a região (clima histórico)
pr_hist_ne_mean = mean(pr_hist_ne, na.rm = TRUE)
# Checando resultado
plot(pr_hist_ne_mean)
plot(shp_ne_wgs84, add=TRUE)


## Precipitação - experimento RCP 8.5
# Carregando dados para o ano de 2075
pr_rcp85 = brick("cordex/rcp85_sam/pr_SAM_HadGEM2-ES_rcp85_2075.nc")

pr_rcp85 = pr_rcp85*86400 # transforma para mm/dia

pr_rcp85_ne = crop(pr_rcp85, extent(shp_ne_wgs84))
pr_rcp85_ne = mask(pr_rcp85_ne, shp_ne_wgs84)

# Calculando média temporal para toda a região (cenário RCP 8.5)
pr_rcp85_ne_mean = mean(pr_rcp85_ne, na.rm = TRUE)

plot(pr_rcp85_ne_mean)
plot(shp_ne_wgs84, add=TRUE)

# Calculando diferença entre o cenário futuro e o clima histórico
# No intuito de observar a distribuição espacial das mudanças
pr_diff_2075_hist = pr_rcp85_ne_mean - pr_hist_ne_mean

# Plotando resultado da diferença
devtools::source_gist('306e4b7e69c87b1826db')
png("mudanca_pr_ne_2075.png")
p <- print(levelplot(pr_diff_2075_hist) + layer(sp.polygons(shp_ne_wgs84)))
diverge0(p, ramp='BrBG')
dev.off()
#########

# A simples diferença espacial não permite avaliar qual época do ano sofre maior impacto
# Portanto, usei dados diários para comparar a série temporal de 2075 com a média de 1961 a 2005


##### Precipitação diária - experimento Historical

pr_hist_day = brick("cordex/hist_sam/pr_SAM_HadGEM2-ES_hist_1961-2005_day.nc")

pr_hist_day_ne = crop(pr_hist_day, extent(shp_ne_wgs84))
pr_hist_day_ne = mask(pr_hist_day_ne, shp_ne_wgs84)

# Calculando ano climatológico de 1961 a 2005
indices = format(as.Date(names(pr_hist_day_ne), format = "X%Y.%m.%d"), format = "%j")
indices = as.numeric(indices)
pr_hist_day_ne_clim = stackApply(pr_hist_day_ne, indices, fun = mean)
names(pr_hist_day_ne_clim) <- month.abb

pr_hist_day_ne_clim = pr_hist_day_ne_clim*86400 # transforma para mm/dia

# Média espacial para cada dia (para obter uma série temporal)
pr_hist_day_ne_tot_mean = cellStats(pr_hist_day_ne_clim, 'mean')


#### Precipitação Diária - experimento RCP8.5
# Carregando dados para o ano de 2075
pr_rcp85_day = brick("cordex/rcp85_sam/pr_SAM_HadGEM2-ES_rcp85_2075_day.nc")

pr_rcp85_day = pr_rcp85_day*86400 # transforma para mm/dia

pr_rcp85_day_ne = crop(pr_rcp85_day, extent(shp_ne_wgs84))
pr_rcp85_day_ne = mask(pr_rcp85_day_ne, shp_ne_wgs84)

# Média espacial para cada dia
pr_rcp85_ne_timeseries = cellStats(pr_rcp85_day_ne, 'mean')

##

# Calculando diferença entre as duas séries temporais (RCP 8.5 - Historical)
pr_diff_anual_day = pr_rcp85_ne_timeseries - pr_hist_day_ne_tot_mean

# Gerando vetor de datas para plot
dates <- seq(as.Date("01-Jan", format = "%d-%b"),by = "days", length = length(pr_diff_anual_day))

# Plotando e salvando resultado da diferença
png("anom_pr_ne_2075.png")
plot(dates, pr_diff_anual_day, type = "l", xaxs = "i", ylab = "Precipitação (mm/dia)", xlab = "Meses", col = 73)
abline(h=0, lty = 4, col = "darkred")
dev.off()
