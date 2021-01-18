library(rgdal)
library(ncdf4)
library(raster)

# Para avaliar as mudanças climáticas sobre o Nordeste do Brasil usei dados do CORDEX,
# regionalizados a partir de experimentos do
# CMIP, pois possuem resolução mais adequada para um estudo regional.
# Fonte dos dados do CORDEX: (https://esgf-index1.ceda.ac.uk/search/esgf-ceda/)

# Primeiramente, baixei um shapefile com os limites do Nordeste
# Fonte do shp: (https://www.ipea.gov.br/ipeageo/malhas.html)

shp_ne = readOGR("GEOFT_REGIAO_POLITICO_ADM_2013/GEOFT_REGIAO_POLITICO_ADM_2013.shp")
shp_ne = subset(shp_ne, NM_REGIAO=="NORDESTE")

# Após o download dos dados, juntei os arquivos e selecionei os anos desejados com o CDO
# Juntar arquivos: cdo mergetime; Selecionar anos: cdo selyear
# Carregando dados de temperatura como raster
tas_hist = brick("cordex/hist_sam/tas_SAM_HadGEM2-ES_hist_1960-2005.nc")

## Temperatura - experimento historical
tas_hist = tas_hist - 273.15 # transforma para Celsius

shp_ne_wgs84 = spTransform(shp_ne, crs(tas_hist))
tas_hist_ne = crop(tas_hist, extent(shp_ne_wgs84))
tas_hist_ne = mask(tas_hist_ne, shp_ne_wgs84)

# Calculando média temporal para toda a região (clima histórico)
tas_hist_ne_mean = mean(tas_hist_ne, na.rm = TRUE)
# Checando resultado
plot(tas_hist_ne_mean)
plot(shp_ne_wgs84, add=TRUE)


## Temperatura - experimento RCP 8.5
# Carregando dados para o ano de 2075
tas_rcp85 = brick("cordex/rcp85_sam/tas_SAM_HadGEM2-ES_rcp85_2075.nc")

tas_rcp85 = tas_rcp85 - 273.15 # transforma para mm/dia

tas_rcp85_ne = crop(tas_rcp85, extent(shp_ne_wgs84))
tas_rcp85_ne = mask(tas_rcp85_ne, shp_ne_wgs84)

# Calculando média temporal para toda a região (cenário RCP 8.5)
tas_rcp85_ne_mean = mean(tas_rcp85_ne, na.rm = TRUE)

plot(tas_rcp85_ne_mean)
plot(shp_ne_wgs84, add=TRUE)

# Calculando diferença entre o cenário futuro e o clima histórico
# No intuito de observar a distribuição espacial das mudanças
tas_diff_2075_hist = tas_rcp85_ne_mean - tas_hist_ne_mean

# Plotando resultado da diferença
devtools::source_gist('306e4b7e69c87b1826db')
png("mudanca_tas_ne_2075.png")
orrd <- colorRampPalette(brewer.pal(9, "OrRd"))
print(levelplot(tas_diff_2075_hist, col.regions = orrd) + layer(sp.polygons(shp_ne_wgs84)))
dev.off()
#########

# A simples diferença espacial não permite avaliar qual época do ano sofre maior impacto
# Portanto, usei dados diários para comparar a série temporal de 2075 com a média de 1961 a 2005

##### Precipitação diária - experimento Historical

tas_hist_day = brick("cordex/hist_sam/tas_SAM_HadGEM2-ES_hist_1961-2005_day.nc")

tas_hist_day_ne = crop(tas_hist_day, extent(shp_ne_wgs84))
tas_hist_day_ne = mask(tas_hist_day_ne, shp_ne_wgs84)

# Calculando ano climatológico de 1961 a 2005
indices = format(as.Date(names(tas_hist_day_ne), format = "X%Y.%m.%d"), format = "%j")
indices = as.numeric(indices)
tas_hist_day_ne_clim = stackApply(tas_hist_day_ne, indices, fun = mean)
names(tas_hist_day_ne_clim) <- month.abb

tas_hist_day_ne_clim = tas_hist_day_ne_clim - 273.15 # transforma para °C

# Média espacial para cada dia (para obter uma série temporal)
tas_hist_day_ne_tot_mean = cellStats(tas_hist_day_ne_clim, 'mean')


#### Precipitação Diária - experimento RCP8.5
# Carregando dados para o ano de 2075
tas_rcp85_day = brick("cordex/rcp85_sam/tas_SAM_HadGEM2-ES_rcp85_2075_day.nc")

tas_rcp85_day = tas_rcp85_day - 273.15 # transforma para °C

tas_rcp85_day_ne = crop(tas_rcp85_day, extent(shp_ne_wgs84))
tas_rcp85_day_ne = mask(tas_rcp85_day_ne, shp_ne_wgs84)

# Média espacial para cada dia
tas_rcp85_ne_timeseries = cellStats(tas_rcp85_day_ne, 'mean')

##

# Calculando diferença entre as duas séries temporais (RCP 8.5 - Historical)
tas_diff_anual_day = tas_rcp85_ne_timeseries - tas_hist_day_ne_tot_mean

# Gerando vetor de datas para plot
dates <- seq(as.Date("01-Jan", format = "%d-%b"),by = "days", length = length(tas_diff_anual_day))

# Plotando e salvando resultado da diferença
png("anom_tas_ne_2075.png")
plot(dates, tas_diff_anual_day, type = "l", xaxs = "i", ylab = "Temperatura (°C)", xlab = "Meses", col = 73)
dev.off()
