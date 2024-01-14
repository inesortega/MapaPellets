### Outra Máis - Monitorización cidadá das praias de Galicia

Aplicación web para facer seguemento da situación das praias galegas. A app está dispoñible na seguinte [web](https://pellets.10cal.ovh/), ademáis de na web de [Noia Limpa](!https://www.noialimpa.org/informacion-pellets-plastico-galicia).



##### Shiny

O ficheiro `app.R` contén o código para a visualización da app web de shiny. A app carga os datos a intervalos regulares de 5min a partir do ficheiro `data/praias.csv`, e mostra a información no mapa. 

##### Preprocesamento de datos

O ficheiro `data_loader.R` é o encargado da obtención e pre-procesamento dos datos. Carga os datos da folla de Excel online, e dependendo do parámetro de actualización `update_all` ou `update_all_dataset` actualiza a información dispoñible. 

- Se ambos parámetros son `FALSE` só actualiza o dataset coa nova información dispoñible en Excel. 
- Se `update_all == TRUE` actualiza todos os datos dese mesmo día. Deste xeito, podemos incluir na app web novos cambios introducidos manualmente na folla Excel (por exemplo, cambios nas coordenadas). 
- Se `update_all_dataset == TRUE` entón actualízase o dataset completo, re-procesando todos os datos dispoñibles. 

Unha vez seleccionados os datos que é necesario actualizar, empregamos a librería `tidygeocode` para preprocesar a xeolocalización aportada polo usuario. Se os datos non son válidos, entón tentamos obter a xeolocalización a partires da información do campo `Praia.Concello`, facendo un proceso de `reverse geocoding`. 

##### Actualización de datos

O ficherio `update_historical.R` executa un proceso en bucle para executar as tarefas de actualización de datos de forma periódica: 

- Obtención de novos datos: cada 5 min
- Actualización dos datos diarios: cada hora
- Actualización do dataset completo: diariamente, as 3AM. 

#### Despliegue con docker

Unha vez configuradas as credenciais, compilar a imaxe: 

```
docker build -t harbor.gradiant.org/iortega-pellets/pellets:latest .
docker push harbor.gradiant.org/iortega-pellets/pellets:latest
```

No servidor lanzar os seguintes comandos para actualizar a imaxe e desplegar a nova versión: 
```
sudo docker pull harbor.gradiant.org/iortega-pellets/pellets:latest
sudo docker container rm -f pellets-shiny
sudo docker-compose up -d
```
