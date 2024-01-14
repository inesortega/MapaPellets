### Outra Máis - Monitorización cidadá das praias de Galicia

Aplicación web para facer seguemento da situación das praias galegas. 


##### Despliegue con docker

Unha vez configuradas as credenciais, compilar a imaxe: 

```
docker build -t pellets .
docker image tag pellets harbor.gradiant.org/iortega-pellets/pellets:latest
docker push harbor.gradiant.org/iortega-pellets/pellets:latest
```

No servidor lanzar os seguintes comandos para actualizar a imaxe e desplegar a nova versión: 
```
docker pull harbor.gradiant.org/iortega-pellets/pellets:latest
sudo docker container rm -f pellets-shiny
docker-compose up -d
```
