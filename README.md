# Outra Máis - Monitorización cidadá das praias de Galicia

Aplicación web para facer seguemento da situación das praias galegas. A app está dispoñible na seguinte [web](https://pellets.10cal.ovh/), ademáis de na web de [Noia Limpa](!https://www.noialimpa.org/informacion-pellets-plastico-galicia).



## Shiny

O ficheiro `app.R` contén o código para a visualización da app web de shiny. A app carga os datos a intervalos regulares de 5min a partir do ficheiro `data/praias.csv`, e mostra a información no mapa. 

### Preprocesamento de datos

O ficheiro `data_loader.R` é o encargado da obtención e pre-procesamento dos datos. Carga os datos da folla de Excel online, e dependendo do parámetro de actualización `update_all` ou `update_all_dataset` actualiza a información dispoñible. 

- Se ambos parámetros son `FALSE` só actualiza o dataset coa nova información dispoñible en Excel. 
- Se `update_all == TRUE` actualiza todos os datos dese mesmo día. Deste xeito, podemos incluir na app web novos cambios introducidos manualmente na folla Excel (por exemplo, cambios nas coordenadas). 
- Se `update_all_dataset == TRUE` entón actualízase o dataset completo, re-procesando todos os datos dispoñibles. 

Unha vez seleccionados os datos que é necesario actualizar, empregamos a librería `tidygeocode` para preprocesar a xeolocalización aportada polo usuario. Se os datos non son válidos, entón tentamos obter a xeolocalización a partires da información do campo `Praia.Concello`, facendo un proceso de `reverse geocoding`. 

### Actualización de datos

O ficherio `update_historical.R` executa un proceso en bucle para executar as tarefas de actualización de datos de forma periódica: 

- Obtención de novos datos: cada 5 min
- Actualización dos datos diarios: cada hora
- Actualización do dataset completo: diariamente, as 3AM. 

## Despregue con docker

A imaxe constrúese e publícase automaticamente en **GitHub Container Registry (GHCR)** mediante GitHub Actions. A imaxe resultante é `ghcr.io/inesortega/mapapellets`.

### CI/CD (automático)

Cada `push` a `master` (ou unha etiqueta `v*`) dispara o workflow [.github/workflows/docker-publish.yml](.github/workflows/docker-publish.yml), que:

1. Constrúe a imaxe para `linux/amd64` nun runner amd64 nativo (sen emulación).
2. Publícaa en `ghcr.io/inesortega/mapapellets` coas etiquetas `latest`, `sha-<commit>` e, no caso de tags, a versión semántica.

> **Visibilidade do paquete:** a primeira vez créase como *privado*. Para que o servidor poida descargalo sen autenticarse, faino público en GitHub (*Packages → mapapellets → Package settings → Change visibility → Public*). Se o mantés privado, autentícate no servidor cun *Personal Access Token* con permiso `read:packages`.

### Despregue no servidor (pull dende GHCR)

No servidor, dentro do repo clonado:

```
# (só se o paquete é privado) autenticarse en GHCR
echo <PAT_read:packages> | docker login ghcr.io -u inesortega --password-stdin

# despregar a última versión
./scripts/deploy_remote.sh

# ... ou manualmente
git pull
docker compose pull
docker compose up -d
```

O servidor segue precisando o directorio `.secrets/` (coa clave de Google) e as variables de entorno definidas en `docker-compose.yml`; eses segredos **non** van dentro da imaxe.

### Compilación e despregue manual (fallback offline)

Se precisas construír localmente e transferir un `.tar` (por exemplo, sen acceso a GHCR):

```
./scripts/export_image.sh pellets-shiny.tar.gz
scp pellets-shiny.tar.gz user@SERVER:/home/user/

# no servidor
./scripts/import_and_deploy_remote.sh /home/user/pellets-shiny.tar.gz /home/user/MapaPellets
```

> En Apple Silicon (Mac M-series) o build local require Docker Desktop co *containerd image store* desactivado, xa que o destino é `linux/amd64` e a extracción de capas amd64 falla en caso contrario.