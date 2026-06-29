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

A actualización **non corre nun contedor aparte**. Para aforrar memoria e CPU no
servidor, un `cron` no host lanza o script de un só disparo `update_once.R`
dentro do contedor `pellets-shiny` xa en execución (`docker compose exec`). O
proceso de R arranca, actualiza `data/praias.csv` e sae, liberando a memoria;
non queda ningún proceso residente nin se consume CPU en reposo. A app recolle o
CSV actualizado soa, porque o relé cada ~5 min (`invalidateLater`).

`update_once.R` recibe o modo como argumento e chama unha soa vez a `get_data()`:

- `fast`  → só datos novos (`get_data()`) — **cada 5 min**
- `today` → datos do día de hoxe (`get_data(update_all = TRUE)`) — **cada hora**
- `full`  → dataset completo (`get_data(update_all_dataset = TRUE)`) — **ás 03:00**

O cron instálao [scripts/setup_cron.sh](scripts/setup_cron.sh) (idempotente, con
`flock` para evitar solapamentos). O despregue completo
([scripts/deploy.sh](scripts/deploy.sh)) xa o configura automaticamente.

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

# despregar a última versión (arranca servizos + carga inicial + instala cron)
./scripts/deploy.sh

# ... ou manualmente
git pull
docker compose pull
docker compose up -d
./scripts/setup_cron.sh        # instala/actualiza o cron de actualización
```

[scripts/deploy.sh](scripts/deploy.sh) fai todo o despregue: `git pull`,
`docker compose pull`, `up -d`, unha carga inicial de datos e a instalación do
cron de actualización ([scripts/setup_cron.sh](scripts/setup_cron.sh)).

O servidor segue precisando o directorio `.secrets/` (coa clave de Google) e as variables de entorno definidas en `docker-compose.yml`; eses segredos **non** van dentro da imaxe.

### Certificados TLS (Let's Encrypt)

O servizo `nginx` termina TLS para `noialimpapellets.publicvm.com`. Os certificados xéranse con **certbot** no host e móntanse no contedor a través de `/etc/letsencrypt`.

**Requisitos previos:**

- O dominio `noialimpapellets.publicvm.com` apunta (rexistro DNS A/AAAA) á IP do servidor.
- Os portos 80 e 443 están abertos no firewall.
- certbot instalado: `sudo apt update && sudo apt install -y certbot`

**Primeira emisión (bootstrap):**

nginx non pode arrancar co bloque HTTPS antes de que exista o certificado, así que a primeira vez emítese co plugin *standalone* (con nginx parado un intre para liberar o porto 80):

```bash
docker compose stop nginx
sudo certbot certonly --standalone \
  -d noialimpapellets.publicvm.com \
  --agree-tos -m <o-teu-email> --no-eff-email
docker compose up -d        # nginx xa atopa o certificado e arranca
```

Os ficheiros quedan en `/etc/letsencrypt/live/noialimpapellets.publicvm.com/` (`fullchain.pem` e `privkey.pem`), que é o que referencia [nginx/conf.d/nginx.conf](nginx/conf.d/nginx.conf).

**Renovación automática:**

certbot instala un *timer* de systemd que renova só (cando faltan menos de 30 días). Como a renovación *standalone* precisa o porto 80, engádense ganchos para parar e volver arrancar nginx (uns segundos de corte cada ~60 días):

```bash
sudo mkdir -p /etc/letsencrypt/renewal-hooks/pre /etc/letsencrypt/renewal-hooks/post
printf '#!/bin/sh\ndocker stop nginx\n'  | sudo tee /etc/letsencrypt/renewal-hooks/pre/stop-nginx.sh
printf '#!/bin/sh\ndocker start nginx\n' | sudo tee /etc/letsencrypt/renewal-hooks/post/start-nginx.sh
sudo chmod +x /etc/letsencrypt/renewal-hooks/pre/stop-nginx.sh /etc/letsencrypt/renewal-hooks/post/start-nginx.sh

# probar (executa os ganchos, así que nginx pararase e arrancará unha vez)
sudo certbot renew --dry-run
```

> **Sen cortes (alternativa webroot):** como nginx serve `/.well-known/acme-challenge/` dende `/srv/www/acme`, podes emitir/renovar por webroot sen parar nada:
> `sudo certbot certonly --webroot -w /srv/www/acme -d noialimpapellets.publicvm.com`.
> Require que nginx xa estea en marcha (polo que segue precisando o bootstrap *standalone* a primeira vez, ou un certificado autoasinado temporal).

### Alternativa: certificado autoasinado con autorrenovación

Se prefires non depender de Let's Encrypt (p.ex. mentres o porto 80 non é accesible publicamente), podes usar un certificado **autoasinado** que se renova só. Os navegadores mostrarán un aviso de "sitio non seguro" — é o comportamento esperado nun autoasinado.

O script [scripts/selfsigned_cert.sh](scripts/selfsigned_cert.sh) xera o certificado (en `/etc/letsencrypt/live/<dominio>/`, onde o espera nginx) e recarga o contedor. Só renova se caduca en menos de 30 días, polo que é idempotente.

**Primeira xeración:**

```bash
sudo ./scripts/selfsigned_cert.sh
docker compose up -d
```

**Autorrenovación (timer de systemd, comproba a diario):**

Crea os dous ficheiros (axusta a ruta do script ao teu repo, p.ex. `/home/MapaPellets`):

```ini
# /etc/systemd/system/selfsigned-cert.service
[Unit]
Description=Renovar certificado autoasinado de nginx

[Service]
Type=oneshot
ExecStart=/home/MapaPellets/scripts/selfsigned_cert.sh
```

```ini
# /etc/systemd/system/selfsigned-cert.timer
[Unit]
Description=Comproba/renova o certificado autoasinado a diario

[Timer]
OnCalendar=daily
Persistent=true

[Install]
WantedBy=timers.target
```

Actívao e compróbao:

```bash
sudo systemctl daemon-reload
sudo systemctl enable --now selfsigned-cert.timer
systemctl list-timers selfsigned-cert.timer
```

> Alternativa con cron, en `/etc/cron.d/selfsigned-cert`:
> `0 3 * * * root /home/MapaPellets/scripts/selfsigned_cert.sh >> /var/log/selfsigned-cert.log 2>&1`

### Compilación e despregue manual (fallback offline)

Se precisas construír localmente e transferir un `.tar` (por exemplo, sen acceso a GHCR):

```
./scripts/export_image.sh pellets-shiny.tar.gz
scp pellets-shiny.tar.gz user@SERVER:/home/user/

# no servidor
./scripts/import_and_deploy_remote.sh /home/user/pellets-shiny.tar.gz /home/user/MapaPellets
```

> En Apple Silicon (Mac M-series) o build local require Docker Desktop co *containerd image store* desactivado, xa que o destino é `linux/amd64` e a extracción de capas amd64 falla en caso contrario.