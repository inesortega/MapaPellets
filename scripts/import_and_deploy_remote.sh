#!/usr/bin/env bash
#
# Carga unha imaxe Docker dende un .tar.gz e desprega con docker compose.
# Pensado para executarse NO SERVIDOR Ubuntu.
#
# Uso:
#   ./scripts/import_and_deploy_remote.sh <ruta-ao-tar.gz> <ruta-ao-repo-MapaPellets>
#
# Exemplo:
#   ./scripts/import_and_deploy_remote.sh /home/user/pellets-shiny.tar.gz /home/user/MapaPellets
#
set -euo pipefail

TARBALL="${1:?Uso: import_and_deploy_remote.sh <ruta-tar.gz> <ruta-MapaPellets>}"
APP_DIR="${2:?Uso: import_and_deploy_remote.sh <ruta-tar.gz> <ruta-MapaPellets>}"

echo ">> Cargando a imaxe dende ${TARBALL}..."
gunzip -c "${TARBALL}" | docker load

echo ">> Desprega en ${APP_DIR}..."
cd "${APP_DIR}"
# A imaxe xa está cargada localmente, así que compose NON a reconstrúe.
docker compose up -d

echo ">> Instalando o cron de actualización..."
"${APP_DIR}/scripts/setup_cron.sh" "${APP_DIR}"

echo ">> Estado dos contedores:"
docker compose ps
