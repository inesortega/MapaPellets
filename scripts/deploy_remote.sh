#!/usr/bin/env bash
#
# Despregue NO SERVIDOR: descarga a última imaxe dende GHCR e reinicia os
# servizos con docker compose.
#
# Uso (dende dentro do repo clonado no servidor):
#   ./scripts/deploy_remote.sh [ruta-ao-repo-MapaPellets]
#
# Se o paquete de GHCR é PRIVADO, autenticarse primeiro (unha soa vez):
#   echo <TOKEN_PAT_read:packages> | docker login ghcr.io -u inesortega --password-stdin
#
set -euo pipefail

APP_DIR="${1:-$(cd "$(dirname "$0")/.." && pwd)}"
cd "$APP_DIR"

echo ">> Actualizando docker-compose.yml e config (git pull)..."
git pull --ff-only || echo "   (aviso: non se puido facer git pull; continúo coa config local)"

echo ">> Descargando a última imaxe dende GHCR..."
docker compose pull

echo ">> Reiniciando os servizos..."
docker compose up -d

echo ">> Limpando imaxes antigas sen usar..."
docker image prune -f

echo ">> Estado dos contedores:"
docker compose ps
