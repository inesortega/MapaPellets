#!/usr/bin/env bash
#
# Despregue completo NO SERVIDOR: arranca os servizos, fai unha carga inicial
# de datos e instala o cron de actualización. Substitúe ao antigo contedor
# `data_loader` por un cron que reutiliza o contedor de Shiny (menos memoria e
# sen consumo de CPU en reposo).
#
# Uso (dende dentro do repo clonado no servidor):
#   ./scripts/deploy.sh [ruta-ao-repo-MapaPellets]
#
# Se a imaxe de GHCR é PRIVADA, autenticarse primeiro (unha soa vez):
#   echo <TOKEN_PAT_read:packages> | docker login ghcr.io -u inesortega --password-stdin
#
set -euo pipefail

APP_DIR="${1:-$(cd "$(dirname "$0")/.." && pwd)}"
APP_DIR="$(cd "$APP_DIR" && pwd)"
cd "$APP_DIR"

echo ">> [1/5] Actualizando config (git pull)..."
git pull --ff-only || echo "   (aviso: non se puido facer git pull; continúo coa config local)"

echo ">> [2/5] Descargando a última imaxe dende GHCR..."
docker compose pull

echo ">> [3/5] Arrancando os servizos..."
docker compose up -d

echo "   Agardando a que pellets-shiny acepte conexións..."
for i in $(seq 1 30); do
  if docker compose exec -T pellets-shiny Rscript -e 'cat("ok")' >/dev/null 2>&1; then
    echo "   pellets-shiny listo."
    break
  fi
  sleep 2
  [ "$i" -eq 30 ] && echo "   (aviso: pellets-shiny aínda non responde; continúo igual)"
done

echo ">> [4/5] Carga inicial de datos (full)..."
docker compose exec -T pellets-shiny Rscript /app/update_once.R full \
  || echo "   (aviso: a carga inicial fallou; o cron volverá tentalo)"

echo ">> [5/5] Instalando o cron de actualización..."
"${APP_DIR}/scripts/setup_cron.sh" "${APP_DIR}"

echo ">> Limpando imaxes antigas sen usar..."
docker image prune -f

echo ">> Estado dos contedores:"
docker compose ps
echo ">> Despregue completo."
