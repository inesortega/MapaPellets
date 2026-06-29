#!/usr/bin/env bash
#
# Instala (ou reinstala) as entradas de cron que actualizan os datos da app
# lanzando `update_once.R` dentro do contedor pellets-shiny xa en execución.
#
# É idempotente: substitúe o seu propio bloque no crontab en cada execución,
# polo que se pode chamar tantas veces como se queira sen duplicar entradas.
#
# Uso:
#   ./scripts/setup_cron.sh [ruta-ao-repo-MapaPellets]
#
set -euo pipefail

APP_DIR="${1:-$(cd "$(dirname "$0")/.." && pwd)}"
APP_DIR="$(cd "$APP_DIR" && pwd)"

MARKER_BEGIN="# >>> mapapellets (xestionado por setup_cron.sh) >>>"
MARKER_END="# <<< mapapellets <<<"
LOCK="/tmp/mapapellets-update.lock"
LOG="${APP_DIR}/data/update.log"

# `flock -n` evita que dúas actualizacións se solapen (a longa de cada hora /
# 03:00 pode coincidir cunha rápida). `docker compose exec -T` reutiliza o
# contedor existente; o proceso de R sae ao rematar e libera a memoria.
EXEC="cd ${APP_DIR} && flock -n ${LOCK} docker compose exec -T pellets-shiny Rscript /app/update_once.R"

BLOCK=$(cat <<EOF
${MARKER_BEGIN}
*/5 * * * * ${EXEC} fast  >> ${LOG} 2>&1
0   * * * * ${EXEC} today >> ${LOG} 2>&1
0   3 * * * ${EXEC} full  >> ${LOG} 2>&1
${MARKER_END}
EOF
)

# Crontab actual sen o noso bloque (se existe), e engadimos o bloque novo.
CURRENT="$(crontab -l 2>/dev/null || true)"
CLEANED="$(printf '%s\n' "$CURRENT" | sed "/${MARKER_BEGIN}/,/${MARKER_END}/d")"

{
  printf '%s\n' "$CLEANED" | sed '/^$/N;/^\n$/D'  # colapsa liñas baleiras
  printf '%s\n' "$BLOCK"
} | crontab -

echo ">> Cron instalado. Entradas activas para mapapellets:"
crontab -l | sed -n "/${MARKER_BEGIN}/,/${MARKER_END}/p"
echo ">> Logs de actualización: ${LOG}"
