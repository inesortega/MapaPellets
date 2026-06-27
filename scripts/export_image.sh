#!/usr/bin/env bash
#
# Constrúe a imaxe Docker para a arquitectura de destino (por defecto amd64)
# e expórtaa nun .tar.gz listo para transferir ao servidor.
#
# Uso:
#   ./scripts/export_image.sh [saida.tar.gz]
#
# Variables opcionais:
#   IMAGE=pellets-shiny:latest   # nome:tag da imaxe
#   PLATFORM=linux/amd64         # arquitectura do servidor (linux/amd64 ou linux/arm64)
#
set -euo pipefail

OUTPUT="${1:-pellets-shiny.tar.gz}"
# Mesma etiqueta que usa docker-compose.yml, para que o tar cargado coincida.
IMAGE="${IMAGE:-ghcr.io/inesortega/mapapellets:latest}"
PLATFORM="${PLATFORM:-linux/amd64}"
# Usar o builder integrado de Docker Desktop (driver docker), que constrúe no
# propio daemon con emulación. Evita arrancar un contedor moby/buildkit aparte
# (driver docker-container), que pode fallar con "buildkit ... exec format error".
BUILDER="${BUILDER:-desktop-linux}"

# Situarse na raíz do repositorio (este script vive en scripts/)
cd "$(dirname "$0")/.."

echo ">> Construíndo ${IMAGE} para ${PLATFORM} (cross-build, builder=${BUILDER})..."
docker buildx build \
  --builder "${BUILDER}" \
  --platform "${PLATFORM}" \
  -t "${IMAGE}" \
  --load \
  .

echo ">> Exportando a ${OUTPUT}..."
docker save "${IMAGE}" | gzip > "${OUTPUT}"

echo ">> Feito: ${OUTPUT}"
ls -lh "${OUTPUT}"
docker image inspect "${IMAGE}" --format '   arquitectura da imaxe: {{.Architecture}}'
