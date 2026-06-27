#!/usr/bin/env bash
#
# Xera/renova un certificado AUTOASINADO para nginx e recarga o contedor.
# Pensado para executarse periodicamente (timer de systemd ou cron) no servidor.
# Só renova se o certificado caduca en menos de ${RENEW_BEFORE} días.
#
# Variables (con valores por defecto):
#   DOMAIN          dominio do certificado          (noialimpapellets.publicvm.com)
#   CERT_DIR        directorio de saída             (/etc/letsencrypt/live/$DOMAIN)
#   DAYS            validez do novo certificado      (365)
#   RENEW_BEFORE    renovar se caduca en < N días    (30)
#   NGINX_CONTAINER nome do contedor de nginx        (nginx)
#
# Uso:
#   sudo ./scripts/selfsigned_cert.sh
#   sudo DOMAIN=exemplo.com DAYS=825 ./scripts/selfsigned_cert.sh
#
set -euo pipefail

DOMAIN="${DOMAIN:-noialimpapellets.publicvm.com}"
CERT_DIR="${CERT_DIR:-/etc/letsencrypt/live/${DOMAIN}}"
DAYS="${DAYS:-365}"
RENEW_BEFORE="${RENEW_BEFORE:-30}"
NGINX_CONTAINER="${NGINX_CONTAINER:-nginx}"

CRT="${CERT_DIR}/fullchain.pem"
KEY="${CERT_DIR}/privkey.pem"

mkdir -p "$CERT_DIR"

# ¿Hai que renovar? Si non existe, ou se caduca dentro de RENEW_BEFORE días.
if [ -f "$CRT" ] && openssl x509 -in "$CRT" -noout -checkend "$(( RENEW_BEFORE * 86400 ))" >/dev/null 2>&1; then
  echo "[selfsigned-cert] ${DOMAIN}: válido máis de ${RENEW_BEFORE} días; non se renova."
  exit 0
fi

echo "[selfsigned-cert] Xerando certificado autoasinado para ${DOMAIN} (${DAYS} días)..."
openssl req -x509 -nodes -newkey rsa:2048 -days "$DAYS" \
  -keyout "$KEY" -out "$CRT" \
  -subj "/CN=${DOMAIN}" \
  -addext "subjectAltName=DNS:${DOMAIN}"
chmod 600 "$KEY"

# Recargar nginx se o contedor está en marcha (recarga en quente; fallback a restart).
if docker ps --format '{{.Names}}' | grep -qx "$NGINX_CONTAINER"; then
  echo "[selfsigned-cert] Recargando nginx..."
  docker exec "$NGINX_CONTAINER" nginx -s reload || docker restart "$NGINX_CONTAINER"
fi

echo "[selfsigned-cert] Feito."
