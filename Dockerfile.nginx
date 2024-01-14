FROM nginx


VOLUME etc/certs
VOLUME etc/private

COPY .thusanarul/bysykkel-localhost.crt /etc/certs/bysykkel-localhost.crt
COPY .thusanarul/bysykkel-localhost.key /etc/private/bysykkel-localhost.key
COPY nginx.conf /etc/nginx/nginx.conf
