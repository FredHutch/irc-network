FROM rocker/shiny:3.6.1
RUN apt-get update
RUN R -q -e 'install.packages(c("igraph", "visNetwork", "shinyWidgets"))'
RUN rm -rf /srv/shiny-server/
ADD app/. /srv/shiny-server
EXPOSE 8888
