FROM rocker/shiny:3.6.1
RUN apt-get update
RUN R -q -e 'install.packages(c("igraph", "visNetwork", "shinyWidgets"))'
RUN useradd -u 5555 -m -d /home/shiny -c "shiny user" shiny
ADD app/. /home/shiny/
RUN chown -R shiny:shiny /home/shiny 
WORKDIR /home/shiny
USER shiny
EXPOSE 7777
CMD Rscript start.R
