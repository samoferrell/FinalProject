FROM rocker/r-ver:3.5.0

RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev

RUN R -e "install.packages('plumber')"
RUN R -e "install.packages('DescTools')"
RUN R -e "install.packages('tidyverse')"

COPY myAPI.R myAPI.R
EXPOSE 8000

ENTRYPOINT ["R", "-e", \
"pr <- plumber::plumb('myAPI.R'); pr$run(host=;0.0.0.0',
port=8000)"]

