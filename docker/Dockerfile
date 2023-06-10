# https://rocker-project.org/images/versioned/r-ver.html
FROM rocker/r-ver:4

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
  libcurl4-openssl-dev \
  && rm -rf /var/lib/apt/lists/*

RUN install2.r devtools 

RUN R -q -e 'devtools::install_github("JayAchar/tbmstr")'

COPY ./docker/evaluate_build.R /home/evaluate_build.R

# RUN R -e 'tbmstr::ready()'

# CMD ['tail', '-f', '/dev/null']
CMD ["Rscript", "/home/evaluate_build.R"]
