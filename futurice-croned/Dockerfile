# This image is used as "distribution of the image"
FROM ubuntu:bionic
MAINTAINER Futurice

RUN true \
  && apt-get -yq update \
  && apt-get -yq --no-install-suggests --no-install-recommends --force-yes install \
    ca-certificates \
    libgmp10 \
    libpq5 \
    python3 \
  && rm -rf /var/lib/apt/lists/*
EXPOSE 8000

WORKDIR /app

# Add croned-service binary
COPY ./croned-server /app

# Add en example
COPY example/script.py /app

# Add a startup command
CMD \
  ["/app/croned-server", "+RTS", "-N4", "-A32m", "-T", "-qn2", "-I60", "-RTS", "--" \
  , "python3", "/app/script.py" ]
