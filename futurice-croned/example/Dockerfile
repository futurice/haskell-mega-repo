FROM ubuntu:bionic
MAINTAINER Futurice

# Install dependencies
#
# ADD DEPENDENCIES YOU NEED in place of python3
#
# but don't remove others (they are needed for croned-server)
RUN true \
  && apt-get -yq update \
  && apt-get -yq --no-install-suggests --no-install-recommends --force-yes install \
    ca-certificates \
    libgmp10 \
    libpq5 \
    python3 \
  && rm -rf /var/lib/apt/lists/*
EXPOSE 8000

# Make user
RUN useradd -m -s /bin/bash -d /app app
WORKDIR /app
RUN chown -R app:app /app
USER app

# Add croned-service binary
ADD croned-server /app

# Add script stuff, CHANGE THIS
ADD script.py /app

# Add a startup command
CMD \
  ["/app/croned-server", "+RTS", "-N4", "-A32m", "-T", "-qn2", "-I60", "-RTS", "--" \
  , "python3", "/app/script.py" ]
