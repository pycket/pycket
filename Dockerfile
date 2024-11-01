# Let's run things on ubuntu 24.04
FROM ubuntu:24.04

# Copy your compiled Pycket directory into the container
# The whole setup assumes the /racket is within pycket/
# Make sure to run make setup-local-racket before this
COPY . /opt/pycket

# Ensure the Pycket binaries are executable

RUN chmod +x /opt/pycket/pycket-c-linklets
RUN chmod +x /opt/pycket/pycket-c

# Set the environment variables
ENV PATH="/opt/pycket:/opt/pycket/racket/bin:${PATH}"
ENV PLTHOME="/opt/pycket"
ENV PLTCOLLECTS="/opt/pycket/racket/collects"
ENV PLTEXECFILE="/opt/pycket/racket/bin/racket"
ENV PLTUSERHOME="/opt/pycket"

WORKDIR /opt/pycket

# Install pycket-lang for old pycket
RUN /opt/pycket/racket/bin/raco pkg install --no-docs -t dir ./pycket-lang
RUN /opt/pycket/racket/bin/raco pkg update --no-docs --link ./pycket-lang
