FROM intel/oneapi-hpckit:latest
ADD .  /app
WORKDIR /app
CMD ./build.sh
