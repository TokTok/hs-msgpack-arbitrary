FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-msgpack-arbitrary"]
COPY --chown=builder:users . /work/hs-msgpack-arbitrary
RUN ["stack", "install", "msgpack-arbitrary"]

FROM scratch
COPY --from=build /home/builder/.local/ /
