#!/bin/bash
cp dist/build/libpandoc.dll/libpandoc.dll /usr/local/lib/libpandoc.so \
    && cp pandoc.h /usr/local/include \
    && ldconfig /usr/local/lib
