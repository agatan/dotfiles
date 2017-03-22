if exists ccache; then
    if exists clang && exists clang++; then
        export CC=ccache-clang
        export CXX=ccache-clang++
    else
        export CC=ccache-gcc
        export CXX=ccache-g++
    fi
fi
