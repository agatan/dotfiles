set -eu

goclis=(
    github.com/mattn/memo
)

for gpkg in $goclis
do
    go get $gpkg
done
