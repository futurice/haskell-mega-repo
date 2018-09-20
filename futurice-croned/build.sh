set -ex
cp build/croned-server/croned-server futurice-croned/croned-server
docker build -t futurice/croned:2018-09-20 futurice-croned
rm futurice-croned/croned-server

# Also make an example
docker build -t futurice/croned-example futurice-croned/example

echo docker run --rm -p 8000:8000 futurice/croned-example
