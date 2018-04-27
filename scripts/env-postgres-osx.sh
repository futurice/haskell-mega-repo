POSTGRES_BIN=/Applications/Postgres.app/Contents/Versions/9.5/bin
if [ -d $POSTGRES_BIN ]; then
  export DATABASE_URL=postgres:///`whoami`
  export PATH=$PATH:$POSTGRES_BIN
fi
