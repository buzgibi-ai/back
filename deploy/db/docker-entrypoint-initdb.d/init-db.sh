#!/bin/bash

# Immediately exits if any error occurs during the script
# execution. If not set, an error could occur and the
# script would continue its execution.
set -o errexit


# Creating an array that defines the environment variables
# that must be set. This can be consumed later via arrray
# variable expansion ${REQUIRED_ENV_VARS[@]}.
readonly REQUIRED_ENV_VARS=(
  "DB_USER"
  "DB_PASSWORD"
  "DB_DATABASE"
  "POSTGRES_USER"
  "POSTGRES_PASSWORD"
  )


# Main execution:
# - verifies if all environment variables are set
# - runs the SQL code to create user and database
main() {
  check_env_vars_set
  init_user_and_db
  echo "user $DB_USER with passowrd $DB_PASSWORD has been created"
  echo "database $DB_DATABASE has been created"
}


# Checks if all of the required environment
# variables are set. If one of them isn't,
# echoes a text explaining which one isn't
# and the name of the ones that need to be
check_env_vars_set() {
  for required_env_var in ${REQUIRED_ENV_VARS[@]}; do
    if [[ -z "${!required_env_var}" ]]; then
      echo "Error:
    Environment variable '$required_env_var' not set.
    Make sure you have the following environment variables set:

      ${REQUIRED_ENV_VARS[@]}

Aborting."
      exit 1
    fi
  done
}


# Performs the initialization in the already-started PostgreSQL
# using the preconfigured POSTGRE_USER user.
init_user_and_db() {
  psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" <<-EOSQL
     create user $DB_USER with password '$DB_PASSWORD'; 
     create database $DB_DATABASE;
     \c $DB_DATABASE;
     grant all privileges on database $DB_DATABASE to $DB_USER;
     alter schema public owner to $DB_USER;
     create extension postgres_fdw;
     create extension hstore;
     create extension ltree;
     create extension pg_trgm;
     create extension pgcrypto;
EOSQL
}

# Executes the main routine with environment variables
# passed through the command line. We don't use them in
# this script but now you know 🤓
main "$@"