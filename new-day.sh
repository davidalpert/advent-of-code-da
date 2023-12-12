#!/bin/bash

YEAR=$1
DAY=$2
shift
shift
REST="$@"

if [ -z "$YEAR" ] || [ -z $DAY ]; then
  echo "usage:"
  echo ""
  echo "  new-day.sh <YYYY> <DD> [name of code module]"
  echo ""
  exit 1
fi

MODULE_NAME="day${DAY}_${REST// /_}"
FILE_NAME=$(echo "${REST// /_}" | tr “[:upper:]” “[:lower:]”)

# echo "year: $YEAR"
# echo "day: $DAY"
# echo "rest: $REST"
# echo "module: $MODULE_NAME"
# echo "file: $FILE_NAME"

INPUT_FILE_NAME="day${DAY}_input.fs"
INPUT_FILE="$YEAR/$INPUT_FILE_NAME"
CODE_FILE_NAME="day${DAY}_$FILE_NAME.fs"
CODE_FILE="$YEAR/$CODE_FILE_NAME"
TEST_FILE_NAME="day${DAY}_tests.fs"
TEST_FILE="$YEAR/$TEST_FILE_NAME"

PROJ_FILE="$YEAR/$YEAR.fsproj"

echo "creating new files:"
echo "- $INPUT_FILE"
echo "- $CODE_FILE"
echo "- $TEST_FILE"
echo "updating project:"
echo "- $PROJ_FILE"

sed "s/YEAR/$YEAR/g;s/DAY_NUMBER/$DAY/g;s/MODULE_NAME/$MODULE_NAME/g" .templates/input.fs > "$YEAR/$INPUT_FILE_NAME"
sed "s/YEAR/$YEAR/g;s/DAY_NUMBER/$DAY/g;s/MODULE_NAME/$MODULE_NAME/g" .templates/code.fs > "$YEAR/$CODE_FILE_NAME"
sed "s/YEAR/$YEAR/g;s/DAY_NUMBER/$DAY/g;s/MODULE_NAME/$MODULE_NAME/g" .templates/tests.fs > "$YEAR/$TEST_FILE_NAME"
sed -i '' "s/\(<!-- NEXT -->\)/<Compile Include=\"$INPUT_FILE_NAME\" \/>\n    <Compile Include=\"$CODE_FILE_NAME\" \/>\n    <Compile Include=\"$TEST_FILE_NAME\" \/>\n    \1/" $PROJ_FILE
