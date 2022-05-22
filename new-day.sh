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

MODULE_NAME="${REST// /}"
FILE_NAME=$(echo "${REST// /_}" | tr “[:upper:]” “[:lower:]”)

# echo "year: $YEAR"
# echo "day: $DAY"
# echo "rest: $REST"
# echo "module: $MODULE_NAME"
# echo "file: $FILE_NAME"

TEST_FILE_NAME="day${DAY}_tests.fs"
TEST_FILE="$YEAR/$TEST_FILE_NAME"
CODE_FILE_NAME="day${DAY}_$FILE_NAME.fs"
CODE_FILE="$YEAR/$CODE_FILE_NAME"

PROJ_FILE="$YEAR/$YEAR.fsproj"

echo "creating new files:"
echo "- $CODE_FILE"
echo "- $TEST_FILE"
echo "updating project:"
echo "- $PROJ_FILE"

sed "s/DAY_NUMBER/$DAY/g;s/MODULE_NAME/$MODULE_NAME/g" .templates/tests.fs > "$YEAR/day${DAY}_tests.fs"
sed "s/DAY_NUMBER/$DAY/g;s/MODULE_NAME/$MODULE_NAME/g" .templates/code.fs > "$YEAR/day${DAY}_$FILE_NAME.fs"
sed -i '' "s/\(<!-- NEXT -->\)/<Compile Include=\"$CODE_FILE_NAME\" \/>\n    <Compile Include=\"$TEST_FILE_NAME\" \/>\n    \1/" $PROJ_FILE
