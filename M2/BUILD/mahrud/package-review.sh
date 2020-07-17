user=mahrud
token=`cat ~/.github_token` # https://github.com/settings/tokens

# List workflows:
#curl -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/Macaulay2/M2/actions/workflows

if [[ -z "$1" ]];
then package="FirstPackage"
else package=$1
fi

shift

if [[ -z "$1" ]];
then ref="refs/master"
else ref=$1
fi

echo package: $package
echo     ref: $ref

curl \
  -u $user:$token -X POST \
  -H "Accept: application/vnd.github.v3+json" \
  -d "{\"inputs\":{\"package\":\"$package\"}, \"ref\":\"$ref\"}" \
  https://api.github.com/repos/Macaulay2/M2/actions/workflows/1904671/dispatches
