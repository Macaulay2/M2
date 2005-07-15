#! /bin/sh
# determine the command needed to run configure again

if [ ! -f config.log ]
then echo "file config.log not found" >&2
     exit 1
fi

LIBS=

eval `egrep "^(ac_cv_env.*|LIBS)=" config.log`

CMD=`egrep '^  \\$' config.log | head -1 | sed 's/^  \\$ //'`

if [ "$LIBS" ]
then CMD="LIBS=\"$LIBS\" $CMD"
fi

if [ "$ac_cv_env_LDFLAGS_set" = set ]
then CMD="LDFLAGS=\"$ac_cv_env_LDFLAGS_value\" $CMD"
fi

if [ "$ac_cv_env_CXXFLAGS_set" = set ]
then CMD="CXXFLAGS=\"$ac_cv_env_CXXFLAGS_value\" $CMD"
fi

if [ "$ac_cv_env_CXX_set" = set ]
then CMD="CXX=\"$ac_cv_env_CXX_value\" $CMD"
fi

if [ "$ac_cv_env_CFLAGS_set" = set ]
then CMD="CFLAGS=\"$ac_cv_env_CFLAGS_value\" $CMD"
fi

if [ "$ac_cv_env_CPPFLAGS_set" = set ]
then CMD="CPPFLAGS=\"$ac_cv_env_CPPFLAGS_value\" $CMD"
fi

if [ "$ac_cv_env_CC_set" = set ]
then CMD="CC=\"$ac_cv_env_CC_value\" $CMD"
fi

echo "env $CMD" "$@"
