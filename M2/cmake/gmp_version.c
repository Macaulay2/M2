#include <gmp.h>

#define MSG(s) (#s " " STR(s))
#define STR(s) #s

#pragma message MSG(__GNU_MP_VERSION)
#pragma message MSG(__GNU_MP_VERSION_MINOR)
#pragma message MSG(__GNU_MP_VERSION_PATCHLEVEL)

int main(){
    return 0;
};
