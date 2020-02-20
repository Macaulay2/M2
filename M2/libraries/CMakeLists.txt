#################################################################################
## tarfile libraries

set(TARFILES
  # My machine download these:
  4ti2      3053e7467b5585ad852f6a56e78e28352653943e7249ad5e5174d4744d174966 4ti2-1.6.9.tar.gz
  cddlib    fe6d04d494683cd451be5f6fe785e147f24e8ce3ef7387f048e739ceb4565ab5 cddlib-094h.tar.gz
  cohomcalg 367c52b99c0b0a4794b215181439bf54abe4998872d3ef25d793bc13c4d40e42 cohomCalg-0.32.tar.gz
  csdp      7f202a15f33483ee205dcfbd0573fdbd74911604bb739a04f8baa35f8a055c5b Csdp-6.2.0.tgz
  factory   9dd84d11204e1457dac0a0d462a78d4cd4103c14cbf792b83d488aa529ad5724 factory-4.1.1.tar.gz 
  flint     cbf1fe0034533c53c5c41761017065f85207a1b770483e98b2392315f6575e87 flint-2.5.2.tar.gz
  frobby    af092383e6dc849c86f4e79747ae0e5cd309a690747230e10aa38d60640062df frobby_v0.9.0.tar.gz
  gc        436a0ddc67b1ac0b0405b61a9675bca9e075c8156f4debd1d06f3a56c7cd289d gc-8.0.4.tar.gz
  gfan      a674d5e5dc43634397de0d55dd5da3c32bd358d05f72b73a50e62c1a1686f10a gfan0.6.2.tar.gz
  gftables  9cd158ceb1c2b1c47bdca2c0b004bba92cb0e0aaa0ea6a43ca784ebdce10eebd factory.4.0.1-gftables.tar.gz
  givaro    19101e41161db46a925a0d055cf530c6d731b0dcc79e69f4358e483778306d16 givaro-4.0.3.tar.gz
  glpk      e398be2e7cb8a98584325268704729872558a4a88555bc8a54139d017eb9ebae glpk-4.59.tar.gz
  gtest     58a6f4277ca2bc8565222b3bbd58a177609e9c488e8a72649359ba51450db7d8 gtest-1.8.0.tar.gz
  lrslib    adf92f9c7e70c001340b9c28f414208d49c581df46b550f56ab9a360348e4f09 lrslib-062.tar.gz
  mpc       6985c538143c1208dcb1ac42cedad6ff52e267b47e5f970183a3e75125b43c2e mpc-1.1.0.tar.gz
  mpfr      1d3be708604eae0e42d578ba93b390c2a145f17743a744d8f3f8c2ad5855a38a mpfr-4.0.2.tar.xz
  mpir      52f63459cf3f9478859de29e00357f004050ead70b45913f2c2269d9708675bb mpir-3.0.0.tar.bz2
  nauty     5d52211cec767d8d8e43483d96202be235f85696d1373c307291273463c812fa nauty27b11.tar.gz
  normaliz  436a870a1ab9a5e0c2330f5900d904dc460938c17428db1c729318dbd9bf27aa normaliz-3.7.2.tar.gz
  ntl       b90b36c9dd8954c9bc54410b1d57c00be956ae1db5a062945822bbd7a86ab4d2 ntl-10.5.0.tar.gz
  pari      4a6532b3c77350363fa618ead5cd794a172d7b7e5757a28f7788e658b5469339 pari-2.11.2.tar.gz
  topcom    3f83b98f51ee859ec321bacabf7b172c25884f14848ab6c628326b987bd8aaab TOPCOM-0.17.8.tar.gz
  # Everything possible:
  # 4ti2 atomic_ops boost cddlib cddplus cohomcalg csdp factory fflas_ffpack flint fplll frobby gc gdbm gfan gftables givaro
  # glpk gmp gtest lapack libtool linbox lrslib mpack mpc mpfr mpir nauty normaliz ntl pari polymake qd readline topcom
  )

# Testing ..
set(M2_SOURCE_URL https://faculty.math.illinois.edu/Macaulay2/Downloads/OtherSourceCode)

include(ExternalProject) # populate at build time; FetchContent populates at configure time

ExternalProject_Add(gftables
  URL               ${M2_SOURCE_URL}/factory.4.0.1-gftables.tar.gz
  URL_HASH          SHA256=9cd158ceb1c2b1c47bdca2c0b004bba92cb0e0aaa0ea6a43ca784ebdce10eebd
  PREFIX            gftables
  SOURCE_DIR        ${CORE_DIR}/factory/gftables # gftables/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ""
  INSTALL_COMMAND   ""
)

#################################################################################
## git submodules section

# bdwgc https://github.com/Macaulay2/bdwgc.git origin/master

# TODO: need to change givaro requirement to 4.0.2
ExternalProject_Add(fflas_ffpack
  GIT_REPOSITORY    https://github.com/Macaulay2/fflas-ffpack.git
  GIT_TAG           712cef0e
  PREFIX            fflas_ffpack
  SOURCE_DIR        fflas_ffpack/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND "" # ./autogen.sh ; ./configure ?
  BUILD_COMMAND     "" # ${MAKE_EXE}
  INSTALL_COMMAND   ""
)

# flint2 https://github.com/Macaulay2/flint2.git ??

# gtest https://github.com/google/googletest.git 42bc671f

# libatomic_ops https://github.com/Macaulay2/libatomic_ops.git ba6c317

ExternalProject_Add(mathic
  GIT_REPOSITORY    https://github.com/Macaulay2/mathic.git
  GIT_TAG           023afcf
  PREFIX            mathic
  SOURCE_DIR        mathic/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND "" # ./autogen.sh ; ./configure ?
  BUILD_COMMAND     "" # ${MAKE_EXE}
  INSTALL_COMMAND   ""
)

ExternalProject_Add(mathicgb
  GIT_REPOSITORY    https://github.com/Macaulay2/mathicgb.git
  GIT_TAG           bd634c8
  PREFIX            mathicgb
  SOURCE_DIR        mathicgb/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND "" # ./autogen.sh ; ./configure ?
  BUILD_COMMAND     "" # ${MAKE_EXE}
  INSTALL_COMMAND   ""
)

ExternalProject_Add(memtailor
  GIT_REPOSITORY    https://github.com/Macaulay2/memtailor.git
  GIT_TAG           e85453b
  PREFIX            memtailor
  SOURCE_DIR        memtailor/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND "" # ./autogen.sh ; ./configure ?
  BUILD_COMMAND     "" # ${MAKE_EXE}
  INSTALL_COMMAND   ""
)

# mpir https://github.com/Macaulay2/mpir.git 82816d99
