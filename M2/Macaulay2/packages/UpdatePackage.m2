
newPackage(
    "UpdatePackage",
    Version => "1.0", 
    Date => "February 3, 2021",
    Authors => {{Name => "Giovanni Staglianò",Email => "giovannistagliano@gmail.com"}},
    Headline => "install the latest available version of packages",
    Keywords => {"System"}
)

export {"updatePackage", "Branch"}

isCurlPresent = try get "!curl -h 2>&1" then true else false;

IsAlreadyUpdated := local IsAlreadyUpdated;

updateSinglePackage = method(Options => {Branch => "master"});
updateSinglePackage (String,List) := o -> (packagename,listauxfiles) -> (
    if o.Branch =!= "master" and o.Branch =!= "pre-master" and o.Branch =!= "development" then error "the possible branches are \"master\", \"pre-master\", and \"development\"";
    if IsAlreadyUpdated_(packagename|o.Branch) === true then return;
    IsAlreadyUpdated_(packagename|o.Branch) = true;
    <<"-- checking updates for package "<<packagename<<" from "<<o.Branch<<" branch..."<<endl;
    if not isCurlPresent then error "please install CURL on your system";
    dir := temporaryFileName() | "/";
    mkdir dir;
    run("curl -s -o "|dir|packagename|".m2 https://raw.githubusercontent.com/Macaulay2/M2/"|o.Branch|"/M2/Macaulay2/packages/"|packagename|".m2");
    if not fileExists(dir|packagename|".m2") then error("something went wrong in downloading the package "|packagename);
    if #listauxfiles > 0 then (
        makeDirectory(dir|packagename);
        for f in listauxfiles do (
            run("curl -s -o "|dir|packagename|"/"|f|" https://raw.githubusercontent.com/Macaulay2/M2/"|o.Branch|"/M2/Macaulay2/packages/"|packagename|"/"|f);
            if not fileExists(dir|packagename|"/"|f) then error("something went wrong in downloading the package "|packagename);
        );
    );
    get("!"|"cd "|dir|/// && M2 --no-preload -e "loadPackage(\"///|packagename|///\",FileName=>\"///|dir|packagename|".m2"|///\"); \"///|dir|///packageVersion.m2\"<<///|packagename|///.Options.Version<<close;" 2>&1 &///);
    v0 := get(dir|"packageVersion.m2");
    removeFile(dir|"packageVersion.m2");
    if (value packagename).Options.Version == v0 then (<<"-- package "<<packagename<<" is already updated to version "<<v0<<" -- nothing to do."<<endl; return);
    e := "";
    while not(e == "y" or e == "yes" or e == "Y" or e == "Yes") do (
        e = read("Your version of the package "|packagename|" is outdated. Do you want to install the latest version of "|packagename|" now? (y/n) ");
        if e == "n" or e == "no" or e == "N" or e == "No" then return;
    );
    <<"-- installing package "<<packagename<<", version "<<v0<<"... (this may take a while)"<<endl;
    get("!"|"cd "|dir|/// && M2 --no-preload -e "uninstallPackage \"///|packagename|///\"" 2>&1 &///);
    get("!"|"cd "|dir|/// && M2 --no-preload -e "installPackage(\"///|packagename|///\",FileName=>\"///|dir|packagename|".m2"|///\")" 2>&1 &///);
    removeFile(dir|packagename|".m2"); 
    if #listauxfiles > 0 then (
        for f in listauxfiles do removeFile(dir|packagename|"/"|f); 
        removeDirectory(dir|packagename);  
    ); 
    <<"--** package "<<packagename<<", version "<<v0<<", has been successfully installed **--"<<endl;
    get("!"|///M2 --no-preload -e "loadPackage \"///|packagename|///\"; \"///|dir|///packageVersion.m2\"<<///|packagename|///.Options.Version<<close;" 2>&1 &///);
    v := get(dir|"packageVersion.m2");
    removeFile(dir|"packageVersion.m2");
    if v != v0 then (
        sf := (value packagename)#"source file";
        err := "Error in loading the new version of the package "|packagename|". Check that in your path you do not have any older version of the package.";
        i0 := 0; while fileExists(sf|"-"|toString(i0)) do i0 = i0+1;
        try moveFile(sf,sf|"-"|toString(i0)) then (<<"--warning: the file "<<sf<<" has been renamed to "<<packagename|".m2-"|toString(i0)|" (you can also delete it)"<<endl) else error err;
        get("!"|///M2 --no-preload -e "loadPackage \"///|packagename|///\"; \"///|dir|///packageVersion.m2\"<<///|packagename|///.Options.Version<<close;" 2>&1 &///);
        v = get(dir|"packageVersion.m2");
        removeFile(dir|"packageVersion.m2");
        if v != v0 then error err;   
    );    
    return v;
);
updateSinglePackage String := o -> packagename -> updateSinglePackage(packagename,{},Branch=>o.Branch);

auxFiles = packagename -> (
    if not (value packagename).Options.AuxiliaryFiles then return {};
    f := lines get("!ls "|(value packagename)#"auxiliary files");
    if any(f,isDirectory) then error("this function is not yet available for the package "|packagename);
    return f;
);

updatePackage = method(Options => {Branch => "master"});
updatePackage String := o -> packagename -> (
    needsPackage packagename;
    ImportsExports := sort unique((value packagename).Options.PackageImports | (value packagename).Options.PackageExports);
    dismiss packagename;
    UpdateDone := false;
    v := null;
    for P in ImportsExports do (
        try v = updateSinglePackage(P,auxFiles P,Branch=>o.Branch) 
        then (if v =!= null then UpdateDone = true) 
        else <<"--warning: this function is not yet available for the dependency on package "|packagename<<endl
    );
    v = updateSinglePackage(packagename,auxFiles packagename,Branch=>o.Branch);
    if (not UpdateDone) and v === null then return;
    <<endl<<"--** A restart of Macaulay2 is required **--"<<endl;
);

beginDocumentation() 
document {Key => {"UpdatePackage"}, 
Headline => "install the latest available version of packages",
PARA{"This package provides the single function ",TO updatePackage,", which automates the process of downloading and installing a single package from the ",HREF{"https://github.com/Macaulay2/M2/tree/master/M2/Macaulay2/packages","master branch"}," of ",EM"Macaulay2",". So that you don't have to wait for the next ",EM"Macaulay2"," release to use the latest version of your preferred package."}}
document {Key => {updatePackage,(updatePackage,String),[updatePackage,Branch]},
Headline => "update a package to the version included in the master branch of Macaulay2",
Usage => "updatePackage P",
Inputs => {"P" => String => {"the name of a package provided with ",EM"Macaulay2"}},
Consequences => {{"The package ",TT"P"," will be updated if it is out of date with respect to the version in the ",HREF{"https://github.com/Macaulay2/M2/tree/master/M2/Macaulay2/packages","master branch"}," of ",EM"Macaulay2"}},
PARA{"Most of the dependent packages will also be checked for updates and updated if necessary. If an update is available, the user will be asked for confirmation before installing it."},
if isCurlPresent then EXAMPLE {"updatePackage \"Cremona\"","updatePackage \"SparseResultants\""}
else PRE ///i1 : updatePackage "Cremona"
-- checking updates for package Cremona from master branch...
-- package Cremona is already updated to version 5.1 -- nothing to do.

i2 : updatePackage "SparseResultants"
-- checking updates for package Resultants from master branch...
-- package Resultants is already updated to version 1.2.2 -- nothing to do.
-- checking updates for package SparseResultants from master branch...
-- package SparseResultants is already updated to version 1.1 -- nothing to do.
///}
undocumented{"Branch"}

TEST ///
debug UpdatePackage
assert(auxFiles "Cremona" === {"documentation.m2","examples.m2","tests.m2"})
assert(auxFiles "CoincidentRootLoci" === {"documentation.m2","equationsCRL.m2","equationsDualCRL.m2","tests.m2"})
assert(auxFiles "SparseResultants" === {})
///

TEST ///
debug UpdatePackage
if isCurlPresent then updatePackage "Cremona"
if isCurlPresent then updatePackage "SparseResultants"
if isCurlPresent then updatePackage "SpecialFanoFourfolds"
///

